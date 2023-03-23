open Core

module Point = struct
  module T = struct
    type t = { row : int; col : int } [@@deriving compare, sexp]
  end

  include T

  let offset ~rows ~cols { row; col } = { row = row + rows; col = col + cols }
  let offset_rock ~rows ~cols = List.map ~f:(offset ~rows ~cols)

  include Comparable.Make (T)
end

let rocks =
  let rock_to_points rock =
    String.split_lines rock
    |> List.rev_map ~f:String.to_list
    |> List.mapi ~f:(fun row ->
           List.filter_mapi ~f:(fun col character ->
               match character with '#' -> Some { Point.row; col } | _ -> None))
    |> List.join
  in
  [ "####"; ".#.\n###\n.#."; "..#\n..#\n###"; "#\n#\n#\n#"; "##\n##" ]
  |> List.map ~f:rock_to_points

module Rock_chamber = struct
  type t = Point.Set.t

  let empty = Point.Set.empty

  let height t =
    match Set.max_elt t with Some { Point.row; _ } -> row + 1 | None -> 0

  let would_collide t rock =
    List.exists rock ~f:(fun { Point.row; col } ->
        col < 0 || col > 6 || row < 0 || Set.mem t { Point.row; col })

  let row_full ~row t =
    let row_points =
      Set.to_sequence ~greater_or_equal_to:{ Point.row; col = 0 }
        ~less_or_equal_to:{ Point.row; col = 6 } t
    in
    Sequence.length row_points = 7

  let add t rock =
    let added = Set.union t (Point.Set.of_list rock) in
    (match
       List.exists rock ~f:(fun { Point.row; _ } -> row_full added ~row)
     with
    | true -> printf "added rock that completes row\n%!"
    | false -> ());
    added
end

let drop_rocks ~num_rocks wind =
  printf "Wind length: %d\n" (String.length wind);
  let rocks = Sequence.(take (cycle_list_exn rocks) num_rocks) in
  let wind =
    String.to_list wind
    |> List.map ~f:(function '<' -> -1 | '>' -> 1 | _ -> failwith "bad input")
  in
  let end_chamber, _ =
    Sequence.foldi rocks ~init:(Rock_chamber.empty, 0)
      ~f:(fun rock_idx (rock_chamber, wind_idx) rock ->
        printf "%d, %d\n%!" (rock_idx % 5) (wind_idx % List.length wind);
        let rock =
          Point.offset_rock rock ~cols:2
            ~rows:(Rock_chamber.height rock_chamber + 3)
        in
        let rec simulate rock wind_idx =
          let current_wind = List.nth_exn wind (wind_idx % List.length wind) in
          let pushed_rock =
            let pushed_rock =
              Point.offset_rock rock ~rows:0 ~cols:current_wind
            in
            match Rock_chamber.would_collide rock_chamber pushed_rock with
            | true -> rock
            | false -> pushed_rock
          in
          let dropped_rock = Point.offset_rock ~rows:(-1) ~cols:0 pushed_rock in
          match Rock_chamber.would_collide rock_chamber dropped_rock with
          | true -> (pushed_rock, wind_idx + 1)
          | false -> simulate dropped_rock (wind_idx + 1)
        in
        let rock, wind_idx = simulate rock wind_idx in
        (Rock_chamber.add rock_chamber rock, wind_idx))
  in
  Rock_chamber.height end_chamber

let test_input = In_channel.read_all "day_17_input_test.txt"
let input = In_channel.read_all "day_17_input.txt"

let () =
  Command.basic ~summary:"advent of code"
    (Command.Param.return (fun () ->
         "day_17_input.txt" |> In_channel.read_all
         |> drop_rocks ~num_rocks:1000000000000
         |> printf "%d\n"))
  |> Command.run

(*
   let%expect_test _ =
     drop_rocks ~num_rocks:2022 test_input |> printf "%d\n";
     [%expect "3068"]

   let%expect_test _ =
     drop_rocks ~num_rocks:2022 input |> printf "%d\n";
     [%expect "3055"]

   let%expect_test _ =
     drop_rocks ~num_rocks:1000000000000 test_input |> printf "%d\n";
     [%expect "1514285714288"]

   let%expect_test _ =
     drop_rocks ~num_rocks:1000000000000 input |> printf "%d\n";
     [%expect "3055"] *)
