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

  let add t rock = Set.union t (Point.Set.of_list rock)
end

let drop_rocks ~num_rocks wind =
  let wind =
    String.to_array wind
    |> Array.map ~f:(function
         | '<' -> -1
         | '>' -> 1
         | _ -> failwith "bad input")
  in
  let drop_rock ~rock_chamber ~rock_idx ~wind_idx =
    let rock =
      Point.offset_rock
        (List.nth_exn rocks rock_idx)
        ~cols:2
        ~rows:(Rock_chamber.height rock_chamber + 3)
    in
    let rec simulate rock wind_idx =
      let current_wind = Array.get wind (wind_idx % Array.length wind) in
      let pushed_rock =
        let pushed_rock = Point.offset_rock rock ~rows:0 ~cols:current_wind in
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
    (Rock_chamber.add rock_chamber rock, wind_idx % Array.length wind)
  in
  let rec drop_rocks ~num_rocks ~wind_idx ~rock_idx ~rock_chamber =
    match num_rocks with
    | 0 -> (wind_idx, rock_idx, rock_chamber)
    | num_rocks ->
        let rock_chamber, wind_idx =
          drop_rock ~rock_chamber ~rock_idx ~wind_idx
        in
        drop_rocks ~num_rocks:(num_rocks - 1) ~wind_idx
          ~rock_idx:((rock_idx + 1) % List.length rocks)
          ~rock_chamber
  in
  let _, _, end_chamber =
    drop_rocks ~num_rocks ~wind_idx:0 ~rock_idx:0
      ~rock_chamber:Rock_chamber.empty
  in
  Rock_chamber.height end_chamber

let test_input = In_channel.read_all "day_17_input_test.txt"
let input = In_channel.read_all "day_17_input.txt"

let () =
  Command.basic ~summary:"advent of code"
    (Command.Param.return (fun () ->
         "day_17_input.txt" |> In_channel.read_all |> drop_rocks ~num_rocks:2022
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
