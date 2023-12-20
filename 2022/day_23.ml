open Core

module Point = struct
  module T = struct
    type t = int * int [@@deriving compare, sexp]
  end

  include T
  include Comparable.Make (T)
end

let bounding_rectangle =
  Set.fold
    ~init:((Int.max_value, Int.max_value), (Int.min_value, Int.min_value))
    ~f:(fun ((ul_row, ul_col), (lr_row, lr_col)) (row, col) ->
      Int.((min ul_row row, min ul_col col), (max lr_row row, max lr_col col)))

let points_to_string points =
  let (ul_row, ul_col), (lr_row, lr_col) = bounding_rectangle points in
  let grid =
    List.init
      (lr_row - ul_row + 1)
      ~f:(fun row ->
        List.init
          (lr_col - ul_col + 1)
          ~f:(fun col ->
            match Set.mem points (row + ul_row, col + ul_col) with
            | true -> '#'
            | false -> '.'))
  in
  List.intersperse grid ~sep:[ '\n' ] |> List.join |> String.of_char_list

let simulate_one_step ~proposal_offset elves =
  let elf_proposals =
    Point.Map.of_key_set elves ~f:(fun (row, col) ->
        let neighbors, possible_proposals =
          let n, e, s, w =
            ((row - 1, col), (row, col + 1), (row + 1, col), (row, col - 1))
          in
          let nw, ne, se, sw =
            ( (row - 1, col - 1),
              (row - 1, col + 1),
              (row + 1, col + 1),
              (row + 1, col - 1) )
          in
          let ordered_proposals =
            [
              (n, [ ne; nw ]); (s, [ se; sw ]); (w, [ nw; sw ]); (e, [ ne; se ]);
            ]
          in
          let first, second = List.split_n ordered_proposals proposal_offset in
          ([ n; e; s; w; nw; ne; se; sw ], second @ first)
        in
        let position_free = Fn.non (Set.mem elves) in
        match List.for_all neighbors ~f:position_free with
        | true -> None
        | false ->
            List.find_map possible_proposals
              ~f:(fun (position, positions_to_check) ->
                match
                  List.exists (position :: positions_to_check)
                    ~f:(fun position -> Set.mem elves position)
                with
                | true -> None
                | false -> Some position))
  in
  let proposed_destination_counts =
    Map.to_alist elf_proposals
    |> List.filter_map ~f:(fun (_, proposal) -> proposal)
    |> List.fold ~init:Point.Map.empty ~f:(fun counts position ->
           Map.update counts position
             ~f:(Option.value_map ~default:1 ~f:(( + ) 1)))
  in
  let elves =
    Point.Set.map elves ~f:(fun elf ->
        match Map.find_exn elf_proposals elf with
        | Some position
          when Map.find_exn proposed_destination_counts position = 1 ->
            position
        | _ -> elf)
  in
  elves

let part_a input =
  let _, positions =
    Fn.apply_n_times ~n:10
      (fun (round, elves) ->
        (round + 1, simulate_one_step ~proposal_offset:(round % 4) elves))
      (0, input)
  in
  let (ul_row, ul_col), (lr_row, lr_col) = bounding_rectangle positions in
  ((lr_row - ul_row + 1) * (lr_col - ul_col + 1)) - Set.length positions

let parse_input input =
  input |> String.split_lines
  |> List.foldi ~init:Point.Set.empty ~f:(fun row_index elves row ->
         let row_elves =
           String.foldi row ~init:Point.Set.empty
             ~f:(fun col_index elves tile ->
               match tile with
               | '.' -> elves
               | '#' -> Set.add elves (row_index, col_index)
               | _ -> failwith "malformed input")
         in
         Set.union elves row_elves)

let test_input = In_channel.read_all "day_23_input_test.txt" |> parse_input
let input = In_channel.read_all "day_23_input.txt" |> parse_input

let%expect_test _ =
  test_input |> part_a |> printf "%d\n";
  [%expect "110"]

let%expect_test _ =
  input |> part_a |> printf "%d\n";
  [%expect "3990"]

let part_b input =
  let rec helper ~round ~elves =
    let updated_elves =
      simulate_one_step ~proposal_offset:((round - 1) % 4) elves
    in
    match Set.equal elves updated_elves with
    | true -> round
    | false -> helper ~round:(round + 1) ~elves:updated_elves
  in
  helper ~round:1 ~elves:input

let%expect_test _ =
  test_input |> part_b |> printf "%d\n";
  [%expect "20"]

let%expect_test _ =
  input |> part_b |> printf "%d\n";
  [%expect "1013"]
