open Core

module Point = struct
  module T = struct
    type t = int * int [@@deriving compare, sexp]
  end

  include T
  include Comparable.Make (T)

  let move ~direction (row, col) =
    match direction with
    | `up -> (row - 1, col)
    | `right -> (row, col + 1)
    | `down -> (row + 1, col)
    | `left -> (row, col - 1)

  let adjacents point =
    List.map
      [ `up; `right; `down; `left ]
      ~f:(fun direction -> move point ~direction)
end

let parse_input input =
  let input_rows = String.split_lines input in
  let blizzards =
    List.foldi input_rows ~init:[] ~f:(fun input_row_index blizzards ->
        String.foldi ~init:blizzards ~f:(fun input_col_index blizzards tile ->
            let add direction =
              ((input_row_index - 1, input_col_index - 1), direction)
              :: blizzards
            in
            match tile with
            | '#' | '.' -> blizzards
            | '^' -> add `up
            | '>' -> add `right
            | 'v' -> add `down
            | '<' -> add `left
            | _ -> failwith "invalid character"))
  in
  ( blizzards,
    List.length input_rows - 2,
    String.length (List.hd_exn input_rows) - 2 )

let test_input = In_channel.read_all "day_24_input_test.txt" |> parse_input
let input = In_channel.read_all "day_24_input.txt" |> parse_input

let simulate_from ~start_point ~end_point ~blizzards ~height ~width =
  let move_blizzard (position, direction) =
    let row, col = Point.move position ~direction in
    ((row % height, col % width), direction)
  in
  let legal_position ~blizzard_positions position =
    let row, col = position in
    Point.equal position (-1, 0)
    || Point.equal position (height, width - 1)
    || 0 <= row && row < height && 0 <= col && col < width
       && not (Set.mem blizzard_positions position)
  in

  let rec helper ~minute ~positions ~blizzards =
    let blizzards = List.map blizzards ~f:move_blizzard in
    let blizzard_positions =
      Point.Set.of_list (List.map blizzards ~f:(fun (position, _) -> position))
    in
    let positions =
      Set.to_list positions
      |> List.map ~f:(fun position -> position :: Point.adjacents position)
      |> List.join |> Point.Set.of_list
      |> Set.filter ~f:(legal_position ~blizzard_positions)
    in
    match Set.mem positions end_point with
    | true -> (minute, blizzards)
    | false -> helper ~minute:(minute + 1) ~positions ~blizzards
  in
  helper ~minute:1 ~positions:(Point.Set.singleton start_point) ~blizzards

let part_a (blizzards, height, width) =
  let start_point, end_point = ((-1, 0), (height, width - 1)) in
  let minutes, _ =
    simulate_from ~start_point ~end_point ~blizzards ~height ~width
  in
  minutes

let%expect_test _ =
  test_input |> part_a |> printf "%d\n";
  [%expect "18"]

let%expect_test _ =
  input |> part_a |> printf "%d\n";
  [%expect "301"]

let part_b (blizzards, height, width) =
  let start_point, end_point = ((-1, 0), (height, width - 1)) in
  let ( *> ) (blizzards, time_elapsed, start_point) end_point =
    let time_in_this_step, new_blizzards =
      simulate_from ~start_point ~end_point ~blizzards ~height ~width
    in
    (new_blizzards, time_elapsed + time_in_this_step, end_point)
  in
  let _, minutes, _ =
    (blizzards, 0, start_point) *> end_point *> start_point *> end_point
  in
  minutes

let%expect_test _ =
  test_input |> part_b |> printf "%d\n";
  [%expect "54"]

let%expect_test _ =
  input |> part_b |> printf "%d\n";
  [%expect "54"]