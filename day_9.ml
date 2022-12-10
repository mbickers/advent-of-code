open Core

let ( >> ) f g x = g (f x)

module Point = struct
  module T = struct
    type t = int * int [@@deriving compare, sexp]
  end

  include T
  include Comparable.Make (T)

  let move (x, y) direction =
    match direction with
    | `Right -> (x + 1, y)
    | `Left -> (x - 1, y)
    | `Up -> (x, y + 1)
    | `Down -> (x, y - 1)

  let distance_squared (x1, y1) (x2, y2) =
    ((x1 - x2) * (x1 - x2)) + ((y1 - y2) * (y1 - y2))
end

let parse line =
  match String.split ~on:' ' line with
  | [ direction; magnitude ] ->
      let direction =
        match direction with
        | "R" -> `Right
        | "L" -> `Left
        | "U" -> `Up
        | "D" -> `Down
        | _ -> failwith "invalid direction"
      in
      (direction, Int.of_string magnitude)
  | _ -> failwith "unable to parse input line"

let part_a lines =
  let _, _, positions_visited =
    let initial = (0, 0) in
    List.fold lines
      ~init:(initial, initial, Point.Set.singleton initial)
      ~f:(fun current_state line ->
        let direction, magnitude = parse line in
        let process_step (current_head, current_tail, visited_positions)
            ~direction =
          let new_head = Point.move current_head direction in
          let new_tail =
            match Point.distance_squared new_head current_tail > 2 with
            | true -> current_head
            | false -> current_tail
          in
          (new_head, new_tail, Set.add visited_positions new_tail)
        in
        Fn.apply_n_times ~n:magnitude (process_step ~direction) current_state)
  in
  Set.length positions_visited

let%expect_test _ =
  let test_input = {|R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2|} in
  part_a (String.split_lines test_input) |> printf "%d\n";
  [%expect "13"]

let%expect_test _ =
  part_a (In_channel.read_lines "day_9_input.txt") |> printf "%d\n";
  [%expect "6311"]

let part_b lines =
  let _, positions_visited =
    let length = 10 in
    let initial = (0, 0) in
    List.fold lines
      ~init:(List.init length ~f:(fun _ -> initial), Point.Set.singleton initial)
      ~f:(fun current_state line ->
        let direction, magnitude = parse line in
        let process_step (old_positions, visited_positions) ~direction =
          let old_head, old_rest =
            (List.hd_exn old_positions, List.tl_exn old_positions)
          in
          let new_head = Point.move old_head direction in
          let new_rest =
            List.folding_map old_rest ~init:(new_head, old_head)
              ~f:(fun (new_pred, old_pred) old_current_knot ->
                let new_current_knot =
                  match
                    Point.distance_squared new_pred old_current_knot > 2
                  with
                  | true -> old_pred
                  | false -> old_current_knot
                in
                ((new_current_knot, old_current_knot), new_current_knot))
          in
          ( new_head :: new_rest,
            Set.add visited_positions (List.last_exn new_rest) )
        in
        Fn.apply_n_times ~n:magnitude (process_step ~direction) current_state)
  in
  Set.length positions_visited

let%expect_test _ =
  let test_input = {|R 5
U 8
L 8
D 3
R 17
D 10
L 25
U 20|} in
  let result = part_b (String.split_lines test_input) in
  printf "%d\n" result;
  [%expect "36"]