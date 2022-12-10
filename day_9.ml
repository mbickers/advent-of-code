open Core

let ( >> ) f g x = g (f x)

module Point = struct
  module T = struct
    type t = int * int [@@deriving compare, sexp]
  end

  include T
  include Comparable.Make (T)
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

let update_knot ~updated_predecessor:(px, py) ~current:(cx, cy) =
  match (abs (px - cx), abs (py - cy)) with
  | 0, 0 | 1, 0 | 0, 1 | 1, 1 -> (cx, cy)
  | 0, 2 -> (px, (py + cy) / 2)
  | 2, 0 -> ((px + cx) / 2, py)
  | _ ->
      let diagonal_moves =
        [
          (cx + 1, cy + 1); (cx - 1, cy + 1); (cx + 1, cy - 1); (cx - 1, cy - 1);
        ]
      in
      let squared_distance (x1, y1) (x2, y2) =
        ((x1 - x2) * (x1 - x2)) + ((y1 - y2) * (y1 - y2))
      in
      List.min_elt
        ~compare:(fun p1 p2 ->
          squared_distance (px, py) p1 - squared_distance (px, py) p2)
        diagonal_moves
      |> Option.value_exn

let tail_positions ~length moves =
  let process_step (rope_positions, tail_positions) ~direction =
    match rope_positions with
    | [] -> failwith "cannot move empty rope"
    | (hx, hy) :: rest ->
        let updated_head =
          match direction with
          | `Right -> (hx + 1, hy)
          | `Left -> (hx - 1, hy)
          | `Up -> (hx, hy + 1)
          | `Down -> (hx, hy - 1)
        in
        let updated_tail, updated_rest =
          List.fold_map rest ~init:updated_head
            ~f:(fun updated_predecessor current ->
              let updated = update_knot ~updated_predecessor ~current in
              (updated, updated))
        in
        (updated_head :: updated_rest, Set.add tail_positions updated_tail)
  in
  let _, tail_positions =
    let initial = (0, 0) in
    List.fold moves
      ~init:(List.init length ~f:(fun _ -> initial), Point.Set.singleton initial)
      ~f:(fun current_state (direction, magnitude) ->
        Fn.apply_n_times ~n:magnitude (process_step ~direction) current_state)
  in
  tail_positions

let part_a lines =
  List.map ~f:parse lines |> tail_positions ~length:2 |> Set.length

let part_b lines =
  List.map ~f:parse lines |> tail_positions ~length:10 |> Set.length

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

let%expect_test _ =
  part_b (In_channel.read_lines "day_9_input.txt") |> printf "%d\n";
  [%expect "2482"]
