open Core

let ( >> ) f g x = g (f x)

module Point = struct
  module T = struct
    type t = { x : int; y : int } [@@deriving compare, sexp]
  end

  include T
  include Comparable.Make (T)
end

let parse =
  String.split_lines
  >> List.map
       ~f:
         (String.substr_replace_all ~pattern:" -> " ~with_:";"
         >> String.split ~on:';'
         >> List.map ~f:(fun pair ->
                let x, y = String.lsplit2_exn ~on:',' pair in
                { Point.x = Int.of_string x; y = Int.of_string y }))

let rock_points =
  let points_in_segment { Point.x = start_x; y = start_y }
      { Point.x = end_x; y = end_y } =
    let list =
      match start_x = end_x with
      | true ->
          List.init
            (abs (start_y - end_y) + 1)
            ~f:(fun offset ->
              { Point.x = start_x; y = min start_y end_y + offset })
      | false ->
          List.init
            (abs (start_x - end_x) + 1)
            ~f:(fun offset ->
              { Point.x = min start_x end_x + offset; y = start_y })
    in
    Point.Set.of_list list
  in
  let points_in_path =
    let map_consecutive_pairs list =
      List.map2_exn (List.drop_last_exn list) (List.tl_exn list)
    in
    map_consecutive_pairs ~f:points_in_segment >> Point.Set.union_list
  in
  List.map ~f:points_in_path >> Point.Set.union_list

let y_max = Point.Set.fold ~init:0 ~f:(fun max { y; _ } -> Int.max y max)

let grains_until_end ~rock_points ~y_max ~y_max_is_floor =
  let grain_source = { Point.x = 500; y = 0 } in
  let rec helper ~grains_dropped ~filled_points =
    let rec simulate_grain { Point.x; y } =
      match y = y_max with
      | true -> `Abyss
      | false -> (
          let possibilities =
            [
              { Point.x; y = y + 1 };
              { x = x - 1; y = y + 1 };
              { x = x + 1; y = y + 1 };
            ]
          in
          let point_empty { Point.x; y } =
            (not (Set.mem filled_points { Point.x; y }))
            && not (y_max_is_floor && y = y_max)
          in
          match List.find ~f:point_empty possibilities with
          | Some point -> simulate_grain point
          | None -> `Rest { Point.x; y })
    in
    match simulate_grain grain_source with
    | `Abyss -> `Abyss_after grains_dropped
    | `Rest resting_point when Point.equal grain_source resting_point ->
        `Source_filled_after (grains_dropped + 1)
    | `Rest resting_point ->
        let filled_points = Set.add filled_points resting_point in
        helper ~grains_dropped:(grains_dropped + 1) ~filled_points
  in
  helper ~grains_dropped:0 ~filled_points:rock_points

let part_a rock_paths =
  let rock_points = rock_points rock_paths in
  match
    grains_until_end ~rock_points ~y_max:(y_max rock_points)
      ~y_max_is_floor:false
  with
  | `Abyss_after grains -> grains
  | `Source_filled_after _ -> failwith "source should never be filled in part a"

let%expect_test _ =
  In_channel.read_all "day_14_input_test.txt"
  |> parse |> part_a |> printf "%d\n";
  [%expect "24"]

let%expect_test _ =
  In_channel.read_all "day_14_input.txt" |> parse |> part_a |> printf "%d\n";
  [%expect "719"]

let part_b rock_paths =
  let rock_points = rock_points rock_paths in
  let y_floor = y_max rock_points + 2 in
  match grains_until_end ~rock_points ~y_max:y_floor ~y_max_is_floor:true with
  | `Abyss_after _ -> failwith "grains should never reach abyss in part b"
  | `Source_filled_after grains -> grains

let%expect_test _ =
  In_channel.read_all "day_14_input_test.txt"
  |> parse |> part_b |> printf "%d\n";
  [%expect "93"]

let%expect_test _ =
  In_channel.read_all "day_14_input.txt" |> parse |> part_b |> printf "%d\n";
  [%expect "23390"]