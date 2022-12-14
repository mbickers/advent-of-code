open Core

module Point = struct
  module T = struct
    type t = { x : int; y : int } [@@deriving compare, sexp]
  end

  include T
  include Comparable.Make (T)
end

let parse input =
  let parse_line line =
    line
    |> String.substr_replace_all ~pattern:" -> " ~with_:";"
    |> String.split ~on:';'
    |> List.map ~f:(fun pair ->
           let x, y = String.lsplit2_exn ~on:',' pair in
           { Point.x = Int.of_string x; y = Int.of_string y })
  in
  input |> String.split_lines |> List.map ~f:parse_line

let part_a rock_paths =
  let rock_points =
    let points_in_segment { Point.x = start_x; y = start_y }
        { Point.x = end_x; y = end_y } =
      Point.Set.of_list
      @@
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
    List.fold rock_paths ~init:Point.Set.empty ~f:(fun points rock_path ->
        let points_in_path =
          List.map2_exn
            (List.drop_last_exn rock_path)
            (List.tl_exn rock_path) ~f:points_in_segment
          |> Point.Set.union_list
        in
        Set.union points points_in_path)
  in
  let y_max =
    Point.Set.fold rock_points ~init:0 ~f:(fun max { y; _ } -> Int.max y max)
  in
  let rec grains_until_abyss grains_dropped filled_points =
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
          match
            List.find
              ~f:(fun point -> not (Set.mem filled_points point))
              possibilities
          with
          | Some point -> simulate_grain point
          | None -> `Rest { Point.x; y })
    in
    match simulate_grain { Point.x = 500; y = 0 } with
    | `Abyss -> grains_dropped
    | `Rest resting_point ->
        grains_until_abyss (grains_dropped + 1)
          (Set.add filled_points resting_point)
  in
  grains_until_abyss 0 rock_points

let%expect_test _ =
  In_channel.read_all "day_14_input_test.txt"
  |> parse |> part_a |> printf "%d\n";
  [%expect "24"]

let%expect_test _ =
  In_channel.read_all "day_14_input.txt" |> parse |> part_a |> printf "%d\n";
  [%expect "719"]
