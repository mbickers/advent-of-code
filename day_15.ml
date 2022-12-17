open Core

let ( >> ) f g x = g (f x)

module Point = struct
  module T = struct
    type t = { x : int; y : int } [@@deriving compare, sexp]
  end

  include T
  include Comparable.Make (T)
end

let parse_input =
  let parse_line line =
    Scanf.sscanf line "Sensor at x=%d, y=%d: closest beacon is at x=%d, y=%d"
      (fun sensor_x sensor_y beacon_x beacon_y ->
        ( { Point.x = sensor_x; y = sensor_y },
          { Point.x = beacon_x; y = beacon_y } ))
  in
  String.split_lines >> List.map ~f:parse_line

let scanned_ranges ~y input =
  let range_scanned
      ( { Point.x = sensor_x; y = sensor_y },
        { Point.x = beacon_x; y = beacon_y } ) =
    let sensor_distance =
      abs (sensor_x - beacon_x) + abs (sensor_y - beacon_y)
    in
    let distance_to_row = abs (sensor_y - y) in
    let max_offset = sensor_distance - distance_to_row in
    match max_offset >= 0 with
    | true -> Some (sensor_x - max_offset, sensor_x + max_offset)
    | false -> None
  in
  let scanned_ranges = List.filter_map input ~f:range_scanned in
  let merged_scanned_ranges =
    let sorted_scanned_ranges =
      List.sort scanned_ranges ~compare:(fun (start1, _) (start2, _) ->
          Int.compare start1 start2)
    in
    let rev_merged_ranges, last_range =
      List.fold sorted_scanned_ranges ~init:([], None)
        ~f:(fun (merged, running_range) (current_start, current_end) ->
          match running_range with
          | None -> (merged, Some (current_start, current_end))
          | Some (running_start, running_end) -> (
              match running_end + 1 >= current_start with
              | true ->
                  (merged, Some (running_start, max running_end current_end))
              | false ->
                  ( (running_start, running_end) :: merged,
                    Some (current_start, current_end) )))
    in
    match last_range with
    | Some last_range -> List.rev (last_range :: rev_merged_ranges)
    | None -> List.rev rev_merged_ranges
  in
  merged_scanned_ranges

let part_a ~y input =
  let beacons_x =
    List.filter_map input ~f:(fun (_, { Point.x = beacon_x; y = beacon_y }) ->
        Option.some_if (y = beacon_y) beacon_x)
    |> Int.Set.of_list
  in
  List.sum
    (module Int)
    (scanned_ranges ~y input)
    ~f:(fun (start, end_) ->
      let intersecting_beacons =
        Set.count beacons_x ~f:(fun x -> start <= x && x <= end_)
      in
      end_ - start + 1 - intersecting_beacons)

let%expect_test _ =
  In_channel.read_all "day_15_input_test.txt"
  |> parse_input |> part_a ~y:10 |> printf "%d\n";
  [%expect "26"]

let%expect_test _ =
  In_channel.read_all "day_15_input.txt"
  |> parse_input |> part_a ~y:2000000 |> printf "%d\n";
  [%expect "5144286"]

let part_b ~bound input =
  let rec helper ~y =
    match y > bound with
    | true -> failwith "didn't find beacon"
    | false -> (
        let scanned_ranges = scanned_ranges ~y input in
        let unscanned_x =
          match List.hd_exn scanned_ranges with
          | start, _ when start > 0 -> Some 0
          | _, end_ when end_ < bound -> Some (end_ + 1)
          | _ -> None
        in
        match unscanned_x with Some x -> (x, y) | None -> helper ~y:(y + 1))
  in
  let x, y = helper ~y:0 in
  (x * 4000000) + y

let%expect_test _ =
  In_channel.read_all "day_15_input_test.txt"
  |> parse_input |> part_b ~bound:20 |> printf "%d\n";
  [%expect "56000011"]

let%expect_test _ =
  In_channel.read_all "day_15_input.txt"
  |> parse_input |> part_b ~bound:4000000 |> printf "%d\n";
  [%expect "10229191267339"]