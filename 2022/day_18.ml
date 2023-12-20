open Core

module Point = struct
  module T = struct
    type t = int * int * int [@@deriving compare, sexp]
  end

  include T
  include Comparable.Make (T)

  let add (x1, y1, z1) (x2, y2, z2) = (x1 + x2, y1 + y2, z1 + z2)

  let neighbors t =
    let offsets =
      [ (1, 0, 0); (-1, 0, 0); (0, 1, 0); (0, -1, 0); (0, 0, 1); (0, 0, -1) ]
    in
    List.map offsets ~f:(add t)

  let to_list (x, y, z) = [ x; y; z ]

  let of_list list =
    match list with [ x; y; z ] -> (x, y, z) | _ -> failwith "invalid list"
end

let input_to_points input =
  String.split_lines input
  |> List.map ~f:(fun line ->
         String.split ~on:',' line |> List.map ~f:Int.of_string |> Point.of_list)

let test_input = In_channel.read_all "day_18_input_test.txt" |> input_to_points
let input = In_channel.read_all "day_18_input.txt" |> input_to_points

let part_a points =
  let points = Point.Set.of_list points in
  Set.fold points ~init:0 ~f:(fun surface_area point ->
      let exposed_sides =
        List.count (Point.neighbors point) ~f:(fun neighbor ->
            not (Set.mem points neighbor))
      in
      surface_area + exposed_sides)

let%expect_test _ =
  test_input |> part_a |> printf "%d\n";
  [%expect "64"]

let%expect_test _ =
  input |> part_a |> printf "%d\n";
  [%expect "3454"]

let part_b points =
  let scanned_points = Point.Set.of_list points in
  let max_coordinate =
    Set.fold scanned_points ~init:0 ~f:(fun max_coord (x, y, z) ->
        List.max_elt ~compare:Int.compare [ max_coord; x; y; z ]
        |> Option.value_exn)
  in
  let rec search ~frontier ~visited ~surface_area =
    let in_bounds point =
      Point.to_list point
      |> List.for_all ~f:(fun coord ->
             -1 <= coord && coord <= max_coordinate + 1)
    in
    match Set.is_empty frontier with
    | true -> surface_area
    | false ->
        let next_frontier, new_surface_area =
          Set.fold frontier ~init:(Point.Set.empty, 0)
            ~f:(fun (next_frontier, surface_area) point ->
              let new_next_frontier =
                Point.neighbors point
                |> List.filter ~f:(fun point ->
                       (not (Set.mem scanned_points point))
                       && in_bounds point
                       && not (Set.mem visited point))
                |> Point.Set.of_list
              in
              let new_surface_area =
                Point.neighbors point |> List.count ~f:(Set.mem scanned_points)
              in
              ( Set.union next_frontier new_next_frontier,
                surface_area + new_surface_area ))
        in
        search ~frontier:next_frontier
          ~visited:(Set.union visited frontier)
          ~surface_area:(surface_area + new_surface_area)
  in
  search
    ~frontier:(Point.Set.singleton (-1, -1, -1))
    ~visited:Point.Set.empty ~surface_area:0

let%expect_test _ =
  test_input |> part_b |> printf "%d\n";
  [%expect "58"]

let%expect_test _ =
  input |> part_b |> printf "%d\n";
  [%expect "2014"]