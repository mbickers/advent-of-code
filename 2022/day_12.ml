open Core

module Point = struct
  module T = struct
    type t = int * int [@@deriving compare, sexp]
  end

  include T
  include Comparable.Make (T)
end

let test_input = {|Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi|}

let bfs ~start ~is_end ~can_reach =
  let rec bfs_step distance visited frontier =
    assert (not (Set.is_empty frontier));
    match Set.exists frontier ~f:is_end with
    | true -> distance
    | false ->
        let new_visited = Set.union visited frontier in
        let can_reach =
          Set.fold frontier ~init:Point.Set.empty ~f:(fun acc point ->
              can_reach ~from:point |> Point.Set.of_list |> Set.union acc)
        in
        let new_frontier = Set.diff can_reach new_visited in
        bfs_step (distance + 1) new_visited new_frontier
  in
  bfs_step 0 Point.Set.empty (Point.Set.singleton start)

let parse_input input =
  let width = String.index_exn input '\n' in
  let height = String.count input ~f:(Char.equal '\n') + 1 in
  let flattened = String.filter input ~f:Char.is_alpha in
  let start, end_ =
    let idx_to_point idx = (idx / width, idx % width) in
    ( String.index_exn flattened 'S' |> idx_to_point,
      String.index_exn flattened 'E' |> idx_to_point )
  in
  let point_to_height (r, c) =
    match String.get flattened ((r * width) + c) with
    | 'a' .. 'z' as c -> Char.to_int c - Char.to_int 'a'
    | 'S' -> 0
    | 'E' -> 25
    | _ -> failwith "unrecognized character"
  in
  let in_bounds (r, c) = 0 <= r && r < height && 0 <= c && c < width in
  (start, end_, in_bounds, point_to_height)

let part_a input =
  let start, end_, in_bounds, point_to_height = parse_input input in
  let can_reach ~from:(r, c) =
    let current_height = point_to_height (r, c) in
    [ (r - 1, c); (r + 1, c); (r, c - 1); (r, c + 1) ]
    |> List.filter ~f:in_bounds
    |> List.filter ~f:(fun point -> point_to_height point <= current_height + 1)
  in
  bfs ~start ~is_end:(Point.equal end_) ~can_reach

let%expect_test _ =
  part_a test_input |> printf "%d\n";
  [%expect "31"]

let%expect_test _ =
  part_a (In_channel.read_all "day_12_input.txt") |> printf "%d\n";
  [%expect "423"]

let part_b input =
  let _, end_, in_bounds, point_to_height = parse_input input in
  let can_reach ~from:(r, c) =
    let current_height = point_to_height (r, c) in
    [ (r - 1, c); (r + 1, c); (r, c - 1); (r, c + 1) ]
    |> List.filter ~f:in_bounds
    |> List.filter ~f:(fun point -> point_to_height point >= current_height - 1)
  in
  bfs ~start:end_ ~is_end:(fun p -> point_to_height p = 0) ~can_reach

let%expect_test _ =
  part_b test_input |> printf "%d\n";
  [%expect "29"]

let%expect_test _ =
  part_b (In_channel.read_all "day_12_input.txt") |> printf "%d\n";
  [%expect "416"]