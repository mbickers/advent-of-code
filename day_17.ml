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
  type t = Point.Set.t [@@deriving compare, sexp]

  let to_string t =
    let bottom_row =
      let { Point.row; _ } = Set.min_elt_exn t in
      row
    in
    let top_row =
      let { Point.row; _ } = Set.max_elt_exn t in
      row
    in
    let empty_chamber =
      List.init (top_row - bottom_row + 1) ~f:(fun _ -> "       ")
      |> String.concat ~sep:"\n"
    in
    Set.fold ~init:empty_chamber t ~f:(fun chamber { row; col } ->
        let rows_from_top = top_row - row in
        let offset = (rows_from_top * 8) + col in
        String.mapi chamber ~f:(fun map_offset current ->
            match offset = map_offset with true -> '#' | false -> current))

  let empty =
    List.init 7 ~f:(fun col -> { Point.row = 0; col }) |> Point.Set.of_list

  let is_solid t { Point.row; col } =
    match col < 0 || col > 6 with
    | true -> `Border
    | false -> (
        match Set.mem t { Point.row; col } with true -> `Rock | false -> `No)

  let would_collide t rock =
    List.exists rock ~f:(fun point ->
        match is_solid t point with `Border | `Rock -> true | `No -> false)

  let add t rock =
    let added = Set.union t (Point.Set.of_list rock) in
    let { Point.row = added_height; _ } = Set.max_elt_exn added in
    let shifted =
      Point.Set.map added ~f:(fun { Point.row; col } ->
          { Point.row = row - added_height; col })
    in
    let rec find_surface_points ~frontier ~visited_air ~visited_surface =
      match Set.length frontier with
      | 0 -> visited_surface
      | _ ->
          let visited_air = Set.union visited_air frontier in
          let new_visited_surface, new_frontier, _ =
            Set.to_list frontier
            |> List.map ~f:(fun point ->
                   Point.
                     [
                       offset ~rows:0 ~cols:(-1) point;
                       offset ~rows:0 ~cols:1 point;
                       offset ~rows:(-1) ~cols:0 point;
                     ])
            |> List.join
            |> List.filter ~f:(fun point ->
                   not
                     (Set.mem visited_air point || Set.mem visited_surface point))
            |> List.partition3_map ~f:(fun point ->
                   match is_solid shifted point with
                   | `Rock -> `Fst point
                   | `No -> `Snd point
                   | `Border -> `Trd point)
          in
          find_surface_points
            ~frontier:(Point.Set.of_list new_frontier)
            ~visited_air
            ~visited_surface:
              (Set.union visited_surface
                 (Point.Set.of_list new_visited_surface))
    in
    let surface_points =
      find_surface_points
        ~frontier:(Point.Set.singleton { Point.row = 1; col = 0 })
        ~visited_air:Point.Set.empty ~visited_surface:Point.Set.empty
    in
    (added_height, surface_points)
end

module State = struct
  module T = struct
    type t = { wind_idx : int; rock_idx : int; rock_chamber : Rock_chamber.t }
    [@@deriving compare, sexp]
  end

  include T
  include Comparable.Make (T)
end

let drop_rock ~wind ~wind_idx ~rock ~rock_chamber =
  let rec drop_until_stuck shifted_rock wind_idx =
    let try_shift ~rows ~cols rock =
      let shifted_rock = Point.offset_rock ~rows ~cols rock in
      match Rock_chamber.would_collide rock_chamber shifted_rock with
      | true -> `Would_collide rock
      | false -> `Shifted shifted_rock
    in
    let wind_offset =
      match String.get wind wind_idx with
      | '<' -> -1
      | '>' -> 1
      | _ -> failwith "invalid input file"
    in
    let shifted_rock =
      match try_shift ~rows:0 ~cols:wind_offset shifted_rock with
      | `Would_collide rock | `Shifted rock -> rock
    in
    let next_wind_idx = (wind_idx + 1) % String.length wind in
    match try_shift ~rows:(-1) ~cols:0 shifted_rock with
    | `Would_collide rock -> (rock, next_wind_idx)
    | `Shifted rock -> drop_until_stuck rock next_wind_idx
  in
  let resting_rock, wind_idx =
    drop_until_stuck (Point.offset_rock ~rows:4 ~cols:2 rock) wind_idx
  in
  let added_height, rock_chamber = Rock_chamber.add rock_chamber resting_rock in
  (wind_idx, added_height, rock_chamber)

let drop_rocks ~num_rocks wind =
  let rec drop_rocks ~current_height ~num_rocks ~wind_idx ~rock_idx
      ~rock_chamber ~first_reached =
    match num_rocks with
    | 0 -> current_height
    | num_rocks -> (
        let this_state = { State.wind_idx; rock_idx; rock_chamber } in
        match Map.find first_reached this_state with
        | Some (prev_num_rocks, prev_height) ->
            let cycle_period = prev_num_rocks - num_rocks in
            let cycle_added_height = current_height - prev_height in
            let cycle_repetitions = num_rocks / cycle_period in
            let resume_at = num_rocks % cycle_period in
            drop_rocks
              ~current_height:
                (current_height + (cycle_repetitions * cycle_added_height))
              ~num_rocks:resume_at ~wind_idx ~rock_idx ~rock_chamber
              ~first_reached:State.Map.empty
        | None ->
            let first_reached =
              Map.add_exn first_reached ~key:this_state
                ~data:(num_rocks, current_height)
            in
            let wind_idx, added_height, rock_chamber =
              drop_rock ~wind ~wind_idx
                ~rock:(List.nth_exn rocks rock_idx)
                ~rock_chamber
            in
            let rock_idx = (rock_idx + 1) % List.length rocks in
            drop_rocks
              ~current_height:(current_height + added_height)
              ~num_rocks:(num_rocks - 1) ~wind_idx ~rock_idx ~rock_chamber
              ~first_reached)
  in
  drop_rocks ~current_height:0 ~num_rocks ~wind_idx:0 ~rock_idx:0
    ~rock_chamber:Rock_chamber.empty ~first_reached:State.Map.empty

let test_input = In_channel.read_all "day_17_input_test.txt"
let input = In_channel.read_all "day_17_input.txt"

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
  [%expect "1507692307690"]
