open Core

module Turn = struct
  type t = Clockwise | Counterclockwise [@@deriving sexp]
end

module Instruction = struct
  type t = Turn of Turn.t | Move of int [@@deriving sexp]
end

module Heading = struct
  type t = Right | Down | Left | Up [@@deriving sexp]

  let to_string t =
    match t with
    | Right -> "Right"
    | Down -> "Down"
    | Left -> "Left"
    | Up -> "Up"

  let apply ~turn t =
    let turn_clockwise t =
      match t with Right -> Down | Down -> Left | Left -> Up | Up -> Right
    in
    match turn with
    | Turn.Clockwise -> turn_clockwise t
    | Counterclockwise -> Fn.apply_n_times ~n:3 turn_clockwise t

  let move ~position:(row, col) t =
    match t with
    | Right -> (row, col + 1)
    | Down -> (row + 1, col)
    | Left -> (row, col - 1)
    | Up -> (row - 1, col)
end

let score ((row, col), heading) =
  let facing_score =
    match heading with Heading.Right -> 0 | Down -> 1 | Left -> 2 | Up -> 3
  in
  (1000 * (row + 1)) + (4 * (col + 1)) + facing_score

module Grid = struct
  type 'a t = 'a list list [@@deriving sexp]

  let height = List.length
  let width t = List.hd_exn t |> List.length

  let get_exn ~position:(row, col) t =
    let row = List.nth_exn t row in
    List.nth_exn row col

  let get ~position:(row, col) t =
    match 0 <= row && row < height t && 0 <= col && col < width t with
    | true ->
        let row = List.nth_exn t row in
        Some (List.nth_exn row col)
    | false -> None
end

module Board = struct
  type tile = Open | Solid | Empty
  type t = tile Grid.t

  let of_string input =
    let rows = String.split_lines input in
    let width =
      List.map rows ~f:String.length
      |> List.max_elt ~compare:Int.compare
      |> Option.value_exn
    in
    List.map rows ~f:(fun row ->
        let row = row ^ String.make (width - String.length row) ' ' in
        String.to_list row
        |> List.map ~f:(fun tile ->
               match tile with
               | ' ' -> Empty
               | '.' -> Open
               | '#' -> Solid
               | _ -> failwith "invalid character"))

  let initial_position t =
    let col =
      List.find_mapi_exn (List.hd_exn t) ~f:(fun col tile ->
          match tile with Open -> Some col | Solid | Empty -> None)
    in
    (0, col)

  let rec try_move ~move ~position ~heading ~number t =
    match number with
    | 0 -> (position, heading)
    | number -> (
        match move ~position ~heading with
        | `blocked -> (position, heading)
        | `moved (new_position, new_heading) ->
            try_move ~move ~position:new_position ~heading:new_heading
              ~number:(number - 1) t)

  let follow_path ~move instructions t =
    List.fold instructions
      ~init:(initial_position t, Heading.Right)
      ~f:(fun (position, heading) instruction ->
        (position, heading) |> [%sexp_of: (int * int) * Heading.t] |> print_s;
        match instruction with
        | Instruction.Turn turn -> (position, Heading.apply ~turn heading)
        | Move number -> try_move ~move ~position ~heading ~number t)
end

let parse_input input =
  let board, instructions =
    let break = String.substr_index_exn input ~pattern:"\n\n" in
    ( String.slice input 0 break,
      String.slice input (break + 2) (String.length input) )
  in
  let parse_instructions instructions =
    String.to_list instructions
    |> List.group ~break:(fun first second ->
           match (first, second) with
           | _, 'L' | _, 'R' | 'L', _ | 'R', _ -> true
           | _ -> false)
    |> List.map ~f:(fun chars ->
           match chars with
           | [ 'L' ] -> Instruction.Turn Counterclockwise
           | [ 'R' ] -> Turn Clockwise
           | digits -> Move (String.of_char_list digits |> Int.of_string))
  in
  (Board.of_string board, parse_instructions instructions)

let test_input = In_channel.read_all "day_22_input_test.txt" |> parse_input
let input = In_channel.read_all "day_22_input.txt" |> parse_input

let part_a (board, instructions) =
  let rec move ~position ~heading =
    let wrap ~position:(row, col) =
      (row % Grid.height board, col % Grid.width board)
    in
    let new_position = wrap ~position:(Heading.move ~position heading) in
    match Grid.get_exn ~position:new_position board with
    | Board.Solid -> `blocked
    | Open -> `moved (new_position, heading)
    | Empty -> move ~position:new_position ~heading
  in
  Board.follow_path ~move instructions board |> score

(* let%expect_test _ =
     test_input |> part_a |> printf "%d\n";
     [%expect "6032"]

   let%expect_test _ =
     input |> part_a |> printf "%d\n";
     [%expect "13566"] *)

module Face_corner = struct
  type t = Upper_right | Lower_right | Lower_left | Upper_left
  [@@deriving equal, sexp]

  let opposite t =
    match t with
    | Upper_right -> Lower_left
    | Lower_right -> Upper_left
    | Lower_left -> Upper_right
    | Upper_left -> Lower_right
end

module Board_cube = struct
  type t = {
    face_width : int;
    regions : int Option.t Grid.t;
    cube_corners : (Face_corner.t * int) list list;
  }
  [@@deriving sexp]

  let of_board (board : Board.t) =
    let face_width =
      List.fold ~init:Int.max_value board ~f:(fun shortest_region_length row ->
          let contiguous_region_lengths =
            List.group row ~break:(fun tile1 tile2 ->
                match (tile1, tile2) with
                | Board.Empty, Board.Empty -> false
                | Board.Empty, Board.Open | _, Board.Empty -> true
                | _ -> false)
            |> List.map ~f:List.length
          in
          List.reduce_exn
            (shortest_region_length :: contiguous_region_lengths)
            ~f:Int.min)
    in
    let regions =
      let upper_left_corners =
        List.filteri board ~f:(fun row _ -> row % face_width = 0)
        |> List.map ~f:(List.filteri ~f:(fun col _ -> col % face_width = 0))
      in
      List.folding_map upper_left_corners ~init:1 ~f:(fun face_index row ->
          List.fold_map row ~init:face_index ~f:(fun face_index tile ->
              match tile with
              | Open | Solid -> (face_index + 1, Some face_index)
              | Empty -> (face_index, None)))
    in
    let initial_cube_corners =
      let potential_upper_rights =
        List.cartesian_product
          (List.init (Grid.height regions + 1) ~f:Fn.id)
          (List.init (Grid.width regions + 1) ~f:Fn.id)
      in
      List.map potential_upper_rights ~f:(fun (row, col) ->
          let region (row, col) =
            Grid.get ~position:(row, col) regions |> Option.join
          in
          let potential_face_corners =
            [
              (Face_corner.Lower_right, region (row - 1, col - 1));
              (Lower_left, region (row - 1, col));
              (Upper_left, region (row, col));
              (Upper_right, region (row, col - 1));
            ]
          in
          List.filter_map potential_face_corners
            ~f:(fun (face_corner, region) ->
              match region with
              | Some region -> Some (face_corner, region)
              | None -> None))
      |> List.filter ~f:(fun corner -> not (List.is_empty corner))
    in
    let corner_regions face_corners =
      List.map face_corners ~f:(fun (_, region) -> region) |> Int.Set.of_list
    in
    let rec join_cube_corners cube_corners =
      match List.length cube_corners = 8 with
      | true -> cube_corners
      | false ->
          let merge_indices ~index1 ~index2 cube_corners =
            let removed =
              List.filteri cube_corners ~f:(fun index _ ->
                  index <> index1 && index <> index2)
            in
            let merged =
              List.nth_exn cube_corners index1
              @ List.nth_exn cube_corners index2
            in
            merged :: removed
          in
          let corner_index1, corner_index2 =
            List.find_map_exn cube_corners ~f:(fun primary_face_corners ->
                match List.length primary_face_corners = 3 with
                | false -> None
                | true -> (
                    let primary_face_corner_regions =
                      corner_regions primary_face_corners
                    in
                    let adjacent_cube_corners =
                      List.filter cube_corners ~f:(fun face_corners ->
                          let secondary_face_corner_regions =
                            corner_regions face_corners
                          in
                          Set.length
                            (Set.inter primary_face_corner_regions
                               secondary_face_corner_regions)
                          = 2)
                    in
                    match List.length adjacent_cube_corners with
                    | 3 | 1 -> None
                    | 2 -> (
                        let shared_region =
                          Set.find_exn primary_face_corner_regions
                            ~f:(fun region_to_check ->
                              List.for_all adjacent_cube_corners
                                ~f:
                                  (List.exists ~f:(fun (_, region) ->
                                       region = region_to_check)))
                        in
                        let face_corners_to_join =
                          List.map adjacent_cube_corners ~f:(fun face_corners ->
                              let face_corner, region =
                                List.find_exn face_corners
                                  ~f:(fun (_, region) ->
                                    region <> shared_region
                                    && Set.mem primary_face_corner_regions
                                         region)
                              in
                              (Face_corner.opposite face_corner, region))
                        in
                        let cube_corners_to_join_indicies =
                          List.filter_mapi cube_corners
                            ~f:(fun index face_corners ->
                              match
                                List.exists face_corners ~f:(fun face_corner ->
                                    List.mem
                                      ~equal:
                                        (Tuple2.equal ~eq1:Face_corner.equal
                                           ~eq2:( = ))
                                      face_corners_to_join face_corner)
                              with
                              | true -> Some index
                              | false -> None)
                        in
                        match cube_corners_to_join_indicies with
                        | [ corner_index1; corner_index2 ] ->
                            Some (corner_index1, corner_index2)
                        | _ -> None)
                    | x -> failwith ("impossible here: " ^ Int.to_string x)))
          in
          let cube_corners =
            merge_indices ~index1:corner_index1 ~index2:corner_index2
              cube_corners
          in
          join_cube_corners cube_corners
    in
    let cube_corners = join_cube_corners initial_cube_corners in
    { face_width; regions; cube_corners }

  let region ~position:(row, col) { face_width; regions; _ } =
    Grid.get ~position:(row / face_width, col / face_width) regions
    |> Option.join

  let move ~position ~heading t =
    let current_region = region ~position t |> Option.value_exn in
    let naive_move_position = Heading.move ~position heading in
    match region ~position:naive_move_position t with
    | Some region when region = current_region -> (naive_move_position, heading)
    | _ ->
        let (row, col), { face_width; regions; cube_corners } = (position, t) in
        let row_in_grid, col_in_grid = (row % face_width, col % face_width) in
        let current_primary_corner, current_secondary_corner, offset =
          match heading with
          | Heading.Up ->
              (Face_corner.Upper_left, Face_corner.Upper_right, col_in_grid)
          | Right -> (Upper_right, Lower_right, row_in_grid)
          | Down -> (Lower_left, Lower_right, col_in_grid)
          | Left -> (Upper_left, Lower_left, row_in_grid)
        in
        let primary_cube_corner, secondary_cube_corner =
          let cube_corner_containing face_corner =
            List.find_exn cube_corners
              ~f:
                (List.exists
                   ~f:
                     ((Tuple2.equal ~eq1:Face_corner.equal ~eq2:Int.equal)
                        face_corner))
          in
          ( cube_corner_containing (current_primary_corner, current_region),
            cube_corner_containing (current_secondary_corner, current_region) )
        in
        let new_region, new_primary_corner, new_secondary_corner =
          List.find_map_exn primary_cube_corner
            ~f:(fun (primary_face_corner, region) ->
              List.find_map secondary_cube_corner
                ~f:(fun (secondary_face_corner, region_to_test) ->
                  Option.some_if
                    (region <> current_region && region = region_to_test)
                    (region, primary_face_corner, secondary_face_corner)))
        in
        printf "moving region %d -> %d\n" current_region new_region;
        let row_in_region, col_in_region, heading =
          let mirrored_offset = face_width - offset - 1 in
          match (new_primary_corner, new_secondary_corner) with
          | Face_corner.Upper_left, Face_corner.Upper_right ->
              (0, offset, Heading.Down)
          | Upper_right, Upper_left -> (0, mirrored_offset, Heading.Down)
          | Upper_right, Lower_right -> (offset, face_width - 1, Left)
          | Lower_right, Upper_right -> (mirrored_offset, face_width - 1, Left)
          | Lower_left, Lower_right -> (face_width - 1, offset, Up)
          | Lower_right, Lower_left -> (face_width - 1, mirrored_offset, Up)
          | Upper_left, Lower_left -> (offset, 0, Right)
          | Lower_left, Upper_left -> (0, mirrored_offset, Right)
          | _ -> failwith "must have distinct face corners"
        in
        let region_row, region_col =
          List.find_mapi_exn regions ~f:(fun row ->
              List.find_mapi ~f:(fun col ->
                  Option.bind ~f:(fun region_to_test ->
                      Option.some_if (new_region = region_to_test) (row, col))))
        in
        let row, col =
          ( (region_row * face_width) + row_in_region,
            (region_col * face_width) + col_in_region )
        in
        ((row, col), heading)
end

let part_b (board, instructions) =
  let cube = Board_cube.of_board board in
  let move ~position ~heading =
    let new_position, new_heading = Board_cube.move ~position ~heading cube in
    match Grid.get_exn ~position:new_position board with
    | Board.Solid -> `blocked
    | Open -> `moved (new_position, new_heading)
    | Empty -> failwith "should've moved to new cube face"
  in
  (* let instructions = List.take instructions 8 in *)
  Board.follow_path ~move instructions board |> score

let%expect_test _ =
  test_input |> part_b |> printf "%d\n";
  [%expect "5031"]

let%expect_test _ =
  input |> part_b |> printf "%d\n";
  [%expect "5031"]
