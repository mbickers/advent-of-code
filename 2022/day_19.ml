open Core

module Blueprint = struct
  type t = {
    id : int;
    ore_robot_ore : int;
    clay_robot_ore : int;
    obsidian_robot_ore : int;
    obsidian_robot_clay : int;
    geode_robot_ore : int;
    geode_robot_obsidian : int;
  }
end

let parse_input input =
  let parse_blueprint line =
    Scanf.sscanf line
      "lueprint %d:%_s@Each ore robot costs %d ore.%_s@Each clay robot costs \
       %d ore.%_s@Each obsidian robot costs %d ore and %d clay.%_s@Each geode \
       robot costs %d ore and %d obsidian.%_s"
      (fun
        id
        ore_robot_ore
        clay_robot_ore
        obsidian_robot_ore
        obsidian_robot_clay
        geode_robot_ore
        geode_robot_obsidian
      ->
        {
          Blueprint.id;
          ore_robot_ore;
          clay_robot_ore;
          obsidian_robot_ore;
          obsidian_robot_clay;
          geode_robot_ore;
          geode_robot_obsidian;
        })
  in
  String.split ~on:'B' input |> List.tl_exn |> List.map ~f:parse_blueprint

let input = In_channel.read_all "day_19_input.txt" |> parse_input
let test_input = In_channel.read_all "day_19_input_test.txt" |> parse_input
let test_blueprint = List.hd_exn test_input

module State = struct
  module T = struct
    type t = {
      ore_robots : int;
      clay_robots : int;
      obsidian_robots : int;
      geode_robots : int;
      ore : int;
      clay : int;
      obsidian : int;
    }
    [@@deriving sexp, compare, fields, hash]
  end

  include T
  include Comparable.Make (T)

  let initial =
    {
      ore_robots = 1;
      clay_robots = 0;
      obsidian_robots = 0;
      geode_robots = 0;
      ore = 0;
      clay = 0;
      obsidian = 0;
    }

  let collect_resources ~old_t t =
    let { ore_robots; clay_robots; obsidian_robots; _ } = old_t in
    let ignore _ _ x = x in
    let change ~by _ _ x = x + by in
    Fields.Direct.map t ~ore_robots:ignore ~clay_robots:ignore
      ~obsidian_robots:ignore ~geode_robots:ignore ~ore:(change ~by:ore_robots)
      ~clay:(change ~by:clay_robots)
      ~obsidian:(change ~by:obsidian_robots)

  let try_make_ore_robot ~blueprint t =
    let { ore; _ } = t in
    let { Blueprint.ore_robot_ore; _ } = blueprint in
    let ignore _ _ x = x in
    let change ~by _ _ x = x + by in
    match Int.( >= ) ore ore_robot_ore with
    | true ->
        Some
          (Fields.Direct.map t ~ore_robots:(change ~by:1) ~clay_robots:ignore
             ~obsidian_robots:ignore ~geode_robots:ignore
             ~ore:(change ~by:(-ore_robot_ore))
             ~clay:ignore ~obsidian:ignore)
    | false -> None

  let try_make_clay_robot ~blueprint t =
    let { ore; _ } = t in
    let { Blueprint.clay_robot_ore; _ } = blueprint in
    let ignore _ _ x = x in
    let change ~by _ _ x = x + by in
    match Int.( >= ) ore clay_robot_ore with
    | true ->
        Some
          (Fields.Direct.map t ~ore_robots:ignore ~clay_robots:(change ~by:1)
             ~obsidian_robots:ignore ~geode_robots:ignore
             ~ore:(change ~by:(-clay_robot_ore))
             ~clay:ignore ~obsidian:ignore)
    | false -> None

  let try_make_obsidian_robot ~blueprint t =
    let { ore; clay; _ } = t in
    let { Blueprint.obsidian_robot_clay; obsidian_robot_ore; _ } = blueprint in
    let ignore _ _ x = x in
    let change ~by _ _ x = x + by in
    match
      Int.( >= ) ore obsidian_robot_ore && Int.( >= ) clay obsidian_robot_clay
    with
    | true ->
        Some
          (Fields.Direct.map t ~ore_robots:ignore ~clay_robots:ignore
             ~obsidian_robots:(change ~by:1) ~geode_robots:ignore
             ~ore:(change ~by:(-obsidian_robot_ore))
             ~clay:(change ~by:(-obsidian_robot_clay))
             ~obsidian:ignore)
    | false -> None

  let try_make_geode_robot ~blueprint t =
    let { ore; obsidian; _ } = t in
    let { Blueprint.geode_robot_ore; geode_robot_obsidian; _ } = blueprint in
    let ignore _ _ x = x in
    let change ~by _ _ x = x + by in
    match
      Int.( >= ) ore geode_robot_ore && Int.( >= ) obsidian geode_robot_obsidian
    with
    | true ->
        Some
          (Fields.Direct.map t ~ore_robots:ignore ~clay_robots:ignore
             ~obsidian_robots:ignore ~geode_robots:(change ~by:1)
             ~ore:(change ~by:(-geode_robot_ore))
             ~clay:ignore
             ~obsidian:(change ~by:(-geode_robot_obsidian)))
    | false -> None

  let branch ~blueprint =
    let should_keep =
      let {
        Blueprint.ore_robot_ore;
        clay_robot_ore;
        obsidian_robot_ore;
        obsidian_robot_clay;
        geode_robot_ore;
        geode_robot_obsidian;
        _;
      } =
        blueprint
      in
      let ore_max =
        List.max_elt ~compare:Int.compare
          [ ore_robot_ore; clay_robot_ore; obsidian_robot_ore; geode_robot_ore ]
        |> Option.value_exn
      in
      let open Int in
      fun { ore; clay; obsidian; ore_robots; clay_robots; obsidian_robots; _ } ->
        (not
           (ore > ore_max && clay > obsidian_robot_clay
           && obsidian > geode_robot_obsidian))
        && (not (ore_robots > ore_max))
        && (not (clay_robots > obsidian_robot_clay))
        && not (obsidian_robots > geode_robot_obsidian)
    in
    fun t ->
      let { obsidian_robots; clay_robots; geode_robots; _ } = t in
      let branches =
        (match (clay_robots, obsidian_robots) with
        | 0, _ ->
            [
              try_make_clay_robot ~blueprint t;
              Some t;
              try_make_ore_robot ~blueprint t;
            ]
        | _, 0 ->
            [
              try_make_obsidian_robot ~blueprint t;
              Some t;
              try_make_clay_robot ~blueprint t;
              try_make_ore_robot ~blueprint t;
            ]
        | _, _ ->
            [
              try_make_geode_robot ~blueprint t;
              Some t;
              try_make_obsidian_robot ~blueprint t;
              try_make_clay_robot ~blueprint t;
              try_make_ore_robot ~blueprint t;
            ])
        |> List.filter_opt |> List.filter ~f:should_keep
        |> List.map ~f:(collect_resources ~old_t:t)
      in
      (geode_robots, branches)

  let geode_upper_bound ~time { geode_robots; _ } =
    (time * geode_robots) + (time * (time - 1) / 2)
end

let blueprint_geodes ~blueprint ~time =
  let branch_state = State.branch ~blueprint in
  let module Key = struct
    type t = int * State.t [@@deriving hash, sexp, compare]
  end in
  let memo = Hashtbl.create (module Key) in
  let best_so_far = ref 0 in
  let rec dfs ~time ~current_geodes state =
    match time with
    | 0 -> Some 0
    | _ -> (
        match
          current_geodes + State.geode_upper_bound ~time state > !best_so_far
        with
        | false -> None
        | true -> (
            match Hashtbl.find memo (time, state) with
            | Some geodes -> Some geodes
            | None -> (
                let new_geodes, branches = branch_state state in
                let best_branch_geodes =
                  List.map branches
                    ~f:
                      (dfs ~time:(time - 1)
                         ~current_geodes:(current_geodes + new_geodes))
                  |> List.filter_opt
                  |> List.max_elt ~compare:Int.compare
                in
                match best_branch_geodes with
                | None -> None
                | Some best_branch_geodes ->
                    let result = best_branch_geodes + new_geodes in
                    Hashtbl.set memo ~key:(time, state) ~data:result;
                    (match !best_so_far < result with
                    | true -> best_so_far := result
                    | false -> ());
                    Some result)))
  in
  dfs ~time ~current_geodes:0 State.initial |> Option.value_exn

let%expect_test _ =
  blueprint_geodes ~blueprint:test_blueprint ~time:24 |> printf "%d\n";
  [%expect "9"]

let%expect_test _ =
  let blueprint = List.hd_exn test_input in
  blueprint_geodes ~blueprint ~time:32 |> printf "%d\n";
  [%expect "62"]

let part_a blueprints =
  let quality_levels =
    List.map blueprints ~f:(fun blueprint ->
        let geodes = blueprint_geodes ~blueprint ~time:24 in
        let { Blueprint.id; _ } = blueprint in
        id * geodes)
  in
  List.reduce_exn quality_levels ~f:( + )

let%expect_test _ =
  test_input |> part_a |> printf "%d\n";
  [%expect "33"]

let%expect_test _ =
  input |> part_a |> printf "%d\n";
  [%expect "1177"]

let part_b blueprints =
  let blueprints = List.take blueprints 3 in
  let geodes =
    List.map blueprints ~f:(fun blueprint ->
        blueprint_geodes ~blueprint ~time:32)
  in
  List.reduce_exn geodes ~f:( * )

let%expect_test _ =
  input |> part_b |> printf "%d\n";
  [%expect "62744"]