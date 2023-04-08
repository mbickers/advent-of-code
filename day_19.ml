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

  let try_make_nothing ~blueprint =
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
    fun t ->
      let { ore; clay; obsidian; _ } = t in
      match
        Int.( >= ) ore ore_max
        && Int.( >= ) clay obsidian_robot_clay
        && Int.( >= ) obsidian geode_robot_obsidian
      with
      | true -> None
      | false -> Some t

  let branch ~blueprint =
    let try_make_nothing = try_make_nothing ~blueprint in
    fun t ->
      let { geode_robots; _ } = t in
      let branches =
        [
          try_make_nothing t;
          try_make_clay_robot ~blueprint t;
          try_make_ore_robot ~blueprint t;
          try_make_obsidian_robot ~blueprint t;
          try_make_geode_robot ~blueprint t;
        ]
        |> List.filter_opt
        |> List.map ~f:(collect_resources ~old_t:t)
      in
      (geode_robots, branches)

  let greedy_geodes_memoized ~blueprint =
    let try_make_nothing = try_make_nothing ~blueprint in
    let module Key = struct
      type nonrec t = int * t [@@deriving hash, sexp, compare]
    end in
    let memo = Hashtbl.create (module Key) in
    fun ~time t ->
      let rec helper ~time t =
        match time with
        | 0 -> 0
        | _ -> (
            match Hashtbl.find memo (time, t) with
            | Some value -> value
            | None ->
                let { obsidian_robots; clay_robots; geode_robots; _ } = t in
                let branches =
                  match (clay_robots, obsidian_robots) with
                  | 0, _ ->
                      [ try_make_clay_robot ~blueprint t; try_make_nothing t ]
                  | _, 0 ->
                      [
                        try_make_obsidian_robot ~blueprint t; try_make_nothing t;
                      ]
                  | _, _ ->
                      [ try_make_geode_robot ~blueprint t; try_make_nothing t ]
                in
                let branch = List.filter_opt branches |> List.hd_exn in
                let advanced = collect_resources ~old_t:t branch in
                let result = geode_robots + helper ~time:(time - 1) advanced in
                Hashtbl.set memo ~key:(time, t) ~data:result;
                result)
      in
      helper ~time t

  let geode_lower_bound ~time { geode_robots; _ } = time * geode_robots

  let geode_upper_bound ~time { geode_robots; _ } =
    (time * geode_robots) + (time * (time - 1) / 2)

  let strictly_worse t1 t2 =
    match equal t1 t2 with
    | true -> false
    | false ->
        let to_list
            {
              ore_robots;
              clay_robots;
              obsidian_robots;
              geode_robots;
              ore;
              clay;
              obsidian;
            } =
          [
            ore_robots;
            clay_robots;
            obsidian_robots;
            geode_robots;
            ore;
            clay;
            obsidian;
          ]
        in
        List.for_all2_exn (to_list t1) (to_list t2) ~f:Int.( <= )
end

let filter_strictly_worse geode_counts =
  Map.filteri geode_counts ~f:(fun ~key:victim_state ~data:victim_geode_count ->
      Map.for_alli geode_counts ~f:(fun ~key:state ~data:geode_count ->
          victim_geode_count >= geode_count
          || not (State.strictly_worse victim_state state)))

let blueprint_geodes ~blueprint ~time =
  let branch_state = State.branch ~blueprint in
  let rec helper ~time ~geode_counts =
    printf "size at %d: %d\n%!" time (Map.length geode_counts);
    match time with
    | 0 -> Map.data geode_counts |> List.max_elt ~compare:Int.compare
    | _ ->
        let geode_counts =
          Map.to_alist geode_counts
          |> List.map ~f:(fun (state, geode_count) ->
                 let new_geodes, branches = branch_state state in
                 List.map branches ~f:(fun branch ->
                     (branch, geode_count + new_geodes)))
          |> List.join |> State.Map.of_alist_multi
          |> Map.map ~f:(fun geode_counts ->
                 List.max_elt ~compare:Int.compare geode_counts
                 |> Option.value_exn)
        in
        let max_lower_bound =
          Map.fold geode_counts ~init:0
            ~f:(fun ~key:state ~data:geode_count max_lower_bound ->
              Int.max max_lower_bound
                (geode_count + State.geode_lower_bound ~time state))
        in
        let geode_counts =
          Map.filteri geode_counts ~f:(fun ~key:state ~data:geode_count ->
              geode_count + State.geode_upper_bound ~time state
              >= max_lower_bound)
        in
        helper ~time:(time - 1) ~geode_counts
  in
  helper ~time ~geode_counts:(State.Map.singleton State.initial 0)
  |> Option.value_exn

let%expect_test _ =
  blueprint_geodes ~blueprint:test_blueprint ~time:24 |> printf "%d\n";
  [%expect "9"]

(*
let%expect_test _ =
  let blueprint = List.hd_exn test_input in
  blueprint_geodes ~blueprint ~time:32 |> printf "%d\n";
  [%expect "62"]
*)

(*
    
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
  [%expect "1177"]
 *)