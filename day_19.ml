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
    [@@deriving sexp, compare, fields]
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

  let branch
      ~blueprint:
        {
          Blueprint.ore_robot_ore;
          clay_robot_ore;
          obsidian_robot_ore;
          obsidian_robot_clay;
          geode_robot_ore;
          geode_robot_obsidian;
          _;
        } =
    let ore_max =
      List.max_elt ~compare:Int.compare
        [ ore_robot_ore; clay_robot_ore; obsidian_robot_ore; geode_robot_ore ]
      |> Option.value_exn
    in
    fun {
          ore_robots;
          clay_robots;
          obsidian_robots;
          geode_robots;
          ore;
          clay;
          obsidian;
        } ->
      let new_ore, new_clay, new_obsidian, mined_geodes =
        ( ore + ore_robots,
          clay + clay_robots,
          obsidian + obsidian_robots,
          geode_robots )
      in
      let open Int in
      let make_nothing =
        match
          ore >= ore_max
          && clay >= obsidian_robot_clay
          && obsidian >= geode_robot_obsidian
        with
        | true -> None
        | false ->
            Some
              {
                ore_robots;
                clay_robots;
                obsidian_robots;
                geode_robots;
                ore = new_ore;
                clay = new_clay;
                obsidian = new_obsidian;
              }
      in
      let make_ore_robot =
        match ore >= ore_robot_ore with
        | true ->
            Some
              {
                ore_robots = ore_robots + 1;
                clay_robots;
                obsidian_robots;
                geode_robots;
                ore = new_ore - ore_robot_ore;
                clay = new_clay;
                obsidian = new_obsidian;
              }
        | false -> None
      in
      let make_clay_robot =
        match ore >= clay_robot_ore with
        | true ->
            Some
              {
                ore_robots;
                clay_robots = clay_robots + 1;
                obsidian_robots;
                geode_robots;
                ore = new_ore - clay_robot_ore;
                clay = new_clay;
                obsidian = new_obsidian;
              }
        | false -> None
      in
      let make_obsidian_robot =
        match ore >= obsidian_robot_ore && clay >= obsidian_robot_clay with
        | true ->
            Some
              {
                ore_robots;
                clay_robots;
                obsidian_robots = obsidian_robots + 1;
                geode_robots;
                ore = new_ore - obsidian_robot_ore;
                clay = new_clay - obsidian_robot_clay;
                obsidian = new_obsidian;
              }
        | false -> None
      in
      let make_geode_robot =
        match ore >= geode_robot_ore && obsidian >= geode_robot_obsidian with
        | true ->
            Some
              {
                ore_robots;
                clay_robots;
                obsidian_robots;
                geode_robots = geode_robots + 1;
                ore = new_ore - geode_robot_ore;
                clay = new_clay;
                obsidian = new_obsidian - geode_robot_obsidian;
              }
        | false -> None
      in
      let branches =
        [
          make_nothing;
          make_ore_robot;
          make_clay_robot;
          make_obsidian_robot;
          make_geode_robot;
        ]
        |> List.filter_opt
      in
      (mined_geodes, branches)

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