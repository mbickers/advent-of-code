open Core

module Resource = struct
  type t = Ore | Clay | Obsidian | Geode [@@deriving sexp, equal, compare]
end

module Robot = struct
  type t = Robot of Resource.t [@@deriving sexp, equal, compare]
end

module Blueprint = struct
  type t = {
    id : int;
    robot_costs : (Robot.t, (Resource.t, int) List.Assoc.t) List.Assoc.t;
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
        ore_ore
        clay_ore
        obsidian_ore
        obsidian_clay
        geode_ore
        geode_obsidian
      ->
        let robot_costs =
          [
            (Robot.Robot Ore, [ (Resource.Ore, ore_ore) ]);
            (Robot Clay, [ (Ore, clay_ore) ]);
            (Robot Obsidian, [ (Ore, obsidian_ore); (Clay, obsidian_clay) ]);
            (Robot Geode, [ (Ore, geode_ore); (Obsidian, geode_obsidian) ]);
          ]
        in
        { Blueprint.id; robot_costs })
  in
  String.split ~on:'B' input |> List.tl_exn |> List.map ~f:parse_blueprint

let input = In_channel.read_all "day_19_input.txt" |> parse_input
let test_input = In_channel.read_all "day_19_input_test.txt" |> parse_input

module State = struct
  module T = struct
    type t = {
      robots : (Robot.t * int) List.t;
      resources : (Resource.t * int) List.t;
    }
    [@@deriving sexp, compare]
  end

  include T
  include Comparable.Make (T)

  let initial =
    {
      robots =
        [
          (Robot Ore, 1); (Robot Clay, 0); (Robot Obsidian, 0); (Robot Geode, 0);
        ];
      resources = [ (Ore, 0); (Clay, 0); (Obsidian, 0); (Geode, 0) ];
    }

  let branch ~blueprint:{ Blueprint.robot_costs; _ } { robots; resources } =
    let could_build_and_cost =
      List.filter robot_costs ~f:(fun (_, resource_costs) ->
          List.for_all resource_costs ~f:(fun (resource, count) ->
              Int.( >= )
                (List.Assoc.find_exn resources ~equal:Resource.equal resource)
                count))
    in
    let resources =
      List.map resources ~f:(fun (resource, resource_count) ->
          let robot_count =
            List.Assoc.find robots ~equal:Robot.equal (Robot resource)
            |> Option.value ~default:0
          in
          (resource, robot_count + resource_count))
    in
    let build_nothing_branch = { robots; resources } in
    let build_branches =
      List.map could_build_and_cost
        ~f:(fun (new_robot, new_robot_resource_costs) ->
          let robots =
            List.map robots ~f:(fun (robot, count) ->
                match Robot.equal robot new_robot with
                | true -> (robot, count + 1)
                | false -> (robot, count))
          in
          let resources =
            List.map resources ~f:(fun (resource, count) ->
                match
                  List.Assoc.find new_robot_resource_costs ~equal:Resource.equal
                    resource
                with
                | Some cost -> (resource, count - cost)
                | None -> (resource, count))
          in
          { robots; resources })
    in
    build_nothing_branch :: build_branches

  let collect { robots; resources } = { robots; resources }

  let geode_lower_bound ~time { resources; robots } =
    List.Assoc.find_exn resources ~equal:Resource.equal Geode
    + (time * List.Assoc.find_exn robots ~equal:Robot.equal (Robot Geode))

  let geode_upper_bound ~time { resources; robots } =
    geode_lower_bound ~time { resources; robots } + (time * (time - 1) / 2)
end

let blueprint_geodes ~blueprint ~time =
  let rec helper ~time ~branches =
    match time with
    | 0 ->
        Set.to_list branches
        |> List.map ~f:(fun { State.resources; _ } ->
               List.Assoc.find_exn resources ~equal:Resource.equal Geode)
        |> List.max_elt ~compare:Int.compare
        |> Option.value_exn
    | time ->
        let branches =
          Set.to_list branches
          |> List.map ~f:(State.branch ~blueprint)
          |> List.join |> State.Set.of_list
        in
        let max_lower_bound =
          Set.fold branches ~init:0 ~f:(fun lower_bound state ->
              let this_lower_bound = State.geode_lower_bound ~time state in
              Int.max lower_bound this_lower_bound)
        in
        let branches =
          Set.filter branches ~f:(fun state ->
              State.geode_upper_bound ~time state >= max_lower_bound)
        in
        helper ~time:(time - 1) ~branches
  in

  helper ~time ~branches:(State.Set.singleton State.initial)

(*
    
let%expect_test _ =
  let blueprint = List.hd_exn test_input in
  blueprint_geodes ~blueprint ~time:24 |> printf "%d\n";
  [%expect "9"]

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
 *)

let%expect_test _ =
  let blueprint = List.hd_exn test_input in
  blueprint_geodes ~blueprint ~time:32 |> printf "%d\n";
  [%expect "62"]

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