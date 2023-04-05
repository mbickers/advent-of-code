open Core

module Resource = struct
  type t = Ore | Clay | Obsidian | Geode [@@deriving compare]
end

module Blueprint = struct
  type t = {
    id : int;
    robot_costs : (Resource.t, (Resource.t, int) List.Assoc.t) List.Assoc.t;
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
            (Resource.Ore, [ (Resource.Ore, ore_ore) ]);
            (Clay, [ (Ore, clay_ore) ]);
            (Obsidian, [ (Ore, obsidian_ore); (Clay, obsidian_clay) ]);
            (Geode, [ (Ore, geode_ore); (Obsidian, geode_obsidian) ]);
          ]
        in
        { Blueprint.id; robot_costs })
  in
  String.split ~on:'B' input |> List.tl_exn |> List.map ~f:parse_blueprint

let input = In_channel.read_all "day_19_input.txt" |> parse_input
let test_input = In_channel.read_all "day_19_input_test.txt" |> parse_input

let%expect_test _ = [%expect "test"]