open Core

let parse input =
  let parse_line line =
    Scanf.sscanf line
      "Valve %s has flow rate=%d; tunnel%_s lead%_s to valve%_s %s@\n"
      (fun valve flow_rate tunnels_list ->
        let tunnels =
          String.split ~on:',' tunnels_list |> List.map ~f:String.lstrip
        in
        (valve, (flow_rate, tunnels)))
  in
  String.split_lines input |> List.map ~f:parse_line |> String.Map.of_alist_exn

module Useful_valve_set = struct
  module T = struct
    type t = int [@@deriving compare, sexp]
  end

  include T

  let initial_setup ~all_valves =
    let useful_valve_list =
      Map.filter all_valves ~f:(fun (flow_rate, _) -> flow_rate > 0)
      |> Map.to_alist
    in
    let useful_valves =
      List.mapi useful_valve_list ~f:(fun index (valve, _) -> (valve, index))
      |> String.Map.of_alist_exn
    in
    let all_useful_valve_sets =
      List.init (1 lsl List.length useful_valve_list) ~f:Fun.id
    in
    let flow_rates =
      all_useful_valve_sets
      |> List.map ~f:(fun open_valves ->
             let total_flow =
               List.mapi useful_valve_list
                 ~f:(fun valve_id (_, (flow_rate, _)) ->
                   match Int.bit_and open_valves (1 lsl valve_id) <> 0 with
                   | true -> flow_rate
                   | false -> 0)
               |> List.reduce_exn ~f:( + )
             in
             (open_valves, total_flow))
      |> Int.Map.of_alist_exn
    in
    (useful_valves, all_useful_valve_sets, flow_rates)

  let empty = 0

  let contains ~useful_valves t valve =
    match Map.find useful_valves valve with
    | Some id -> Int.bit_and (1 lsl id) t <> 0
    | None -> false

  let minus ~useful_valves t valve =
    assert (contains ~useful_valves t valve);
    let id = Map.find_exn useful_valves valve in
    Int.bit_xor (1 lsl id) t

  let overlap t1 t2 = Int.bit_and t1 t2 <> 0

  include Comparable.Make (T)
end

module State = struct
  module T = struct
    type t = String.t * Useful_valve_set.t [@@deriving compare, sexp]
  end

  include T
  include Comparable.Make (T)
end

let best_scores ~all_valves ~useful_valves ~all_useful_valve_sets ~flow_rates =
  let rec best_scores ~time =
    match time with
    | 0 ->
        let all_states =
          List.cartesian_product (Map.keys all_valves) all_useful_valve_sets
        in
        let all_state_nones =
          State.Map.of_alist_exn
            (List.map all_states ~f:(fun state -> (state, None)))
        in
        Map.set all_state_nones
          ~key:("AA", Useful_valve_set.empty)
          ~data:(Some 0)
    | time ->
        let previous_bests = best_scores ~time:(time - 1) in
        Map.mapi previous_bests
          ~f:(fun ~key:(position, open_valves) ~data:score ->
            let stay_in_place =
              Option.map score ~f:(fun score ->
                  score + Map.find_exn flow_rates open_valves)
            in
            let open_in_place =
              match
                Useful_valve_set.contains ~useful_valves open_valves position
              with
              | false -> None
              | true ->
                  let rest =
                    Useful_valve_set.minus ~useful_valves open_valves position
                  in
                  Option.map
                    (Map.find_exn previous_bests (position, rest))
                    ~f:(fun score -> score + Map.find_exn flow_rates rest)
            in
            let move_from_neighbor =
              let _, neighbors = Map.find_exn all_valves position in
              List.map neighbors ~f:(fun neighbor ->
                  Map.find_exn previous_bests (neighbor, open_valves)
                  |> Option.map ~f:(fun score ->
                         score + Map.find_exn flow_rates open_valves))
            in
            let possible_pred_states =
              stay_in_place :: open_in_place :: move_from_neighbor
            in
            List.max_elt
              ~compare:(Option.compare Int.compare)
              possible_pred_states
            |> Option.value_exn)
  in
  best_scores

let part_a all_valves =
  let useful_valves, all_useful_valve_sets, flow_rates =
    Useful_valve_set.initial_setup ~all_valves
  in
  let end_scores =
    best_scores ~all_valves ~useful_valves ~all_useful_valve_sets ~flow_rates
      ~time:30
  in
  Map.to_alist end_scores
  |> List.filter_map ~f:(fun (_, score) -> score)
  |> List.max_elt ~compare:Int.compare
  |> Option.value_exn

let%expect_test _ =
  "day_16_input_test.txt" |> In_channel.read_all |> parse |> part_a
  |> printf "%d\n";
  [%expect "1651"]

let%expect_test _ =
  "day_16_input.txt" |> In_channel.read_all |> parse |> part_a |> printf "%d\n";
  [%expect "1716"]

let part_b all_valves =
  let useful_valves, all_useful_valve_sets, flow_rates =
    Useful_valve_set.initial_setup ~all_valves
  in
  let end_scores =
    best_scores ~all_valves ~useful_valves ~all_useful_valve_sets ~flow_rates
      ~time:26
  in
  let end_scores =
    end_scores |> Map.to_alist
    |> List.filter_map ~f:(fun ((_, open_valves), score) ->
           Option.map score ~f:(fun score -> (open_valves, score)))
    |> Useful_valve_set.Map.of_alist_reduce ~f:Int.max
    |> Map.to_alist
  in
  List.cartesian_product end_scores end_scores
  |> List.filter_map ~f:(fun ((open_valves1, score1), (open_valves2, score2)) ->
         Option.some_if
           (not (Useful_valve_set.overlap open_valves1 open_valves2))
           (score1 + score2))
  |> List.max_elt ~compare:Int.compare
  |> Option.value_exn

let%expect_test _ =
  "day_16_input_test.txt" |> In_channel.read_all |> parse |> part_b
  |> printf "%d\n";
  [%expect "1707"]

let%expect_test _ =
  "day_16_input.txt" |> In_channel.read_all |> parse |> part_b |> printf "%d\n";
  [%expect "2504"]