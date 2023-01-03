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

let memoize (type arg) (module M : Comparable with type t = arg) f =
  let memo = ref M.Map.empty in
  let rec g arg =
    match Map.find !memo arg with
    | Some value -> value
    | None ->
        let value = f ~self:g arg in
        memo := Map.set !memo ~key:arg ~data:value;
        value
  in
  g

let part_a valves =
  let module State = struct
    module T = struct
      type t = String.Set.t * String.t * int [@@deriving compare, sexp]
    end

    include T
    include Comparable.Make (T)
  end in
  let solve ~self:solve (open_valves, location, current_time) =
    let pressure_this_turn =
      Set.sum
        (module Int)
        open_valves
        ~f:(fun valve ->
          let flow_rate, _ = Map.find_exn valves valve in
          flow_rate)
    in
    let time_left = 30 - current_time in
    match time_left = 0 with
    | true -> pressure_this_turn
    | false ->
        let move_scores =
          let _, possible_locations = Map.find_exn valves location in
          List.map possible_locations ~f:(fun location ->
              solve (open_valves, location, current_time + 1))
        in
        let stay_score = time_left * pressure_this_turn in
        let flow_rate_here, _ = Map.find_exn valves location in
        let open_score =
          match Set.mem open_valves location || flow_rate_here = 0 with
          | true -> stay_score
          | false ->
              solve (Set.add open_valves location, location, current_time + 1)
        in
        let score =
          List.max_elt ~compare:Int.compare
            (stay_score :: open_score :: move_scores)
          |> Option.value_exn
        in
        pressure_this_turn + score
  in
  let solve = memoize (module State) solve in
  solve (String.Set.empty, "AA", 1)

let%expect_test _ =
  "day_16_input_test.txt" |> In_channel.read_all |> parse |> part_a
  |> printf "%d\n";
  [%expect "1651"]

let%expect_test _ =
  "day_16_input.txt" |> In_channel.read_all |> parse |> part_a |> printf "%d\n";
  [%expect "1716"]

(* let part_b valves =
   let useful_valves =
     Map.filter valves ~f:(fun (flow_rate, _) -> Int.is_positive flow_rate)
     |> Map.keys
   in
   let useful_valve_subsets =
     List.init
       (1 lsl List.length useful_valves)
       ~f:(fun set_idx ->
         List.filteri useful_valves ~f:(fun idx _ ->
             Int.is_positive (Int.bit_and set_idx (1 lsl idx)))
         |> String.Set.of_list)
   in
   let flow_rate open_valves =
     Set.sum
       (module Int)
       open_valves
       ~f:(fun valve ->
         let flow_rate, _ = Map.find_exn valves valve in
         flow_rate)
   in
   let module State = struct
     module T = struct
       type t = String.Set.t * (String.t * String.t) [@@deriving compare, sexp]
     end

     include T
     include Comparable.Make (T)
   end in
   let rec scores ~t =
     match t with
     | 0 ->
         let v =
           List.cartesian_product useful_valve_subsets
             (List.cartesian_product (Map.keys valves) (Map.keys valves))
           |> List.map ~f:(fun state -> (state, 0))
         in
         State.Map.of_alist_exn v
     | t ->
         let prev = scores ~t:(t - 1) in
         printf "Layer %d\n" t;
         Map.mapi prev
           ~f:(fun ~key:(open_valves, (location1, location2)) ~data:_ ->
             let actions location =
               let move_actions =
                 List.map
                   (let _, tunnels = Map.find_exn valves location in
                    tunnels)
                   ~f:(fun location -> `Move location)
               in
               match
                 (not (Set.mem open_valves location))
                 && List.mem useful_valves location ~equal:String.equal
               with
               | true -> `Open location :: move_actions
               | false -> move_actions
             in
             let prev_scores =
               List.cartesian_product (actions location1) (actions location2)
               |> List.map ~f:(fun (action1, action2) ->
                      let open_valves =
                        match action1 with
                        | `Open location -> Set.add open_valves location
                        | _ -> open_valves
                      in
                      let open_valves =
                        match action2 with
                        | `Open location -> Set.add open_valves location
                        | _ -> open_valves
                      in
                      let locations =
                        ( (match action1 with
                          | `Open location | `Move location -> location),
                          match action2 with
                          | `Open location | `Move location -> location )
                      in
                      Map.find_exn prev (open_valves, locations))
             in
             flow_rate open_valves
             + (List.max_elt prev_scores ~compare:Int.compare |> Option.value_exn))
   in
   let begin_scores = scores ~t:26 in
   Map.find_exn begin_scores (String.Set.empty, ("AA", "AA")) *)

let part_b valves =
  let useful_valves =
    Map.filter valves ~f:(fun (flow_rate, _) -> Int.is_positive flow_rate)
    |> Map.keys
  in
  let flow_rate open_valves =
    Set.sum
      (module Int)
      open_valves
      ~f:(fun valve ->
        let flow_rate, _ = Map.find_exn valves valve in
        flow_rate)
  in
  let module State = struct
    module T = struct
      type t = String.Set.t * String.t * String.t * int
      [@@deriving compare, sexp]
    end

    include T
    include Comparable.Make (T)
  end in
  let actions ~open_valves ~location =
    let move_actions =
      List.map
        (let _, tunnels = Map.find_exn valves location in
         tunnels)
        ~f:(fun location -> `Move location)
    in
    match
      (not (Set.mem open_valves location))
      && List.mem useful_valves location ~equal:String.equal
    with
    | true -> `Open location :: move_actions
    | false -> move_actions
  in
  let state ~open_valves ~actions:(action1, action2) ~t =
    let open_valves =
      match action1 with
      | `Open location -> Set.add open_valves location
      | _ -> open_valves
    in
    let open_valves =
      match action2 with
      | `Open location -> Set.add open_valves location
      | _ -> open_valves
    in
    let location1 =
      match action1 with `Open location | `Move location -> location
    in
    let location2 =
      match action2 with `Open location | `Move location -> location
    in
    (open_valves, location1, location2, t)
  in
  let solve ~self:solve (open_valves, location1, location2, current_time) =
    let time_left = 26 - current_time in
    match Set.length open_valves = List.length useful_valves with
    | true -> flow_rate open_valves * (time_left + 1)
    | false -> (
        match time_left = 0 with
        | true -> flow_rate open_valves
        | false ->
            let next_states =
              List.cartesian_product
                (actions ~open_valves ~location:location1)
                (actions ~open_valves ~location:location2)
              |> List.map ~f:(fun actions ->
                     state ~open_valves ~actions ~t:(current_time + 1))
            in
            let scores = List.map next_states ~f:solve in
            let best_score =
              List.max_elt scores ~compare:Int.compare |> Option.value_exn
            in
            flow_rate open_valves + best_score)
  in
  let solve = memoize (module State) solve in
  solve (String.Set.empty, "AA", "AA", 1)

let%expect_test _ =
  "day_16_input_test.txt" |> In_channel.read_all |> parse |> part_b
  |> printf "%d\n";
  [%expect "1707"]

let%expect_test _ =
  "day_16_input.txt" |> In_channel.read_all |> parse |> part_b |> printf "%d\n";
  [%expect "1707"]
