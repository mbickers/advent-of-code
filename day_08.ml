open Core

let ( >> ) f g x = g (f x)

let heights =
  In_channel.read_lines "day_8_input.txt"
  |> List.map
       ~f:(String.to_list >> List.map ~f:(String.of_char >> Int.of_string))

let visibility =
  let max_height = 9 in
  List.folding_mapi ~init:Int.Map.empty ~f:(fun idx height_positions height ->
      let closest_blocking_idx =
        Map.fold_range_inclusive height_positions ~min:height ~max:max_height
          ~init:None ~f:(fun ~key:_ ~data:blocking_idx closest_blocking_idx ->
            Option.merge (Some blocking_idx) closest_blocking_idx ~f:Int.max)
      in
      let visibility =
        match closest_blocking_idx with
        | Some blocking_idx -> `Blocked_at_distance (idx - blocking_idx)
        | None -> `Border_at_distance idx
      in
      (Map.set height_positions ~key:height ~data:idx, visibility))

let visibilities =
  let direction_visibilities =
    [
      List.map ~f:visibility heights;
      List.map ~f:(List.rev >> visibility >> List.rev) heights;
      List.transpose_exn (List.map ~f:visibility (List.transpose_exn heights));
      List.transpose_exn
        (List.map
           ~f:(List.rev >> visibility >> List.rev)
           (List.transpose_exn heights));
    ]
  in
  direction_visibilities |> List.transpose_exn |> List.map ~f:List.transpose_exn

let part_a =
  List.join visibilities
  |> List.count
       ~f:
         (List.exists ~f:(function
           | `Border_at_distance _ -> true
           | `Blocked_at_distance _ -> false))

let%expect_test _ =
  part_a |> printf "a: %d\n";
  [%expect "a: 1849"]

let part_b =
  List.join visibilities
  |> List.map
       ~f:
         (List.fold ~init:1 ~f:(fun product visiblity ->
              match visiblity with
              | `Border_at_distance distance | `Blocked_at_distance distance ->
                  product * distance))
  |> List.reduce_exn ~f:Int.max

let%expect_test _ =
  part_b |> printf "b: %d\n";
  [%expect "b: 201600"]