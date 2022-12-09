open Core

module Shape = struct
  type t = Rock | Paper | Scissors [@@deriving equal]

  let value = function Rock -> 1 | Paper -> 2 | Scissors -> 3
  let beats = function Rock -> Scissors | Paper -> Rock | Scissors -> Paper
  let beaten_by = Fn.compose beats beats

  let outcome ~me ~opponent =
    match (equal (beats me) opponent, equal (beaten_by me) opponent) with
    | true, _ -> `Win
    | _, true -> `Loss
    | false, false -> `Draw
end

let part_a file =
  In_channel.fold_lines file ~init:0 ~f:(fun score line ->
      let opponent =
        match String.get line 0 with
        | 'A' -> Shape.Rock
        | 'B' -> Paper
        | 'C' -> Scissors
        | _ -> assert false
      in
      let me =
        match String.get line 2 with
        | 'X' -> Shape.Rock
        | 'Y' -> Paper
        | 'Z' -> Scissors
        | _ -> assert false
      in
      let round_outcome_points =
        match Shape.outcome ~me ~opponent with
        | `Loss -> 0
        | `Draw -> 3
        | `Win -> 6
      in
      score + round_outcome_points + Shape.value me)

let%expect_test _ =
  In_channel.with_file "day_2_input.txt" ~f:part_a |> printf "%d\n";
  [%expect "14827"]

let part_b file =
  In_channel.fold_lines file ~init:0 ~f:(fun score line ->
      let opponent =
        match String.get line 0 with
        | 'A' -> Shape.Rock
        | 'B' -> Paper
        | 'C' -> Scissors
        | _ -> assert false
      in
      let round_outcome =
        match String.get line 2 with
        | 'X' -> `Loss
        | 'Y' -> `Draw
        | 'Z' -> `Win
        | _ -> assert false
      in
      let me =
        match round_outcome with
        | `Loss -> Shape.beats opponent
        | `Draw -> opponent
        | `Win -> Shape.beaten_by opponent
      in
      let round_outcome_points =
        match round_outcome with `Loss -> 0 | `Draw -> 3 | `Win -> 6
      in
      score + round_outcome_points + Shape.value me)

let%expect_test _ =
  In_channel.with_file "day_2_input.txt" ~f:part_b |> printf "%d\n";
  [%expect "13889"]