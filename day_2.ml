open Core

module Game_outcome = struct
  type t = Left_wins | Right_wins | Draw
end

module Rock_paper_scissors = struct
  type t = Rock | Paper | Scissors

  let value = function Rock -> 1 | Paper -> 2 | Scissors -> 3

  let winner left right =
    match (left, right) with
    | Rock, Paper -> Game_outcome.Right_wins
    | Paper, Scissors -> Right_wins
    | Scissors, Rock -> Right_wins
    | Paper, Rock -> Left_wins
    | Scissors, Paper -> Left_wins
    | Rock, Scissors -> Left_wins
    | _, _ -> Draw
end

let () =
  let score =
    In_channel.with_file "day_2_input.txt"
      ~f:
        (In_channel.fold_lines ~init:0 ~f:(fun score line ->
             let opponent_choice =
               match String.get line 0 with
               | 'A' -> Rock_paper_scissors.Rock
               | 'B' -> Paper
               | 'C' -> Scissors
               | _ -> assert false
             in
             let my_choice =
               match String.get line 2 with
               | 'X' -> Rock_paper_scissors.Rock
               | 'Y' -> Paper
               | 'Z' -> Scissors
               | _ -> assert false
             in
             let round_outcome_points =
               match Rock_paper_scissors.winner opponent_choice my_choice with
               | Left_wins -> 0
               | Draw -> 3
               | Right_wins -> 6
             in
             let points =
               Rock_paper_scissors.value my_choice + round_outcome_points
             in
             points + score))
  in
  print_endline ("a: " ^ Int.to_string score ^ " points")