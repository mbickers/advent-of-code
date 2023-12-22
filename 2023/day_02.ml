open Core

let ( >> ) f g x = g (f x)

let parse_games =
  let open Angstrom in
  let number = Int.of_string <$> take_while1 Char.is_digit in
  let whitespace = take_while1 Char.is_whitespace in
  let game =
    let subset =
      sep_by
        (string ", ")
        (number
         >>= fun count ->
         whitespace
         *> (string "red" *> return (`red count)
             <|> string "green" *> return (`green count)
             <|> string "blue" *> return (`blue count)))
    in
    string "Game " *> number
    >>= fun id ->
    string ": " *> sep_by (string "; ") subset >>= fun subsets -> return (id, subsets)
  in
  parse_string ~consume:All (sep_by whitespace game) >> Result.ok_or_failwith
;;

let part_a =
  let possible_game (_, subsets) =
    List.for_all
      subsets
      ~f:
        (List.for_all ~f:(function
          | `red count -> count <= 12
          | `green count -> count <= 13
          | `blue count -> count <= 14))
  in
  parse_games
  >> List.filter ~f:possible_game
  >> List.sum (module Int) ~f:(fun (id, _) -> id)
;;

let%expect_test _ =
  {|Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
  Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
  Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
  Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
  Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green|}
  |> part_a
  |> [%sexp_of: int]
  |> print_s;
  [%expect "8"]
;;

let%expect_test _ =
  In_channel.read_all "day_02_input.txt" |> part_a |> [%sexp_of: int] |> print_s;
  [%expect "2285"]
;;

let part_b =
  let power =
    (fun (_, subsets) -> subsets)
    >> List.concat
    >> List.fold
         ~init:(`red 0, `green 0, `blue 0)
         ~f:(fun (`red red, `green green, `blue blue) drawing ->
           let new_red, new_green, new_blue =
             match drawing with
             | `red red -> red, 0, 0
             | `green green -> 0, green, 0
             | `blue blue -> 0, 0, blue
           in
           ( `red (Int.max red new_red)
           , `green (Int.max green new_green)
           , `blue (Int.max blue new_blue) ))
    >> fun (`red red, `green green, `blue blue) -> red * green * blue
  in
  parse_games >> List.sum (module Int) ~f:power
;;

let%expect_test _ =
  {|Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
  Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
  Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
  Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
  Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green|}
  |> part_b
  |> [%sexp_of: int]
  |> print_s;
  [%expect "2286"]
;;

let%expect_test _ =
  In_channel.read_all "day_02_input.txt" |> part_b |> [%sexp_of: int] |> print_s;
  [%expect "77021"]
;;
