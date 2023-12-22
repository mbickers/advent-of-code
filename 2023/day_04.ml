open Core

let ( >> ) f g x = g (f x)

let example =
  {|Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11|}
;;

let parse_cards =
  let open Angstrom in
  let padded_number =
    Int.of_string <$> take_while Char.is_whitespace *> take_while1 Char.is_digit
  in
  let padded_list = sep_by (char ' ') padded_number in
  let card =
    string "Card " *> padded_number *> string ": " *> padded_list
    >>= fun winning -> string " | " *> padded_list >>= fun have -> return (winning, have)
  in
  parse_string ~consume:Prefix (sep_by (char '\n') card) >> Result.ok_or_failwith
;;

let part_a =
  let card_value (winning, have) =
    match List.count have ~f:(List.mem winning ~equal) with
    | 0 -> 0
    | matches -> 1 lsl (matches - 1)
  in
  parse_cards >> List.sum (module Int) ~f:card_value
;;

let%expect_test _ =
  example |> part_a |> [%sexp_of: int] |> print_s;
  [%expect "13"]
;;

let%expect_test _ =
  In_channel.read_all "day_04_input.txt" |> part_a |> [%sexp_of: int] |> print_s;
  [%expect "27454"]
;;

let part_b =
  parse_cards
  >> List.rev
  >> List.fold ~init:[] ~f:(fun previous_copies (winning, have) ->
    let matches = List.count have ~f:(List.mem winning ~equal) in
    let copies =
      (List.take previous_copies matches |> List.sum (module Int) ~f:Fn.id) + matches
    in
    copies :: previous_copies)
  >> List.sum (module Int) ~f:(fun copies -> copies + 1)
;;

let%expect_test _ =
  example |> part_b |> [%sexp_of: int] |> print_s;
  [%expect "30"]
;;

let%expect_test _ =
  In_channel.read_all "day_04_input.txt" |> part_b |> [%sexp_of: int] |> print_s;
  [%expect "6857330"]
;;
