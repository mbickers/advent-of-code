open Core

let ( >> ) f g x = g (f x)

let example = {|Time:      7  15   30
Distance:  9  40  200|}

let parse_document_a =
  let open Angstrom in
  let padded_number =
    take_while Char.is_whitespace *> take_while1 Char.is_digit >>| Int.of_string
  in
  let document =
    string "Time:" *> many padded_number
    >>= fun times ->
    string "\nDistance:" *> many padded_number
    >>= fun distances ->
    return
      (List.map2_exn times distances ~f:(fun time distance ->
         `time time, `distance distance))
  in
  parse_string ~consume:All document >> Result.ok_or_failwith
;;

let number_of_ways_to_beat_record (`time time, `distance distance) =
  let distance = Float.of_int distance in
  let time = Float.of_int time in
  let min_charge_time =
    Float.(
      (time / 2.) - (Float.sqrt ((time * time) - (4. * distance)) / 2.)
      |> one_ulp `Up
      |> round_up
      |> to_int)
  in
  let max_charge_time =
    Float.(
      (time / 2.) + (Float.sqrt ((time * time) - (4. * distance)) / 2.)
      |> one_ulp `Down
      |> round_down
      |> to_int)
  in
  max_charge_time - min_charge_time + 1
;;

let part_a =
  parse_document_a
  >> List.fold ~init:1 ~f:(fun acc -> number_of_ways_to_beat_record >> ( * ) acc)
;;

let%expect_test _ =
  example |> part_a |> [%sexp_of: int] |> print_s;
  [%expect "288"]
;;

let%expect_test _ =
  In_channel.read_all "day_06_input.txt" |> part_a |> [%sexp_of: int] |> print_s;
  [%expect "1624896"]
;;

let parse_document_b =
  let open Angstrom in
  let poorly_kerned_number =
    many (take_while1 Char.is_whitespace *> take_while1 Char.is_digit)
    >>| (String.concat >> Int.of_string)
  in
  let document =
    string "Time:" *> poorly_kerned_number
    >>= fun time ->
    string "\nDistance:" *> poorly_kerned_number
    >>= fun distance -> return (`time time, `distance distance)
  in
  parse_string ~consume:All document >> Result.ok_or_failwith
;;

let part_b = parse_document_b >> number_of_ways_to_beat_record

let%expect_test _ =
  example |> part_b |> [%sexp_of: int] |> print_s;
  [%expect "71503"]
;;

let%expect_test _ =
  In_channel.read_all "day_06_input.txt" |> part_b |> [%sexp_of: int] |> print_s;
  [%expect "32583852"]
;;
