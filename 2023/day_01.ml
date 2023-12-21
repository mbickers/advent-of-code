open Core

let ( >> ) f g x = g (f x)

let part_a =
  String.split_lines
  >> List.sum
       (module Int)
       ~f:
         (String.filter ~f:Char.is_digit
          >> (fun string -> String.prefix string 1 ^ String.suffix string 1)
          >> Int.of_string)
;;

let%expect_test _ =
  {|1abc2
  pqr3stu8vwx
  a1b2c3d4e5f
  treb7uchet|} |> part_a |> printf "%d\n";
  [%expect "142"]
;;

let%expect_test _ =
  In_channel.read_all "day_01_input.txt" |> part_a |> printf "%d\n";
  [%expect "55816"]
;;

let part_b =
  let spelled_digits =
    [ "one"; "two"; "three"; "four"; "five"; "six"; "seven"; "eight"; "nine" ]
  in
  let first_pattern_value ~patterns_and_values line =
    let _, value =
      List.filter_map patterns_and_values ~f:(fun (pattern, value) ->
        String.substr_index line ~pattern |> Option.map ~f:(fun index -> index, value))
      |> List.min_elt ~compare:(fun (index0, _) (index1, _) -> Int.compare index0 index1)
      |> Option.value_exn
    in
    value
  in
  let patterns_and_values =
    List.mapi spelled_digits ~f:(fun i word -> word, i + 1)
    @ List.init 9 ~f:(fun digit -> Int.to_string (digit + 1), digit + 1)
  in
  let rev_patterns_and_values =
    List.map patterns_and_values ~f:(fun (pattern, value) -> String.rev pattern, value)
  in
  let number_from_line line =
    let first_digit = first_pattern_value ~patterns_and_values line in
    let last_digit =
      first_pattern_value ~patterns_and_values:rev_patterns_and_values (String.rev line)
    in
    (first_digit * 10) + last_digit
  in
  String.split_lines >> List.sum (module Int) ~f:number_from_line
;;

let%expect_test _ =
  {|two1nine
  eightwothree
  abcone2threexyz
  xtwone3four
  4nineeightseven2
  zoneight234
  7pqrstsixteen|}
  |> part_b
  |> printf "%d\n";
  [%expect "281"]
;;

let%expect_test _ =
  In_channel.read_all "day_01_input.txt" |> part_b |> printf "%d\n";
  [%expect "54980"]
;;
