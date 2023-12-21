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

let part_b input =
  let spelled_digits =
    [ "one"; "two"; "three"; "four"; "five"; "six"; "seven"; "eight"; "nine" ]
  in
  0
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
  |> printf "%s\n";
  [%expect "281"]
;;
