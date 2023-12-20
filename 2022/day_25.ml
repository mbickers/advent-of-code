open Core

let ( >> ) f g x = g (f x)

module Snafu = struct
  module Digit = struct
    type t = Two | One | Zero | Minus | Double_minus

    let of_char = function
      | '2' -> Two
      | '1' -> One
      | '0' -> Zero
      | '-' -> Minus
      | '=' -> Double_minus
      | _ -> failwith "illegal character in snafu digit"

    let to_char = function
      | Two -> '2'
      | One -> '1'
      | Zero -> '0'
      | Minus -> '-'
      | Double_minus -> '='

    let to_int = function
      | Two -> 2
      | One -> 1
      | Zero -> 0
      | Minus -> -1
      | Double_minus -> -2

    let of_int = function
      | 2 -> Two
      | 1 -> One
      | 0 -> Zero
      | -1 -> Minus
      | -2 -> Double_minus
      | _ -> failwith "illegal value for snafu digit"
  end

  type t = Digit.t list

  let to_string : t -> string = List.map ~f:Digit.to_char >> String.of_char_list
  let of_string : string -> t = String.to_list >> List.map ~f:Digit.of_char

  let to_int : t -> int =
    List.fold ~init:0 ~f:(fun sum digit -> (5 * sum) + Digit.to_int digit)

  let of_int x : t =
    (* let place_value = Float.(to_int (log (abs (of_int x) / 3.) / log 5. + 1.)) in *)
    let rec helper (place_value, max_magnitude_with_place) =
      let preceding_digits, remainder =
        match abs x <= max_magnitude_with_place with
        | true -> ([], x)
        | false ->
            let new_place_value = 5 * place_value in
            helper
              (new_place_value, max_magnitude_with_place + (2 * new_place_value))
      in
      let digit_int =
        ((remainder + max_magnitude_with_place) / place_value) - 2
      in
      let new_remainder = remainder - (place_value * digit_int) in
      (preceding_digits @ [ Digit.of_int digit_int ], new_remainder)
    in
    let digits, _ = helper (1, 2) in
    digits
end

let test_input =
  In_channel.read_lines "day_25_input_test.txt" |> List.map ~f:Snafu.of_string

let input =
  In_channel.read_lines "day_25_input.txt" |> List.map ~f:Snafu.of_string

let part_a =
  List.sum (module Int) ~f:Snafu.to_int >> Snafu.(of_int >> Snafu.to_string)

let%expect_test _ =
  test_input |> part_a |> printf "%s\n";
  [%expect "2=-1=0"]

let%expect_test _ =
  input |> part_a |> printf "%s\n";
  [%expect "2==0=0===02--210---1"]
