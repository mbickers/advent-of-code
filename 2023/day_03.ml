open Core

let ( >> ) f g x = g (f x)

type ('a, 'b) parser = 'a list -> ('b * 'a list) option

type schematic_parser =
  ( char * (int * int)
    , [ `number of int * (int * int) list | `symbol of char * (int * int) ] list )
    parser

let parse_digits : schematic_parser = function
  | ('0' .. '9', _) :: _ as input ->
    let digits_and_positions, rest =
      List.split_while ~f:(fun (character, _) -> Char.is_digit character) input
    in
    let digits, positions =
      List.fold
        ~init:([], [])
        ~f:(fun (digits, positions) (digit, position) ->
          digit :: digits, position :: positions)
        (List.rev digits_and_positions)
    in
    Some ([ `number (digits |> String.of_char_list |> Int.of_string, positions) ], rest)
  | _ -> None
;;

let parse_blank : schematic_parser = function
  | (('.' | '\n'), _) :: rest -> Some ([], rest)
  | _ -> None
;;

let parse_symbol : schematic_parser = function
  | (symbol, position) :: rest -> Some ([ `symbol (symbol, position) ], rest)
  | _ -> None
;;

let rec any_of parsers input =
  match parsers with
  | [] -> None
  | parser :: other_parsers ->
    (match parser input with
     | Some (result, rest) -> Some (result, rest)
     | None -> any_of other_parsers input)
;;

let repeated parser input =
  let rec helper input output =
    match parser input with
    | None -> List.rev output, input
    | Some (new_output, rest) -> helper rest (new_output @ output)
  in
  Some (helper input [])
;;

let parse_schematic : schematic_parser =
  repeated (any_of [ parse_digits; parse_blank; parse_symbol ])
;;

let lex_schematic =
  let append_newline s = s ^ "\n" in
  String.split_lines
  >> List.concat_mapi ~f:(fun row ->
    append_newline
    >> String.to_list
    >> List.concat_mapi ~f:(fun col c -> [ c, (row, col) ]))
;;

let process_input =
  lex_schematic
  >> parse_schematic
  >> function
  | Some (processed_schematic, []) ->
    List.partition_map processed_schematic ~f:(function
      | `number (number, positions) -> Either.first (number, positions)
      | `symbol (symbol, position) -> Either.second (symbol, position))
  | _ -> failwith "unable to process schematic"
;;

let adjacent (r1, c1) (r2, c2) = Int.abs (r1 - r2) <= 1 && Int.abs (c1 - c2) <= 1

let part_a input =
  let numbers, symbols = process_input input in
  let is_adjacent_to_symbol (_, positions) =
    List.exists positions ~f:(fun position ->
      List.exists symbols ~f:(fun (_, symbol_position) ->
        adjacent position symbol_position))
  in
  List.filter numbers ~f:is_adjacent_to_symbol
  |> List.sum (module Int) ~f:(fun (number, _) -> number)
;;

let example =
  {|467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..|}
;;

let%expect_test _ =
  example |> part_a |> [%sexp_of: int] |> print_s;
  [%expect "4361"]
;;

let%expect_test _ =
  In_channel.read_all "day_03_input.txt" |> part_a |> [%sexp_of: int] |> print_s;
  [%expect "556367"]
;;

let part_b input =
  let numbers, symbols = process_input input in
  let gear_ratio (symbol, position) =
    match symbol with
    | '*' ->
      let adjacent_numbers =
        List.filter numbers ~f:(fun (_, positions) ->
          List.exists positions ~f:(fun number_position ->
            adjacent position number_position))
      in
      (match adjacent_numbers with
       | [ (n1, _); (n2, _) ] -> Some (n1 * n2)
       | _ -> None)
    | _ -> None
  in
  List.sum (module Int) symbols ~f:(gear_ratio >> Option.value ~default:0)
;;

let%expect_test _ =
  example |> part_b |> [%sexp_of: int] |> print_s;
  [%expect "467835"]
;;

let%expect_test _ =
  In_channel.read_all "day_03_input.txt" |> part_b |> [%sexp_of: int] |> print_s;
  [%expect "89471771"]
;;
