open Core

module Operation = struct
  type t = Addition | Subtraction | Multiplication | Division

  let operate operation =
    match operation with
    | Addition -> ( + )
    | Subtraction -> ( - )
    | Multiplication -> ( * )
    | Division -> ( / )
end

let parse_input input =
  String.split_lines input
  |> List.map ~f:(fun line ->
         match String.split_on_chars ~on:[ ' '; ':' ] line with
         | [ name; ""; dependency1; operation; dependency2 ] ->
             let operation =
               match String.get operation 0 with
               | '+' -> Operation.Addition
               | '-' -> Subtraction
               | '*' -> Multiplication
               | '/' -> Division
               | _ -> failwith "invalid operation"
             in
             (name, `operation (dependency1, operation, dependency2))
         | [ name; ""; number ] -> (name, `number (Int.of_string number))
         | _ -> failwith ("malformed line: " ^ line))

let test_input = In_channel.read_all "day_21_input_test.txt" |> parse_input
let input = In_channel.read_all "day_21_input.txt" |> parse_input

let part_a monkeys =
  let rec helper name =
    match
      List.find_map_exn monkeys ~f:(fun (monkey_name, action) ->
          Option.some_if (String.equal monkey_name name) action)
    with
    | `number n -> n
    | `operation (dependency1, operation, dependency2) ->
        Operation.operate operation (helper dependency1) (helper dependency2)
  in
  helper "root"

let%expect_test _ =
  test_input |> part_a |> printf "%d\n";
  [%expect "152"]

let%expect_test _ =
  input |> part_a |> printf "%d\n";
  [%expect "299983725663456"]

module Calculation_tree = struct
  type node = Human | Number of int | Operation of node * Operation.t * node
  type t = Equality of node * node
end

let part_b monkeys =
  let tree =
    let rec helper name =
      match
        List.find_exn monkeys ~f:(fun (monkey_name, _) ->
            String.equal monkey_name name)
      with
      | "humn", _ -> Calculation_tree.Human
      | _, `number n -> Number n
      | _, `operation (dependency1, operation, dependency2) ->
          Calculation_tree.Operation
            (helper dependency1, operation, helper dependency2)
    in
    match
      List.find_map_exn monkeys ~f:(fun (monkey_name, action) ->
          Option.some_if (String.equal monkey_name "root") action)
    with
    | `number _ -> failwith "invalid input"
    | `operation (dependency1, _, dependency2) ->
        Calculation_tree.Equality (helper dependency1, helper dependency2)
  in
  let (Equality (a, b)) = tree in
  let rec solve_some node =
    match node with
    | Calculation_tree.Human -> Calculation_tree.Human
    | Number n -> Number n
    | Operation (dependency1, operation, dependency2) -> (
        match (solve_some dependency1, solve_some dependency2) with
        | Number n1, Number n2 -> Number (Operation.operate operation n1 n2)
        | dependency1, dependency2 ->
            Operation (dependency1, operation, dependency2))
  in
  let a, b = (solve_some a, solve_some b) in
  let rec solve_rest (node1, node2) =
    match (node1, node2) with
    | Calculation_tree.Number n, Calculation_tree.Human
    | Calculation_tree.Human, Calculation_tree.Number n ->
        n
    | ( Calculation_tree.Number n,
        Calculation_tree.Operation (dependency1, operation, dependency2) )
    | ( Calculation_tree.Operation (dependency1, operation, dependency2),
        Calculation_tree.Number n ) -> (
        match (dependency1, operation, dependency2) with
        | Number x, Addition, rest | rest, Addition, Number x ->
            solve_rest (rest, Number (n - x))
        | Number x, Multiplication, rest | rest, Multiplication, Number x ->
            solve_rest (rest, Number (n / x))
        | Number x, Subtraction, rest -> solve_rest (rest, Number (x - n))
        | rest, Subtraction, Number x -> solve_rest (rest, Number (n + x))
        | Number x, Division, rest -> solve_rest (rest, Number (x / n))
        | rest, Division, Number x -> solve_rest (rest, Number (n * x))
        | _ -> failwith "hopefully unreachable")
    | _ -> failwith "should've solved this branch in solve_some"
  in
  solve_rest (a, b)

let%expect_test _ =
  test_input |> part_b |> printf "%d\n";
  [%expect "301"]

let%expect_test _ =
  input |> part_b |> printf "%d\n";
  [%expect "3093175982595"]