open Core

let ( >> ) f g x = g (f x)

module Monkey = struct
  type t = { operation : int -> int; throws_to : int -> int; divisor : int }
end

let scan_monkey monkey =
  Scanf.sscanf monkey
    "Monkey %_d:\n\
    \  Starting items: %s@\n\
    \  Operation: new = %s %s %s\n\
    \  Test: divisible by %d\n\
    \    If true: throw to monkey %d\n\
    \    If false: throw to monkey %d"
    (fun
      starting_items
      operand1
      operator
      operand2
      divisor
      true_monkey
      false_monkey
    ->
      let starting_items =
        starting_items |> String.split ~on:','
        |> List.map ~f:(String.lstrip >> Int.of_string)
      in
      let operation =
        let operator =
          match operator with
          | "+" -> ( + )
          | "*" -> ( * )
          | _ -> failwith "unrecognized operator"
        in
        let operand1, operand2 =
          Tuple2.map (operand1, operand2) ~f:(function
            | "old" -> Fn.id
            | d ->
                let d = Int.of_string d in
                fun _ -> d)
        in
        fun old -> operator (operand1 old) (operand2 old)
      in
      let throws_to worry_level =
        match worry_level % divisor = 0 with
        | true -> true_monkey
        | false -> false_monkey
      in
      (starting_items, { Monkey.operation; throws_to; divisor }))

let%expect_test _ =
  let monkey =
    {|Monkey 0:
  Starting items: 56, 52, 58, 96, 70, 75, 72
  Operation: new = old * 17
  Test: divisible by 11
    If true: throw to monkey 2
    If false: throw to monkey 3|}
  in
  let starting_items, monkey = scan_monkey monkey in
  print_endline (Int.to_string (List.hd_exn starting_items));
  [%expect "56"];
  print_endline (Int.to_string (monkey.operation 2));
  [%expect "34"];
  print_endline (Int.to_string (monkey.throws_to 22));
  [%expect "2"]

let read_input filename =
  In_channel.read_lines filename
  |> List.chunks_of ~length:7
  |> List.map
       ~f:
         (List.filter ~f:(Fn.non String.is_empty)
         >> List.intersperse ~sep:"\n" >> List.reduce_exn ~f:( ^ )
         >> scan_monkey)
  |> List.unzip

let calculate_monkey_business ~rounds ~transform_after_inspection
    (initial_monkey_items, monkeys) =
  let round init =
    List.foldi monkeys ~init
      ~f:(fun
           monkey_idx
           (monkey_items, inspection_times)
           { Monkey.operation; throws_to; _ }
         ->
        let monkey_holding = List.nth_exn monkey_items monkey_idx in
        let monkey_items_without_current_monkey =
          List.mapi monkey_items ~f:(fun idx stack ->
              match idx = monkey_idx with true -> [] | false -> stack)
        in
        let updated_monkey_items =
          List.fold monkey_holding ~init:monkey_items_without_current_monkey
            ~f:(fun state item_worry_level ->
              let item_worry_level =
                operation item_worry_level |> transform_after_inspection
              in
              List.mapi state ~f:(fun idx stack ->
                  match idx = throws_to item_worry_level with
                  | true -> stack @ [ item_worry_level ]
                  | false -> stack))
        in
        let updated_inspection_times =
          List.mapi inspection_times ~f:(fun idx inspections ->
              match idx = monkey_idx with
              | true -> inspections + List.length monkey_holding
              | false -> inspections)
        in
        (updated_monkey_items, updated_inspection_times))
  in
  let initial_inspection_times = List.map monkeys ~f:(fun _ -> 0) in
  let _, inspection_times =
    Fn.apply_n_times ~n:rounds round
      (initial_monkey_items, initial_inspection_times)
  in
  match List.sort ~compare:Int.compare inspection_times |> List.rev with
  | inspections1 :: inspections2 :: _ -> inspections1 * inspections2
  | _ -> failwith "not enough inspection times"

let part_a =
  calculate_monkey_business ~rounds:20
    ~transform_after_inspection:(fun worry_level -> worry_level / 3)

let%expect_test _ =
  read_input "day_11_input_test.txt" |> part_a |> printf "%d\n";
  [%expect "10605"]

let%expect_test _ =
  read_input "day_11_input.txt" |> part_a |> printf "%d\n";
  [%expect "98280"]

let part_b (initial_state, monkeys) =
  let common_multiple =
    List.map monkeys ~f:(fun { Monkey.divisor; _ } -> divisor)
    |> List.reduce_exn ~f:( * )
  in
  calculate_monkey_business ~rounds:10000
    ~transform_after_inspection:(fun worry_level ->
      worry_level % common_multiple)
    (initial_state, monkeys)

let%expect_test _ =
  read_input "day_11_input_test.txt" |> part_b |> printf "%d\n";
  [%expect "2713310158"]

let%expect_test _ =
  read_input "day_11_input.txt" |> part_b |> printf "%d\n";
  [%expect "17673687232"]