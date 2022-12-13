open Core

module Packet = struct
  type t = List of t list | Int of int

  let rec compare left right =
    match (left, right) with
    | Int left, Int right -> Int.compare left right
    | List left, List right -> List.compare compare left right
    | Int left, List right -> compare (List [ Int left ]) (List right)
    | List left, Int right -> compare (List left) (List [ Int right ])

  let rec to_string = function
    | List list ->
        "[" ^ (List.map list ~f:to_string |> String.concat ~sep:",") ^ "]"
    | Int x -> Int.to_string x
end

let parse_input input =
  let rec take_packet input =
    match String.is_prefix ~prefix:"[" input with
    | true ->
        let rec helper input =
          match String.is_prefix ~prefix:"]" input with
          | true -> ([], String.chop_prefix_exn ~prefix:"]" input)
          | false ->
              let packet, rest = take_packet input in
              let rest = String.chop_prefix_if_exists ~prefix:"," rest in
              let rest_of_packet, rest = helper rest in
              (packet :: rest_of_packet, rest)
        in
        let list, rest = helper (String.chop_prefix_exn input ~prefix:"[") in
        (Packet.List list, rest)
    | false ->
        let digits = String.take_while ~f:Char.is_digit input in
        (Int (Int.of_string digits), String.chop_prefix_exn input ~prefix:digits)
  in
  String.split_lines input
  |> List.filter ~f:(Fn.non String.is_empty)
  |> List.map ~f:(fun line ->
         match take_packet line with
         | packet, "" -> packet
         | _ -> failwith "failed to parse entire input line")
  |> List.chunks_of ~length:2
  |> List.map ~f:(fun chunk ->
         match chunk with
         | [ left; right ] -> (left, right)
         | _ -> failwith "chunk wrong size")

let%expect_test _ =
  let input = In_channel.read_all "day_13_input.txt" in
  let round_trip_input =
    parse_input input
    |> List.map ~f:(fun (left, right) ->
           Packet.to_string left ^ "\n" ^ Packet.to_string right)
    |> String.concat ~sep:"\n\n"
  in
  printf "%b" (String.equal input round_trip_input);
  [%expect "true"]

let part_a =
  List.foldi ~init:0 ~f:(fun index sum (left, right) ->
      match Packet.compare left right with -1 -> sum + (index + 1) | _ -> sum)

let%expect_test _ =
  In_channel.read_all "day_13_input_test.txt"
  |> parse_input |> part_a |> printf "%d\n";
  [%expect "13"]

let%expect_test _ =
  In_channel.read_all "day_13_input.txt"
  |> parse_input |> part_a |> printf "%d\n";
  [%expect "6420"]

let part_b packet_pairs =
  let divider1, divider2 =
    ( Packet.List [ Packet.List [ Packet.Int 2 ] ],
      Packet.List [ Packet.List [ Packet.Int 6 ] ] )
  in
  let packets =
    List.concat_map ~f:(fun (left, right) -> [ left; right ]) packet_pairs
    @ [ divider1; divider2 ]
  in
  let sorted_packets = List.sort packets ~compare:Packet.compare in
  let index1 =
    List.find_mapi_exn
      ~f:(fun idx packet ->
        Option.some_if (Packet.compare packet divider1 = 0) idx)
      sorted_packets
    + 1
  in
  let index2 =
    List.find_mapi_exn
      ~f:(fun idx packet ->
        Option.some_if (Packet.compare packet divider2 = 0) idx)
      sorted_packets
    + 1
  in
  index1 * index2

let%expect_test _ =
  In_channel.read_all "day_13_input_test.txt"
  |> parse_input |> part_b |> printf "%d\n";
  [%expect "140"]

let%expect_test _ =
  In_channel.read_all "day_13_input.txt"
  |> parse_input |> part_b |> printf "%d\n";
  [%expect "22000"]