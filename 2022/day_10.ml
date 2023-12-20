open Core

let ( >> ) f g x = g (f x)

let parse line =
  match String.split ~on:' ' line with
  | [ "addx"; value ] -> `Addx (Int.of_string value)
  | [ "noop" ] -> `Noop
  | _ -> failwith "unable to parse input line"

let register_values instructions =
  List.folding_map ~init:1 instructions ~f:(fun register_value instruction ->
      match instruction with
      | `Addx value ->
          (register_value + value, [ register_value; register_value ])
      | `Noop -> (register_value, [ register_value ]))
  |> List.concat

let part_a instructions =
  let sample_at = List.init 6 ~f:(fun x -> 20 + (40 * x)) |> Int.Set.of_list in
  let signal_strengths_sum =
    List.foldi (register_values instructions) ~init:0
      ~f:(fun idx sum register_value ->
        let cycle = idx + 1 in
        match Set.mem sample_at cycle with
        | true -> sum + (cycle * register_value)
        | false -> sum)
  in
  signal_strengths_sum

let part_b instructions =
  let screen_width = 40 in
  let pixels =
    List.mapi (register_values instructions) ~f:(fun idx sprite_position ->
        let scanner_position = idx % screen_width in
        match abs (scanner_position - sprite_position) <= 1 with
        | true -> '#'
        | false -> '.')
  in
  List.chunks_of ~length:screen_width pixels
  |> List.map ~f:String.of_char_list
  |> List.intersperse ~sep:"\n" |> List.reduce_exn ~f:( ^ )

let%expect_test _ =
  In_channel.read_lines "day_10_test_input.txt"
  |> List.map ~f:parse |> part_a |> printf "%d\n";
  [%expect "13140"]

let%expect_test _ =
  In_channel.read_lines "day_10_input.txt"
  |> List.map ~f:parse |> part_a |> printf "%d\n";
  [%expect "14360"]

let%expect_test _ =
  In_channel.read_lines "day_10_test_input.txt"
  |> List.map ~f:parse |> part_b |> print_endline;
  [%expect
    {|
##..##..##..##..##..##..##..##..##..##..
###...###...###...###...###...###...###.
####....####....####....####....####....
#####.....#####.....#####.....#####.....
######......######......######......####
#######.......#######.......#######.....|}]

let%expect_test _ =
  In_channel.read_lines "day_10_input.txt"
  |> List.map ~f:parse |> part_b |> print_endline;
  [%expect
    {|
###...##..#..#..##..####.###..####.####.
#..#.#..#.#.#..#..#.#....#..#.#.......#.
###..#....##...#..#.###..#..#.###....#..
#..#.#.##.#.#..####.#....###..#.....#...
#..#.#..#.#.#..#..#.#....#.#..#....#....
###...###.#..#.#..#.####.#..#.####.####.|}]
