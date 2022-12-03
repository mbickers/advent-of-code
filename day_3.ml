open Core

let ( % ) = Fn.compose

let priority = function
  | 'a' .. 'z' as c -> Char.to_int c - Char.to_int 'a' + 1
  | 'A' .. 'Z' as c -> Char.to_int c - Char.to_int 'A' + 27
  | _ -> failwith "invalid character"

let common_char lists =
  List.map lists ~f:(Fn.compose Char.Set.of_list String.to_list)
  |> List.reduce_exn ~f:Set.inter
  |> Set.choose_exn

let () =
  let split line =
    let middle = String.length line / 2 in
    [ String.prefix line middle; String.drop_prefix line middle ]
  in
  let priorities =
    In_channel.read_lines "day_3_input.txt"
    |> List.sum (module Int) ~f:(priority % common_char % split)
  in
  print_endline ("a: " ^ Int.to_string priorities)

let () =
  let priorities =
    In_channel.read_lines "day_3_input.txt"
    |> List.chunks_of ~length:3
    |> List.sum (module Int) ~f:(priority % common_char)
  in
  print_endline ("b: " ^ Int.to_string priorities)