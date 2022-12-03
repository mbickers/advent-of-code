open Core
open Async

let ( % ) f g x = f (g x)
let ( >> ) f g x = g (f x)

let priority = function
  | 'a' .. 'z' as c -> Char.to_int c - Char.to_int 'a' + 1
  | 'A' .. 'Z' as c -> Char.to_int c - Char.to_int 'A' + 27
  | _ -> failwith "invalid character"

let common_char lists =
  List.map lists ~f:(Fn.compose Char.Set.of_list String.to_list)
  |> List.reduce_exn ~f:Set.inter
  |> Set.choose_exn

let part_a () =
  let split line =
    let middle = String.length line / 2 in
    [ String.prefix line middle; String.drop_prefix line middle ]
  in
  let%map priorities =
    Reader.with_file "day_3_input.txt"
      ~f:
        (Reader.lines
        >> Pipe.map ~f:(priority % common_char % split)
        >> Pipe.fold_without_pushback ~init:0 ~f:( + ))
  in
  print_endline ("a: " ^ Int.to_string priorities)

let part_b () =
  let chunks_of ~length =
    Pipe.folding_filter_map ?max_queue_length:None ~init:[] ~f:(fun chunk x ->
        match List.length chunk = length - 1 with
        | true -> ([], Some (x :: chunk))
        | false -> (x :: chunk, None))
  in
  let%map priorities =
    Reader.with_file "day_3_input.txt"
      ~f:
        (Reader.lines >> chunks_of ~length:3
        >> Pipe.map ~f:(priority % common_char)
        >> Pipe.fold_without_pushback ~init:0 ~f:( + ))
  in
  print_endline ("b: " ^ Int.to_string priorities)

let () =
  Command.async ~summary:"none"
    (Command.Param.return (fun () ->
         let%bind () = part_a () in
         part_b ()))
  |> Command.run
