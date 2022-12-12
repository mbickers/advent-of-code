open Core
open Async

let ( % ) f g x = f (g x)
let ( >> ) f g x = g (f x)

let priority = function
  | 'a' .. 'z' as c -> Char.to_int c - Char.to_int 'a' + 1
  | 'A' .. 'Z' as c -> Char.to_int c - Char.to_int 'A' + 27
  | _ -> failwith "invalid character"

let common_char =
  List.map ~f:(String.to_list >> Char.Set.of_list)
  >> List.reduce_exn ~f:Set.inter
  >> Set.choose_exn

let part_a reader =
  let split line =
    let middle = String.length line / 2 in
    [ String.prefix line middle; String.drop_prefix line middle ]
  in
  reader |> Reader.lines
  |> Pipe.map ~f:(split >> common_char >> priority)
  |> Pipe.fold_without_pushback ~init:0 ~f:( + )

let%expect_test _ =
  let%bind result = Reader.with_file "day_3_input.txt" ~f:part_a in
  printf "%d\n" result;
  [%expect "7701"]

let part_b reader =
  let chunks_of ~length =
    Pipe.folding_filter_map ?max_queue_length:None ~init:[] ~f:(fun chunk x ->
        match List.length chunk = length - 1 with
        | true -> ([], Some (x :: chunk))
        | false -> (x :: chunk, None))
  in
  reader |> Reader.lines |> chunks_of ~length:3
  |> Pipe.map ~f:(common_char >> priority)
  |> Pipe.fold_without_pushback ~init:0 ~f:( + )

let%expect_test _ =
  let%bind result = Reader.with_file "day_3_input.txt" ~f:part_a in
  printf "%d\n" result;
  [%expect "7701"]