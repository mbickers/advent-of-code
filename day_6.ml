open Core
open List.Let_syntax

let position_of_k_distinct ~k =
  String.fold_until ~init:([], 1)
    ~f:(fun (previous_chunk, current_index) current_char ->
      let possible_match = current_char :: previous_chunk in
      match List.contains_dup ~compare:Char.compare possible_match with
      | true -> Stop current_index
      | false -> Continue (List.take possible_match (k - 1), current_index + 1))
    ~finish:(fun _ -> failwith "failed to find distinct sequence")

let (_ : unit list) =
  let%map part, k = [ ('a', 4); ('b', 14) ] in
  In_channel.read_all "day_6_input.txt"
  |> position_of_k_distinct ~k |> printf "%c: %d\n" part
