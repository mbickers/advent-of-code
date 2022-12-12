open Core
open Async

let ( >> ) f g x = g (f x)

let rearrange_result ~crane_transform =
  let parse line =
    String.split line ~on:' ' |> List.filter ~f:(Fn.non String.is_empty)
    |> function
    | hd :: _ when String.is_prefix ~prefix:"[" hd ->
        `Header
          (String.to_list line |> List.filteri ~f:(fun idx _ -> idx % 4 = 1))
    | "1" :: _ -> `Header_indices
    | [] -> `Empty
    | [ "move"; quantity; "from"; source; "to"; destination ] ->
        `Rearrange
          Int.(of_string quantity, of_string source, of_string destination)
    | _ -> failwith "unable to parse input line"
  in
  let update_state stacks = function
    | `Header stack_entries ->
        let stacks =
          match stacks with
          | [] -> List.map stack_entries ~f:(fun _ -> [])
          | stacks -> stacks
        in
        List.map2_exn stacks stack_entries ~f:(fun stack -> function
          | ' ' -> stack | c -> stack @ [ c ])
    | `Header_indices | `Empty -> stacks
    | `Rearrange (quantity, source, destination) ->
        let moved_crates, updated_source =
          List.split_n (List.nth_exn stacks (source - 1)) quantity
        in
        List.mapi stacks ~f:(fun idx stack ->
            match idx with
            | _ when idx = source - 1 -> updated_source
            | _ when idx = destination - 1 ->
                crane_transform moved_crates @ stack
            | _ -> stack)
  in
  Reader.with_file "day_5_input.txt"
    ~f:
      (Reader.lines >> Pipe.map ~f:parse
      >> Pipe.fold_without_pushback ~init:[] ~f:update_state)

let part_a =
  rearrange_result ~crane_transform:List.rev
  >>| List.map ~f:List.hd_exn >>| String.of_char_list

let%expect_test _ =
  let%bind result = part_a in
  printf "%s\n" result;
  [%expect "WCZTHTMPS"]

let part_b =
  rearrange_result ~crane_transform:Fn.id
  >>| List.map ~f:List.hd_exn >>| String.of_char_list

let%expect_test _ =
  let%bind result = part_b in
  printf "%s\n" result;
  [%expect "BLSGJSDTS"]