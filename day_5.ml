open Core
open Async

let ( >> ) f g x = g (f x)

let with_parsed_input ~f =
  let parse line =
    match String.length line >= 2 with
    | false -> `Empty
    | true -> (
        match String.get line 1 with
        | ' ' | 'A' .. 'Z' ->
            `Header
              (String.to_list line |> List.filteri ~f:(fun idx _ -> idx % 4 = 1))
        | '1' -> `Header_indices
        | 'o' -> (
            match String.split ~on:' ' line with
            | [ "move"; quantity; "from"; source; "to"; destination ] ->
                `Rearrange
                  Int.
                    (of_string quantity, of_string source, of_string destination)
            | _ -> failwith "unable to parse input line")
        | _ -> failwith "unable to parse input line")
  in
  Reader.with_file "day_5_input.txt" ~f:(Reader.lines >> Pipe.map ~f:parse >> f)

let part_a () =
  let update_state state line =
    match (state, line) with
    | `Initial, `Header stack_entries ->
        let stacks =
          List.map stack_entries ~f:(function ' ' -> [] | c -> [ c ])
        in
        `Setup stacks
    | `Setup stacks, `Header stack_entries ->
        let stacks =
          List.map2_exn stacks stack_entries ~f:(fun stack entry ->
              match entry with ' ' -> stack | c -> List.append stack [ c ])
        in
        `Setup stacks
    | `Setup stacks, `Header_indices -> `Setup stacks
    | `Setup stacks, `Empty -> `Rearranging stacks
    | `Rearranging stacks, `Rearrange (quantity, source, destination) ->
        let moved_crates, updated_source =
          List.split_n (List.nth_exn stacks (source - 1)) quantity
        in
        let stacks =
          List.mapi stacks ~f:(fun idx stack ->
              match idx with
              | _ when idx = source - 1 -> updated_source
              | _ when idx = destination - 1 ->
                  List.append (List.rev moved_crates) stack
              | _ -> stack)
        in
        `Rearranging stacks
    | _ -> failwith "invalid line type for state"
  in
  with_parsed_input
    ~f:(Pipe.fold_without_pushback ~init:`Initial ~f:update_state)
  >>| function
  | `Rearranging stacks ->
      List.map ~f:List.hd_exn stacks |> String.of_char_list |> printf "a: %s\n"
  | _ -> failwith "input file ended before rearranging"

let part_b () =
  let update_state state line =
    match (state, line) with
    | `Initial, `Header stack_entries ->
        let stacks =
          List.map stack_entries ~f:(function ' ' -> [] | c -> [ c ])
        in
        `Setup stacks
    | `Setup stacks, `Header stack_entries ->
        let stacks =
          List.map2_exn stacks stack_entries ~f:(fun stack entry ->
              match entry with ' ' -> stack | c -> List.append stack [ c ])
        in
        `Setup stacks
    | `Setup stacks, `Header_indices -> `Setup stacks
    | `Setup stacks, `Empty -> `Rearranging stacks
    | `Rearranging stacks, `Rearrange (quantity, source, destination) ->
        let moved_crates, updated_source =
          List.split_n (List.nth_exn stacks (source - 1)) quantity
        in
        let stacks =
          List.mapi stacks ~f:(fun idx stack ->
              match idx with
              | _ when idx = source - 1 -> updated_source
              | _ when idx = destination - 1 -> List.append moved_crates stack
              | _ -> stack)
        in
        `Rearranging stacks
    | _ -> failwith "invalid line type for state"
  in
  with_parsed_input
    ~f:(Pipe.fold_without_pushback ~init:`Initial ~f:update_state)
  >>| function
  | `Rearranging stacks ->
      List.map ~f:List.hd_exn stacks |> String.of_char_list |> printf "b: %s\n"
  | _ -> failwith "input file ended before rearranging"

let () =
  Command.async ~summary:"advent of code"
    (Command.Param.return (fun () ->
         let%bind () = part_a () in
         part_b ()))
  |> Command.run
