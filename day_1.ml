open Core

let () =
  let calories =
    let max_elf_calories, last_elf_calories =
      In_channel.with_file "day_1_input.txt" ~f:(fun r ->
          In_channel.fold_lines r ~init:(0, 0)
            ~f:(fun (max_elf_calories, current_calories) line ->
              match line with
              | "" -> (Int.max max_elf_calories current_calories, 0)
              | _ -> (max_elf_calories, current_calories + Int.of_string line)))
    in
    Int.max max_elf_calories last_elf_calories
  in
  print_endline ("a: " ^ Int.to_string calories ^ " calories")

let () =
  let keep_top list new_element =
    List.take
      (List.sort (new_element :: list) ~compare:Int.compare |> List.rev)
      3
  in
  let elf_calories =
    let elf_calories, last_elf_calories =
      In_channel.with_file "day_1_input.txt" ~f:(fun r ->
          In_channel.fold_lines r ~init:([], 0)
            ~f:(fun (elf_calories, current_calories) line ->
              match line with
              | "" -> (keep_top elf_calories current_calories, 0)
              | _ -> (elf_calories, current_calories + Int.of_string line)))
    in
    match last_elf_calories with
    | 0 -> elf_calories
    | _ -> keep_top elf_calories last_elf_calories
  in
  let calories = List.sum (module Int) ~f:Fn.id elf_calories in
  print_endline ("b: " ^ Int.to_string calories ^ " calories")