open Core

let part_a file =
  let max_elf_calories, last_elf_calories =
    In_channel.fold_lines ~init:(0, 0) file
      ~f:(fun (max_elf_calories, current_calories) line ->
        match line with
        | "" -> (Int.max max_elf_calories current_calories, 0)
        | _ -> (max_elf_calories, current_calories + Int.of_string line))
  in
  Int.max max_elf_calories last_elf_calories

let%expect_test _ =
  In_channel.with_file "day_1_input.txt" ~f:part_a |> printf "%d\n";
  [%expect "75501"]

let part_b file =
  let keep_top list new_element =
    List.take
      (List.sort (new_element :: list) ~compare:Int.compare |> List.rev)
      3
  in
  let elf_calories =
    let elf_calories, last_elf_calories =
      In_channel.fold_lines ~init:([], 0) file
        ~f:(fun (elf_calories, current_calories) line ->
          match line with
          | "" -> (keep_top elf_calories current_calories, 0)
          | _ -> (elf_calories, current_calories + Int.of_string line))
    in
    match last_elf_calories with
    | 0 -> elf_calories
    | _ -> keep_top elf_calories last_elf_calories
  in
  List.sum (module Int) ~f:Fn.id elf_calories

let%expect_test _ =
  In_channel.with_file "day_1_input.txt" ~f:part_b |> printf "%d\n";
  [%expect "215594"]