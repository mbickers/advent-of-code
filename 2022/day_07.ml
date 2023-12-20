open Core

let ( % ) f g x = f (g x)
let ( >> ) f g x = g (f x)

module Filesystem = struct
  type t = Directory of (string * t) list | File of int

  let empty = Directory []

  let rec add_node t path node =
    match (t, path) with
    | Directory siblings, [ name ] -> Directory (siblings @ [ (name, node) ])
    | Directory siblings, name :: rest -> (
        let found, new_contents =
          List.fold_map siblings ~init:`Not_found
            ~f:(fun found (inner_name, inner_t) ->
              match String.equal name inner_name with
              | false -> (found, (inner_name, inner_t))
              | true -> (`Found, (inner_name, add_node inner_t rest node)))
        in
        match found with
        | `Found -> Directory new_contents
        | `Not_found -> failwith "unable to find subdirectory")
    | File _, _ -> failwith "unable to add node to file"
    | _, [] -> failwith "unable to add file with no name"

  let rec to_string ~prefix t =
    match t with
    | File size -> prefix ^ " (file, size=" ^ Int.to_string size ^ ")"
    | Directory entries ->
        List.map entries ~f:(fun (name, t) ->
            to_string ~prefix:(prefix ^ "/" ^ name) t)
        |> List.intersperse ~sep:"\n" |> List.reduce_exn ~f:( ^ )

  let rec size t =
    match t with
    | Directory entries ->
        List.sum (module Int) ~f:(fun (_, t) -> size t) entries
    | File size -> size
end

let construct_filesystem commands_and_output =
  let filesystem, _ =
    List.fold commands_and_output ~init:(Filesystem.empty, [])
      ~f:(fun (filesystem, current_path) line ->
        match String.split line ~on:' ' with
        | [ "$"; "cd"; "/" ] | [ "$"; "ls" ] -> (filesystem, current_path)
        | [ "$"; "cd"; ".." ] -> (filesystem, List.drop_last_exn current_path)
        | [ "$"; "cd"; dir ] -> (filesystem, current_path @ [ dir ])
        | [ "dir"; name ] ->
            ( Filesystem.add_node filesystem
                (current_path @ [ name ])
                (Directory []),
              current_path )
        | [ size; name ] ->
            ( Filesystem.add_node filesystem
                (current_path @ [ name ])
                (File (Int.of_string size)),
              current_path )
        | _ -> failwith (sprintf "unrecognized input line '%s'" line))
  in
  filesystem

let filesystem = construct_filesystem (In_channel.read_lines "day_7_input.txt")

let part_a =
  let rec helper acc filesystem =
    match filesystem with
    | Filesystem.Directory entries -> (
        let accs_and_sizes = List.map ~f:(fun (_, t) -> helper 0 t) entries in
        let accs_sum, size =
          List.reduce_exn
            ~f:(fun (acc1, size1) (acc2, size2) -> (acc1 + acc2, size1 + size2))
            accs_and_sizes
        in
        match size <= 100000 with
        | true -> (acc + accs_sum + size, size)
        | false -> (acc + accs_sum, size))
    | File size -> (0, size)
  in
  let acc, _ = helper 0 filesystem in
  acc

let%expect_test _ =
  part_a |> printf "a: %d\n";
  [%expect "a: 1886043"]

let part_b =
  let free_space = 70000000 - Filesystem.size filesystem in
  let space_needed = 30000000 - free_space in
  let rec helper filesystem =
    match filesystem with
    | Filesystem.Directory entries ->
        let accs_and_sizes = List.map ~f:(fun (_, t) -> helper t) entries in
        let best_size, size =
          List.reduce_exn
            ~f:(fun (best_size1, size1) (best_size2, size2) ->
              (Option.merge ~f:Int.min best_size1 best_size2, size1 + size2))
            accs_and_sizes
        in
        let best_size =
          Option.merge best_size
            (Option.some_if (size >= space_needed) size)
            ~f:Int.min
        in
        (best_size, size)
    | File size -> (None, size)
  in
  let acc, _ = helper filesystem in
  Option.value_exn acc

let%expect_test _ =
  part_b |> printf "b: %d\n";
  [%expect "b: 3842121"]