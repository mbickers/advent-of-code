open Core

let ( >> ) f g x = g (f x)

let example1 =
  {|RL

AAA = (BBB, CCC)
BBB = (DDD, EEE)
CCC = (ZZZ, GGG)
DDD = (DDD, DDD)
EEE = (EEE, EEE)
GGG = (GGG, GGG)
ZZZ = (ZZZ, ZZZ)|}
;;

let example2 = {|LLR

AAA = (BBB, BBB)
BBB = (AAA, ZZZ)
ZZZ = (ZZZ, ZZZ)|}

let parse_map =
  let open Angstrom in
  let map =
    many (char 'R' *> return `right <|> char 'L' *> return `left)
    >>= fun instructions ->
    end_of_line
    *> end_of_line
    *> sep_by
         end_of_line
         (take 3
          >>= fun node ->
          string " = (" *> take 3
          >>= fun left ->
          string ", " *> take 3
          >>= fun right -> char ')' *> return (node, (`left left, `right right)))
    >>= fun network -> return (instructions, network)
  in
  parse_string ~consume:All map >> Result.ok_or_failwith
;;

let part_a =
  let steps_from ~source ~dest (instructions, map) =
    let rec helper location steps_taken =
      match String.equal location dest with
      | true -> steps_taken
      | false ->
        let `left left, `right right =
          List.Assoc.find_exn map ~equal:String.equal location
        in
        (match List.nth_exn instructions (steps_taken % List.length instructions) with
         | `left -> helper left (steps_taken + 1)
         | `right -> helper right (steps_taken + 1))
    in
    helper source 0
  in
  parse_map >> steps_from ~source:"AAA" ~dest:"ZZZ"
;;

let%expect_test _ =
  example1 |> part_a |> [%sexp_of: int] |> print_s;
  [%expect "2"]
;;

let%expect_test _ =
  example2 |> part_a |> [%sexp_of: int] |> print_s;
  [%expect "6"]
;;

let%expect_test _ =
  In_channel.read_all "day_08_input.txt" |> part_a |> [%sexp_of: int] |> print_s;
  [%expect "15517"]
;;

let example3 =
  {|LR

11A = (11B, XXX)
11B = (XXX, 11Z)
11Z = (11B, XXX)
22A = (22B, XXX)
22B = (22C, 22C)
22C = (22Z, 22Z)
22Z = (22B, 22B)
XXX = (XXX, XXX)|}
;;

let part_b input =
  let instructions, map = parse_map input in
  let instructions = Array.of_list instructions in
  let map = String.Map.of_alist_exn map in
  let start_locations = Map.keys map |> List.filter ~f:(String.is_suffix ~suffix:"A") in
  let best = ref 0 in
  let rec advance steps_taken locations =
    let finished_locations = List.count locations ~f:(String.is_suffix ~suffix:"Z") in
    best := Int.max finished_locations !best;
    match steps_taken with
    | 10000000000 -> failwith (sprintf "best: %d/%d" !best (List.length locations))
    | _ ->
      (match List.for_all locations ~f:(String.is_suffix ~suffix:"Z") with
       | true -> steps_taken
       | false ->
         let next location =
           let `left left, `right right = Map.find_exn map location in
           match Array.get instructions (steps_taken % Array.length instructions) with
           | `left -> left
           | `right -> right
         in
         advance (steps_taken + 1) (List.map locations ~f:next))
  in
  advance 0 start_locations
;;

let%expect_test _ =
  example3 |> part_b |> [%sexp_of: int] |> print_s;
  [%expect "6"]
;;

let%expect_test _ =
  In_channel.read_all "day_08_input.txt" |> part_b |> [%sexp_of: int] |> print_s;
  [%expect "15517"]
;;
