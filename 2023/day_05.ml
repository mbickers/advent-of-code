open Core

let ( >> ) f g x = g (f x)

let example =
  {|seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4|}
;;

let parse_almanac =
  let open Angstrom in
  let number = Int.of_string <$> take_while1 Char.is_digit in
  let almanac =
    string "seeds: " *> sep_by (char ' ') number
    >>= fun seeds ->
    string "\n\n"
    *> sep_by
         (string "\n\n")
         (take_till Char.is_whitespace
          >>= fun map_name ->
          string " map:\n"
          *> sep_by
               (char '\n')
               (number
                >>= fun dest ->
                char ' ' *> number
                >>= fun src ->
                char ' ' *> number >>= fun len -> return (`dest dest, `src src, `len len)
               )
          >>= fun ranges -> return (map_name, ranges))
    >>= fun maps -> return (seeds, maps)
  in
  parse_string ~consume:All almanac >> Result.ok_or_failwith
;;

let part_a =
  let apply_map (_, ranges) number =
    match
      List.find ranges ~f:(fun (_, `src src, `len len) ->
        Int.between ~low:src ~high:(src + len - 1) number)
    with
    | Some (`dest dest, `src src, _) -> dest - src + number
    | None -> number
  in
  parse_almanac
  >> fun (seeds, maps) ->
  let seed_to_location =
    List.fold ~init:Fn.id ~f:(fun acc map -> acc >> apply_map map) maps
  in
  List.map seeds ~f:seed_to_location |> List.min_elt ~compare |> Option.value_exn
;;

let%expect_test _ =
  example |> part_a |> [%sexp_of: int] |> print_s;
  [%expect "35"]
;;

let%expect_test _ =
  In_channel.read_all "day_05_input.txt" |> part_a |> [%sexp_of: int] |> print_s;
  [%expect "836040384"]
;;

let part_b =
  let rec pair_seeds = function
    | start :: length :: rest -> (`start start, `length length) :: pair_seeds rest
    | [] -> []
    | _ -> failwith "bad seeds"
  in
  let apply_map (_, ranges) number =
    match
      List.find ranges ~f:(fun (_, `src src, `len len) ->
        Int.between ~low:src ~high:(src + len - 1) number)
    with
    | Some (`dest dest, `src src, _) -> dest - src + number
    | None -> number
  in
  parse_almanac
  >> fun (seeds, maps) ->
  let seeds =
    pair_seeds seeds
    |> List.concat_map ~f:(fun (`start start, `length length) ->
      List.init length ~f:(fun i -> start + i))
  in
  let seed_to_location =
    List.fold ~init:Fn.id ~f:(fun acc map -> acc >> apply_map map) maps
  in
  List.map seeds ~f:seed_to_location |> List.min_elt ~compare |> Option.value_exn
;;

let%expect_test _ =
  example |> part_b |> [%sexp_of: int] |> print_s;
  [%expect "46"]
;;

(* let%expect_test _ =
   In_channel.read_all "day_05_input.txt" |> part_b |> [%sexp_of: int] |> print_s;
   [%expect "836040384"]
   ;; *)
