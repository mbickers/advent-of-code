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

let parse_almanac ~transform_seeds =
  let open Angstrom in
  let number = Int.of_string <$> take_while1 Char.is_digit in
  let almanac =
    string "seeds: " *> (transform_seeds <$> sep_by (char ' ') number)
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
  parse_almanac ~transform_seeds:Fn.id
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

module Range = struct
  type t =
    { start : int
    ; end_ : int
    }
  [@@deriving sexp]

  let valid { start; end_ } = start <= end_
  let shift ~by t = { start = t.start + by; end_ = t.end_ + by }
end

module Range_map = struct
  type t =
    { src_start : int
    ; src_end : int
    ; dest_start : int
    }
  [@@deriving sexp]

  let apply { src_start; src_end; dest_start } Range.{ start; end_ } =
    let mapped_start = Int.max src_start start in
    let mapped_end = Int.min src_end end_ in
    match mapped_start <= mapped_end with
    | true ->
      ( `mapped
          Range.
            [ shift
                ~by:(dest_start - src_start)
                { start = mapped_start; end_ = mapped_end }
            ]
      , `unmapped
          (List.filter
             ~f:Range.valid
             Range.
               [ { start; end_ = mapped_start - 1 }; { start = mapped_end + 1; end_ } ]) )
    | false -> `mapped [], `unmapped Range.[ { start; end_ } ]
  ;;

  let apply_to_multiple t =
    List.fold
      ~init:(`mapped [], `unmapped [])
      ~f:(fun (`mapped mapped, `unmapped unmapped) range ->
        let `mapped new_mapped, `unmapped new_unmapped = apply t range in
        `mapped (new_mapped @ mapped), `unmapped (new_unmapped @ unmapped))
  ;;
end

let part_b input =
  let rec seeds_to_ranges = function
    | start :: len :: rest -> (`start start, `len len) :: seeds_to_ranges rest
    | [] -> []
    | _ -> failwith "bad seeds"
  in
  let seed_ranges, maps = parse_almanac ~transform_seeds:seeds_to_ranges input in
  let seed_ranges =
    List.map seed_ranges ~f:(fun (`start start, `len len) ->
      Range.{ start; end_ = start + len - 1 })
  in
  let maps =
    List.map maps ~f:(fun (name, map_ranges) ->
      ( name
      , List.map map_ranges ~f:(fun (`dest dest, `src src, `len len) ->
          Range_map.{ src_start = src; src_end = src + len - 1; dest_start = dest }) ))
  in
  List.fold maps ~init:seed_ranges ~f:(fun ranges (_, map_ranges) ->
    let `mapped mapped, `unmapped unmapped =
      List.fold
        map_ranges
        ~init:(`mapped [], `unmapped ranges)
        ~f:(fun (`mapped mapped, `unmapped ranges) range_map ->
          let `mapped new_mapped, `unmapped ranges =
            Range_map.apply_to_multiple range_map ranges
          in
          `mapped (new_mapped @ mapped), `unmapped ranges)
    in
    mapped @ unmapped)
  |> List.map ~f:(fun { start; _ } -> start)
  |> List.min_elt ~compare
  |> Option.value_exn
;;

let%expect_test _ =
  example |> part_b |> [%sexp_of: int] |> print_s;
  [%expect "46"]
;;

let%expect_test _ =
  In_channel.read_all "day_05_input.txt" |> part_b |> [%sexp_of: int] |> print_s;
  [%expect "10834440"]
;;
