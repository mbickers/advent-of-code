open Core

let ( >> ) f g x = g (f x)

let example = {|32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483|}

module Card = struct
  type t = char [@@deriving sexp]

  let value ~j_is_joker = function
    | '2' -> 2
    | '3' -> 3
    | '4' -> 4
    | '5' -> 5
    | '6' -> 6
    | '7' -> 7
    | '8' -> 8
    | '9' -> 9
    | 'T' -> 10
    | 'J' ->
      (match j_is_joker with
       | true -> 1
       | false -> 11)
    | 'Q' -> 12
    | 'K' -> 13
    | 'A' -> 14
    | _ -> failwith "unknown card"
  ;;

  let compare ~j_is_joker = Comparable.lift Int.compare ~f:(value ~j_is_joker)
end

module Hand = struct
  type t = Card.t * Card.t * Card.t * Card.t * Card.t [@@deriving sexp]

  let of_list = function
    | [ c1; c2; c3; c4; c5 ] -> c1, c2, c3, c4, c5
    | _ -> failwith "invalid hand"
  ;;

  let to_list (c1, c2, c3, c4, c5) = [ c1; c2; c3; c4; c5 ]

  module Hand_type = struct
    type t =
      | Five_of_a_kind
      | Four_of_a_kind
      | Full_house
      | Three_of_a_kind
      | Two_pair
      | One_pair
      | High_card
    [@@deriving sexp]

    let of_hand ~j_is_joker hand =
      let group_like_cards =
        List.sort_and_group ~compare:(Card.compare ~j_is_joker)
        >> List.sort ~compare:(Comparable.lift Int.descending ~f:List.length)
      in
      let hand =
        match j_is_joker with
        | false -> hand
        | true ->
          let replace_joker_with =
            match
              hand |> to_list |> List.filter ~f:(Char.( <> ) 'J') |> group_like_cards
            with
            | (card :: _) :: _ -> card
            | [] -> 'J'
            | _ -> failwith "shouldn't be here"
          in
          List.map
            ~f:(function
              | 'J' -> replace_joker_with
              | c -> c)
            (to_list hand)
          |> of_list
      in
      match hand |> to_list |> group_like_cards with
      | [ [ _; _; _; _; _ ] ] -> Five_of_a_kind
      | [ [ _; _; _; _ ]; [ _ ] ] -> Four_of_a_kind
      | [ [ _; _; _ ]; [ _; _ ] ] -> Full_house
      | [ [ _; _; _ ]; [ _ ]; [ _ ] ] -> Three_of_a_kind
      | [ [ _; _ ]; [ _; _ ]; [ _ ] ] -> Two_pair
      | [ [ _; _ ]; [ _ ]; [ _ ]; [ _ ] ] -> One_pair
      | [ [ _ ]; [ _ ]; [ _ ]; [ _ ]; [ _ ] ] -> High_card
      | _ -> failwith "invalid hand"
    ;;

    let value = function
      | Five_of_a_kind -> 6
      | Four_of_a_kind -> 5
      | Full_house -> 4
      | Three_of_a_kind -> 3
      | Two_pair -> 2
      | One_pair -> 1
      | High_card -> 0
    ;;

    let compare = Comparable.lift Int.compare ~f:value
  end

  let compare ~j_is_joker =
    Comparable.(
      lexicographic
        [ lift Hand_type.compare ~f:(Hand_type.of_hand ~j_is_joker)
        ; lift (List.compare (Card.compare ~j_is_joker)) ~f:to_list
        ])
  ;;
end

let parse_list_of_hands =
  let open Angstrom in
  let hand =
    count 5 any_char
    >>= fun hand ->
    char ' ' *> take_while1 Char.is_digit
    >>= fun bid -> many (char ' ') *> return (Hand.of_list hand, Int.of_string bid)
  in
  parse_string ~consume:All (sep_by (char '\n') hand) >> Result.ok_or_failwith
;;

let total_winnings ~j_is_joker =
  List.sort
    ~compare:(Tuple2.compare ~cmp1:(Hand.compare ~j_is_joker) ~cmp2:(fun _ _ -> 0))
  >> List.mapi ~f:(fun index (_, bid) -> bid * (index + 1))
  >> List.sum (module Int) ~f:Fn.id
;;

let part_a = parse_list_of_hands >> total_winnings ~j_is_joker:false

let%expect_test _ =
  example |> part_a |> [%sexp_of: int] |> print_s;
  [%expect "6440"]
;;

let%expect_test _ =
  In_channel.read_all "day_07_input.txt" |> part_a |> [%sexp_of: int] |> print_s;
  [%expect "250058342"]
;;

let part_b = parse_list_of_hands >> total_winnings ~j_is_joker:true

let%expect_test _ =
  example |> part_b |> [%sexp_of: int] |> print_s;
  [%expect "5905"]
;;

let%expect_test _ =
  In_channel.read_all "day_07_input.txt" |> part_b |> [%sexp_of: int] |> print_s;
  [%expect "250506580"]
;;
