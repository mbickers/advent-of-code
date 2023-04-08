open Core

let test_input =
  In_channel.read_lines "day_20_input_test.txt" |> List.map ~f:Int.of_string

let input =
  In_channel.read_lines "day_20_input.txt" |> List.map ~f:Int.of_string

module Circular_list = struct
  type entry = { number : int; original_index : int; current_index : int }
  type t = entry List.t

  let of_list =
    List.mapi ~f:(fun original_index number ->
        { number; original_index; current_index = original_index })

  let to_list t =
    List.sort t
      ~compare:(fun
                 { current_index = current_index1; _ }
                 { current_index = current_index2; _ }
               -> Int.compare current_index1 current_index2)
    |> List.map ~f:(fun { number; _ } -> number)

  let shift ~original_index ~by t =
    let { current_index; _ } =
      List.find_exn t ~f:(fun { original_index = entry_original_index; _ } ->
          original_index = entry_original_index)
    in
    let new_index, old_index =
      ((current_index + by) % (List.length t - 1), current_index)
    in
    List.map t ~f:(fun entry ->
        let { current_index; _ } = entry in
        match
          ( Int.compare current_index old_index,
            Int.compare current_index new_index )
        with
        | 0, _ -> { entry with current_index = new_index }
        | -1, 0 | -1, 1 -> { entry with current_index = current_index + 1 }
        | 1, -1 | 1, 0 -> { entry with current_index = current_index - 1 }
        | _, _ -> entry)

  let mix t =
    List.fold t ~init:t ~f:(fun t { number; original_index; _ } ->
        shift ~original_index ~by:number t)

  let nth_exn t index =
    List.find_map_exn t ~f:(fun { number; current_index; _ } ->
        Option.some_if (current_index = index % List.length t) number)

  let zero_index =
    List.find_map_exn ~f:(fun { number; current_index; _ } ->
        Option.some_if (number = 0) current_index)
end

let%expect_test _ =
  Circular_list.(of_list test_input |> shift ~original_index:0 ~by:1 |> to_list)
  |> [%sexp_of: int list] |> print_s;
  [%expect "(2 1 -3 3 -2 0 4)"]

let part_a input =
  let mixed = Circular_list.(of_list input |> mix) in
  List.sum
    (module Int)
    [ 1000; 2000; 3000 ]
    ~f:(fun index ->
      Circular_list.nth_exn mixed (index + Circular_list.zero_index mixed))

let%expect_test _ =
  test_input |> part_a |> printf "%d\n%!";
  [%expect "3"]

let%expect_test _ =
  input |> part_a |> printf "%d\n%!";
  [%expect "8372"]

let part_b input =
  let decryption_key = 811589153 in
  let list =
    List.map input ~f:(( * ) decryption_key) |> Circular_list.of_list
  in
  let mixed = Fn.apply_n_times ~n:10 Circular_list.mix list in
  List.sum
    (module Int)
    [ 1000; 2000; 3000 ]
    ~f:(fun index ->
      Circular_list.nth_exn mixed (index + Circular_list.zero_index mixed))

let%expect_test _ =
  test_input |> part_b |> printf "%d\n%!";
  [%expect "1623178306"]

let%expect_test _ =
  input |> part_b |> printf "%d\n%!";
  [%expect "7865110481723"]