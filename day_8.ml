open Core

let ( >> ) f g x = g (f x)

let test_input = {|30373
25512
65332
33549
35390|} |> String.split_lines

let heights =
  In_channel.read_lines "day_8_input.txt"
  |> List.map ~f:String.to_list
  |> List.map ~f:(List.map ~f:(String.of_char >> Int.of_string))

let running_maximum =
  List.folding_map ~init:(-1) ~f:(fun max_seen current ->
      (Int.max max_seen current, max_seen))

let matrix_to_string matrix ~f =
  List.map matrix ~f:(List.map ~f >> List.reduce_exn ~f:( ^ ))
  |> List.intersperse ~sep:"\n" |> List.reduce_exn ~f:( ^ )

let direction_maximums =
  [
    List.map ~f:running_maximum heights;
    List.map ~f:(List.rev >> running_maximum >> List.rev) heights;
    List.transpose_exn
      (List.map ~f:running_maximum (List.transpose_exn heights));
    List.transpose_exn
      (List.map
         ~f:(List.rev >> running_maximum >> List.rev)
         (List.transpose_exn heights));
  ]

let () =
  let get ~row_idx ~col_idx matrix =
    let row = List.nth_exn matrix row_idx in
    List.nth_exn row col_idx
  in
  let visible =
    List.mapi heights ~f:(fun row_idx row ->
        List.mapi row ~f:(fun col_idx height ->
            let direction_maximums =
              List.map direction_maximums ~f:(get ~row_idx ~col_idx)
            in
            let visible = List.exists direction_maximums ~f:(( > ) height) in
            visible))
  in
  let count = List.sum (module Int) ~f:(List.count ~f:Fn.id) visible in
  printf "a: %d\n" count
