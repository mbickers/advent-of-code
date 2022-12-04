open Core
open Async

let ( % ) f g x = f (g x)
let ( >> ) f g x = g (f x)

let with_parsed_input ~f =
  let parse =
    String.split_on_chars ~on:[ ','; '-' ] >> List.map ~f:Int.of_string
    >> function
    | [ l1; u1; l2; u2 ] -> ((l1, u1), (l2, u2))
    | _ -> failwith "failed to parse line"
  in
  Reader.with_file "day_4_input.txt" ~f:(Reader.lines >> Pipe.map ~f:parse >> f)

let part_a () =
  let either_range_contains_other (range1, range2) =
    let contained ~in_:(l1, u1) (l2, u2) = l1 <= l2 && u2 <= u1 in
    contained ~in_:range1 range2 || contained ~in_:range2 range1
  in
  with_parsed_input
    ~f:(Pipe.filter ~f:either_range_contains_other >> Pipe.drain_and_count)
  >>| print_int

let part_b () =
  let ranges_overlap ((l1, u1), (l2, u2)) =
    (l2 <= u1 && u2 >= l1) || (l1 <= u2 && u1 >= l2)
  in
  with_parsed_input ~f:(Pipe.filter ~f:ranges_overlap >> Pipe.drain_and_count)
  >>| print_int

let () =
  Command.async ~summary:"advent of code"
    (Command.Param.return (fun () ->
         let%bind () = part_a () in
         part_b ()))
  |> Command.run
