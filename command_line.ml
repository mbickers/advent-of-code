open Core
open Advent_of_code

let time ~name ~f =
  let start_time = Time_ns.now () in
  let result = f () in
  let end_time = Time_ns.now () in
  printf "%s: %dms\n%!" name
    (Time_ns.diff end_time start_time |> Time_ns.Span.to_int_ms);
  result

let () =
  Command.basic ~summary:"advent of code"
    (Command.Param.return (fun () ->
         let result =
           time ~name:"test@24" ~f:(fun () ->
               Day_19.blueprint_geodes
                 ~blueprint:(List.nth_exn Day_19.input 2)
                 ~time:32)
         in
         printf "%d\n" result))
  |> Command.run
