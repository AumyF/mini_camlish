open Containers
open Mini_ocaml

(*
   let check =
     let open Ast in
     let open Ast.Expression in
     Let ("x", Plus (IntLiteral 4, IntLiteral 3), Plus (VarRef "x", IntLiteral 3))
     |> eval3_with_emptyenv |> string_of_value |> print_endline
*)
let c =
  let exp, _ =
    Oc_parser.(
      Parser.parse_string value
        {|
      let x = 1 in
      let f = fun y -> x + y in
      let x = 2 in
      f (x + 3)
    |})
    |> Result.get_exn
  in
  Ast.(exp |> eval3_with_emptyenv |> string_of_value |> print_endline)

let () = c
