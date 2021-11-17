open Containers
open Mini_ocaml

let check =
  Ast.(
    Let ("x", Plus (IntLiteral 4, IntLiteral 3), Plus (VarRef "x", IntLiteral 3))
    |> eval3_with_emptyenv |> string_of_value |> print_endline)

let c =
  let exp, _ =
    Oc_parser.(Parser.parse_string get_exp "let x = 4 in x")
    |> Option.get_exn_or "parse error"
  in
  Ast.(exp |> eval3_with_emptyenv |> string_of_value |> print_endline)

let () = c
