open Mini_ocaml

let check =
  Ast.(
    Let ("x", Plus (IntLiteral 4, IntLiteral 3), Plus (VarRef "x", IntLiteral 3))
    |> eval3_with_emptyenv |> string_of_value |> print_endline)

let () = check
