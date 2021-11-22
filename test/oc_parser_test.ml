open Mini_ocaml
open Oc_parser

let parse_result =
  let open Alcotest in
  let char_list = char |> list in
  let expression = Ast.Expression.(testable pp equal) in
  pair expression char_list |> option

let test_addition () =
  Alcotest.(check parse_result)
    "whether both are same"
    (Some Ast.Expression.(Plus (IntLiteral 3, IntLiteral 4), []))
    (parse_string value "3 + 4")

let test_subtract () =
  Alcotest.(check parse_result)
    "whether both are same"
    (Some Ast.Expression.(Plus (IntLiteral 3, IntLiteral 4), []))
    (parse_string value "4 - 3")

let test_let_in () =
  Alcotest.(check parse_result)
    "whether both are same"
    (let open Ast.Expression in
    Some (Let ("x", IntLiteral 4, VarRef "x"), []))
    (parse_string value "let x = 4 in x")

let test_if_then_else () =
  let open Ast.Expression in
  Alcotest.(check parse_result)
    "whether both are same"
    (Some (If (BoolLiteral true, IntLiteral 3, IntLiteral 9), []))
    (parse_string value "if true then 3 else 9")

let () =
  let open Alcotest in
  run "Oc_parser"
    [
      ( "ocparser",
        [
          test_case "addition" `Quick test_addition;
          test_case "let_in" `Quick test_let_in;
          test_case "if_then_else" `Quick test_if_then_else;
        ] );
    ]
