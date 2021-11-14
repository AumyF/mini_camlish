open Mini_ocaml

let parse_result t =
  Alcotest.option (Alcotest.pair t (Alcotest.list Alcotest.char))

let test_middle () =
  Alcotest.(check (parse_result char))
    "whether both are same"
    (Some ('b', []))
    (Parser.parse_string Parser.get_middle "abc")

let () =
  Alcotest.run "Parser"
    [ ("parse", [ Alcotest.test_case "parse_result" `Quick test_middle ]) ]
