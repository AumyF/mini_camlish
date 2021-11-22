open Parserlib_test

let () =
  let open Alcotest in
  run "Parser"
    [
      ("parse", [ test_case "parse_result" `Quick test_middle ]);
      ("parse_ipv4", [ test_case "parse_ipv4" `Quick test_ipv4 ]);
      ("parse_string", [ test_case "parse_string" `Quick test_match_string ]);
      ( "some/many",
        [
          test_case "parse_some" `Quick test_some;
          test_case "parse_many" `Quick test_many;
        ] );
      ( "numeric",
        [
          test_case "match_nat" `Quick test_match_nat;
          test_case "match_int" `Quick test_match_int;
        ] );
      ("ident", [ test_case "match_nat" `Quick test_match_identifier ]);
    ]
