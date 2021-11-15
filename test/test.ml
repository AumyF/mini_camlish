open Mini_ocaml
open Containers

let parse_ipv4_address =
  let _parse_ipv4_digit =
    Parser.(
      (fun d3 d2 d1 d0 -> [ d3; d2; d1; d0 ] |> String.of_list)
      <$> get_char <*> get_char <*> get_char <*> get_char)
  in
  Parser.((fun x -> [ x ] |> String.of_list) <$> get_char)

let parse_result t =
  Alcotest.option (Alcotest.pair t (Alcotest.list Alcotest.char))

let test_middle () =
  Alcotest.(check (parse_result char))
    "whether both are same"
    (Some ('b', []))
    (Parser.parse_string Parser.get_middle "abc")

let test_ipv4 () =
  Alcotest.(check (parse_result string))
    "correctly parse ipv4 address"
    (Some ("1", []))
    Parser.(parse_string parse_ipv4_address "1")

let test_get_string () =
  Alcotest.(check (parse_result string))
    "correctly parse string"
    (Some ("foo", [ 'b'; 'a'; 'r' ]))
    Parser.(parse_string (get_string "foo") "foobar")

let test_some () =
  Alcotest.(check (char |> list |> parse_result))
    "correctly parse some"
    (Some ([ 'f'; 'f'; 'f' ], [ 'b'; 'a'; 'r' ]))
    Parser.(parse_string (many (match_char 'f')) "fffbar");

  Alcotest.(check (char |> list |> parse_result))
    "correctly parse some"
    (Some ([ 'x'; 'y'; 'z'; 'x'; 'y'; 'z'; 'x'; 'z'; 'y' ], [ 'b'; 'a'; 'r' ]))
    Parser.(
      parse_string
        (many (satisfy (function 'x' .. 'z' -> true | _ -> false)))
        "xyzxyzxzybar");

  Alcotest.(check (char |> list |> parse_result))
    "succeeds in parsing with string which contains zero f's" None
    Parser.(parse_string (some (match_char 'f')) "efff")

let test_many () =
  Alcotest.(check (char |> list |> parse_result))
    "correctly parse many"
    (Some ([ 'f'; 'f'; 'f' ], [ 'b'; 'a'; 'r' ]))
    Parser.(parse_string (many (match_char 'f')) "fffbar");

  Alcotest.(check (char |> list |> parse_result))
    "succeeds in parsing with string which contains zero f's"
    (Some ([], [ 'e'; 'f'; 'f'; 'f' ]))
    Parser.(parse_string (many (match_char 'f')) "efff")

let () =
  Alcotest.run "Parser"
    [
      ("parse", [ Alcotest.test_case "parse_result" `Quick test_middle ]);
      ("parse_ipv4", [ Alcotest.test_case "parse_ipv4" `Quick test_ipv4 ]);
      ( "parse_string",
        [ Alcotest.test_case "parse_string" `Quick test_get_string ] );
      ( "some/many",
        [
          Alcotest.test_case "parse_some" `Quick test_some;
          Alcotest.test_case "parse_many" `Quick test_many;
        ] );
    ]
