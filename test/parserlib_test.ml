open Mini_ocaml
open Containers

let parse_ipv4_address =
  let _parse_ipv4_digit =
    Parser.(
      (fun d3 d2 d1 d0 -> [ d3; d2; d1; d0 ] |> String.of_list)
      <$> get_char <*> get_char <*> get_char <*> get_char)
  in
  Parser.((fun x -> [ x ] |> String.of_list) <$> get_char)

let parse_error =
  let open Alcotest in
  Parser.ParseError.(testable pp equal)

let parse_result t =
  let open Alcotest in
  result (pair t (list char)) parse_error

let test_middle () =
  Alcotest.(check (parse_result char))
    "whether both are same"
    (Ok ('b', []))
    (Parser.parse_string Parser.get_middle "abc")

let test_ipv4 () =
  Alcotest.(check (parse_result string))
    "correctly parse ipv4 address"
    (Ok ("1", []))
    Parser.(parse_string parse_ipv4_address "1")

let test_match_string () =
  Alcotest.(check (parse_result string))
    "correctly parse string"
    (Ok ("foo", [ 'b'; 'a'; 'r' ]))
    Parser.(parse_string (match_string "foo") "foobar")

let test_some () =
  Alcotest.(check (char |> list |> parse_result))
    "correctly parse some"
    (Ok ([ 'f'; 'f'; 'f' ], [ 'b'; 'a'; 'r' ]))
    Parser.(parse_string (many (match_char 'f')) "fffbar");

  Alcotest.(check (char |> list |> parse_result))
    "correctly parse some"
    (Ok ([ 'x'; 'y'; 'z'; 'x'; 'y'; 'z'; 'x'; 'z'; 'y' ], [ 'b'; 'a'; 'r' ]))
    Parser.(
      parse_string
        (many (satisfy (function 'x' .. 'z' -> true | _ -> false)))
        "xyzxyzxzybar");

  Alcotest.(check (char |> list |> parse_result))
    "succeeds in parsing with string which contains zero f's"
    (Error Parser.ParseError.{ msg = "empty" })
    Parser.(parse_string (some (match_char 'f')) "efff")

let test_many () =
  let parse ch = Parser.(parse_string (many (match_char ch))) in
  Alcotest.(check (char |> list |> parse_result))
    "correctly parse many"
    (Ok ([ 'f'; 'f'; 'f' ], [ 'b'; 'a'; 'r' ]))
    (parse 'f' "fffbar");

  Alcotest.(check (char |> list |> parse_result))
    "succeeds in parsing with string which contains zero f's"
    (Ok ([], [ 'e'; 'f'; 'f'; 'f' ]))
    (parse 'f' "efff")

let test_match_nat () =
  let parse_string = Parser.(parse_string match_nat) in

  Alcotest.(check (int |> parse_result))
    "succeeds in parsing with string starts with a positive number"
    (Ok (12345, [ 'f'; 'o'; 'o' ]))
    (parse_string "12345foo");

  Alcotest.(check (int |> parse_result))
    "succeeds in parsing with string starts with a 0"
    (Ok (0, [ 'f'; 'o'; 'o' ]))
    (parse_string "0foo");

  Alcotest.(check (int |> parse_result))
    "fails in parsing with string starts with a negative number"
    (Error Parser.ParseError.{ msg = "empty" })
    (parse_string "-1321baz")

let test_match_int () =
  let parse_string = Parser.(parse_string match_int) in

  Alcotest.(check (int |> parse_result))
    "succeeds in parsing with string starts with a positive number"
    (Ok (1321, [ 'b'; 'a'; 'z' ]))
    (parse_string "1321baz");

  Alcotest.(check (int |> parse_result))
    "succeeds in parsing with string starts with a negative number"
    (Ok (-1321, [ 'b'; 'a'; 'z' ]))
    (parse_string "-1321baz");

  Alcotest.(check (int |> parse_result))
    "succeeds in parsing with string starts with a -0"
    (Ok (0, [ 'b'; 'a'; 'z' ]))
    (parse_string "-0baz")

let test_match_identifier () =
  let parse = Parser.(parse_string match_identifier) in

  Alcotest.(check (string |> parse_result))
    "succeeds in parsing with string starts with a lowercase"
    (Ok ("abc", [ ' '; 'f'; 'o' ]))
    (parse "abc fo");

  Alcotest.(check (string |> parse_result))
    "succeeds in parsing with string starts with a lowercase and includes some \
     digits"
    (Ok ("e38182", [ ' '; 'f'; 'o' ]))
    (parse "e38182 fo")

let test_get_list_of_uints () =
  let parse = Parser.(parse_string get_list_of_uints) in

  Alcotest.(check (int |> list |> parse_result))
    "succeeds in parsing list of uints"
    (Ok ([ 1; 2; 3 ], [ ' '; 'f'; 'u' ]))
    (parse "[1; 2; 3] fu");

  Alcotest.(check (int |> list |> parse_result))
    "succeeds in parsing list of uints with a trailing semicolon"
    (Ok ([ 1; 2; 3 ], [ ' '; 'f'; 'u' ]))
    (parse "[1; 2; 3;] fu")
