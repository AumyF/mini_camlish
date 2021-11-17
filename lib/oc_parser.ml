open Parser
open Ast

let get_identifier = get_identifier

(* let get_string_literal =
   let match_dquote = match_char '"' in
   let match_ascii = satisfy (function ' ' .. '~' -> true | _ -> false) in
   get_token (let+ _ = match_dquote and+ content = match_ascii and+_ = match_dquote in content ) *)

let get_int_literal = map (fun x -> IntLiteral x) get_int

let get_plus = get_symbol "+"

let get_minus = get_symbol "-"

let get_asterisk = get_symbol "*"

let get_slash = get_symbol "/"

let get_equal = get_symbol "="

let get_less = get_symbol "<"

let get_greater = get_symbol ">"

let get_coloncolon = get_symbol "::"

let get_paren_left = get_symbol "("

let get_paren_right = get_symbol ")"

let get_bracket_left = get_symbol "["

let get_bracket_right = get_symbol "]"

let get_arrow = get_symbol "->"

let get_vbar = get_symbol "|"

let get_semicolon = get_symbol ";"

(* Keywords *)

let get_true = get_symbol "true" *> pure (BoolLiteral true)

let get_false = get_symbol "false" *> pure (BoolLiteral false)

let get_bool_literal = get_true <|> get_false

let get_fun = get_symbol "fun"

let get_let = get_symbol "let"

let get_rec = get_symbol "rec"

let get_in = get_symbol "in"

let get_if = get_symbol "if"

let get_then = get_symbol "then"

let get_else = get_symbol "else"

let get_literal = get_int_literal <|> get_bool_literal

let get_varref = map (fun varname -> VarRef varname) get_identifier

(* unit -> (exp Parser.t) * (exp Parser.t) ref *)
let create_parser_forwarded_to_ref _ =
  let dummy_parser =
    let inner = Parser (fun _ -> failwith "unfixed forwarded parser") in
    inner
  in
  let parser_ref = ref dummy_parser in
  let inner = Parser (fun input -> parse !parser_ref input) in
  let wrapper_parser = inner in

  (wrapper_parser, parser_ref)

let (get_exp : exp t), get_exp_ref = create_parser_forwarded_to_ref ()

let get_if_then_else =
  let+ _ = get_if
  and+ predicate = get_exp
  and+ _ = get_then
  and+ then_expr = get_exp
  and+ _ = get_else
  and+ else_expr = get_exp in
  If (predicate, then_expr, else_expr)

let get_let_in =
  let+ _ = get_let
  and+ varname = get_identifier
  and+ _ = get_equal
  and+ varexpr = get_exp
  and+ _ = get_in
  and+ rest = get_exp in
  Let (varname, varexpr, rest)

let get_exp = get_literal <|> get_let_in <|> get_if_then_else <|> get_varref

let () = get_exp_ref := get_exp
