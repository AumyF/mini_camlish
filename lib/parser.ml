open Containers

let unimplemented name = failwith (name ^ "is not implemented!")

type 'a result = ('a * char list) option

type 'a t = Parser of (char list -> 'a result)

let parse (Parser p) chars = chars |> p

let parse_string (Parser p) s = s |> String.to_list |> p

let get_char = Parser (function [] -> None | c :: cs -> Some (c, cs))

let map f p =
  Parser
    (fun chars ->
      match parse p chars with
      | None -> None
      | Some (v, chars') -> Some (f v, chars'))

let ( <$> ) = map

(** Parser which always succeeds. *)
let pure v = Parser (fun chars -> Some (v, chars))

(** Parser which always fails. *)
let empty = Parser (fun _ -> None)

let apply (p_f : ('a -> 'b) t) p_a =
  Parser
    (fun chars ->
      match parse p_f chars with
      | None -> None
      | Some (f, chars') -> parse (map f p_a) chars')

let ( <*> ) = apply

let ( <* ) xp yp = (fun x _ -> x) <$> xp <*> yp

let ( *> ) xp yp = (fun _ x -> x) <$> xp <*> yp

let product xp yp = (fun x y -> (x, y)) <$> xp <*> yp

let ( let+ ) x f = map f x

let ( and+ ) xa ya = product xa ya

(** If `xp` suceeds returns its result. Otherwise returns `yp`'s one. *)
let either xp yp =
  Parser
    (fun chars ->
      match parse xp chars with None -> parse yp chars | Some _ as r -> r)

(** Binary operator version of `either`. If left parser suceeds returns its result. Otherwise returns right side's one. *)
let ( <|> ) = either

let bind (xp : 'a t) (fp : 'a -> 'b t) =
  Parser
    (fun chars ->
      match parse xp chars with
      | None -> None
      | Some (x, chars') -> parse (fp x) chars')

let ( >>= ) = bind

let ( let* ) x f = bind x f

let satisfy predicate =
  let* x = get_char in
  if predicate x then pure x else empty

let get_two = (fun x y -> [ x; y ]) <$> get_char <*> get_char

let match_char ch = satisfy (Char.equal ch)

let match_lowercase_ascii =
  let p = function 'a' .. 'z' -> true | _ -> false in
  satisfy p

let match_capital_ascii =
  let p = function 'A' .. 'Z' -> true | _ -> false in
  satisfy p

let match_digit =
  let p = function '0' .. '9' -> true | _ -> false in
  satisfy p

let match_latin_ascii = match_lowercase_ascii <|> match_capital_ascii

let match_alphanumetic_ascii = match_latin_ascii <|> match_digit

let match_space =
  satisfy (function ' ' | '\t' | '\n' | '\r' -> true | _ -> false)

let get_middle = get_char *> get_char <* get_char

let get_string str =
  let rec f = function
    | [] -> pure []
    | ch :: chars ->
        let* _ = match_char ch and+ _ = f chars in
        pure (ch :: chars)
  in
  str |> String.to_list |> f |> map String.of_list

let some (p : 'a t) =
  let* first = p in
  let purep = map Option.pure p <|> pure None in
  let rec inner acc =
    let* ch = purep in
    match ch with None -> pure acc | Some ch -> inner List.(append acc [ ch ])
  in
  inner [ first ]

let many p = some p <|> pure []

let match_nat = int_of_string <$> (String.of_list <$> some match_digit)

let match_int = match_nat <|> (( ~- ) <$> match_char '-' *> match_nat)

let match_identifier =
  String.of_list
  <$> (List.cons <$> match_lowercase_ascii <*> many match_alphanumetic_ascii)

let get_token p =
  let spaces = many match_space in
  spaces *> p <* spaces
