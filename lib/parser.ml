open Containers

let unimplemented name = failwith (name ^ "is not implemented!")

module Result = struct
  type 'a t = ('a * char list) option

  let pp = ""
end

type 'a t = char list -> 'a Result.t

let parse p chars = chars |> p

let parse_string p s = s |> String.to_list |> p

let get_char = function [] -> None | c :: cs -> Some (c, cs)

let map f p chars =
  match parse p chars with
  | None -> None
  | Some (v, chars') -> Some (f v, chars')

let ( <$> ) = map

(** Parser which always succeeds. *)
let pure v chars = Some (v, chars)

(** Parser which always fails. *)
let empty _ = None

let apply (p_f : ('a -> 'b) t) p_a chars =
  match parse p_f chars with
  | None -> None
  | Some (f, chars') -> parse (map f p_a) chars'

let ( <*> ) = apply

let ( <* ) xp yp = (fun x _ -> x) <$> xp <*> yp

let ( *> ) xp yp = (fun _ x -> x) <$> xp <*> yp

let product xp yp = (fun x y -> (x, y)) <$> xp <*> yp

let ( let+ ) x f = map f x

let ( and+ ) xa ya = product xa ya

(** If `xp` suceeds returns its result. Otherwise returns `yp`'s one. *)
let either xp yp chars =
  match parse xp chars with None -> parse yp chars | Some _ as r -> r

(** Binary operator version of `either`. If left parser suceeds returns its result. Otherwise returns right side's one. *)
let ( <|> ) = either

let bind (xp : 'a t) (fp : 'a -> 'b t) chars =
  match parse xp chars with
  | None -> None
  | Some (x, chars') -> parse (fp x) chars'

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

let match_string str =
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

(* let chainl1 x op =
   let rec loop a =
     (let* op = op in
      let* b = x in
      loop (op a b))
     <|> pure a
   in
   x >>= loop *)

let chainl1 x op =
  let rec loop a = op >>= (fun op -> x >>= fun b -> loop (op a b)) <|> pure a in
  x >>= loop

let chainl x op default = chainl1 x op <|> pure default

let match_nat = int_of_string <$> (String.of_list <$> some match_digit)

let match_int = match_nat <|> (( ~- ) <$> match_char '-' *> match_nat)

let match_identifier =
  String.of_list
  <$> (List.cons <$> match_lowercase_ascii <*> many match_alphanumetic_ascii)

let get_token p =
  let spaces = many match_space in
  spaces *> p <* spaces

let get_identifier = get_token match_identifier

let get_unsigned_int = get_token match_nat

let get_int = get_token match_int

let get_symbol s = get_token (match_string s)

let get_list_of_uints =
  let+ _ = get_symbol "["
  and+ n = get_unsigned_int
  and+ ns = many (get_symbol ";" *> get_unsigned_int)
  and+ _ = get_symbol ";" <|> pure ""
  and+ _ = get_symbol "]" in
  n :: ns
