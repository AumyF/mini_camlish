open Containers

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

let pure v chars = Some (v, chars)

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

let get_two = (fun x y -> [ x; y ]) <$> get_char <*> get_char

let get_middle = get_char *> get_char <* get_char
