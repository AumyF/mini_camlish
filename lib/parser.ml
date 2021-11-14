open Containers

type 'a result = ('a * char list) option

type 'a t = char list -> 'a result

let parse p s = p @@ String.to_list s

let get_char = function [] -> None | c :: cs -> Some (c, cs)

let map f p chars =
  match p chars with None -> None | Some (v, chars') -> Some (f v, chars')

let ( <$> ) = map

let pure v chars = Some (v, chars)

let apply (p_f : ('a -> 'b) t) p_a chars =
  match p_f chars with None -> None | Some (f, chars') -> (map f p_a) chars'

let ( <*> ) = apply

let ( <* ) xp yp = (fun x _ -> x) <$> xp <*> yp

let ( *> ) xp yp = (fun _ x -> x) <$> xp <*> yp

let product xp yp = (fun x y -> (x, y)) <$> xp <*> yp

let get_two = (fun x y -> [ x; y ]) <$> get_char <*> get_char

let get_middle = get_char *> get_char <* get_char
