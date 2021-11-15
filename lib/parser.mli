val unimplemented : string -> 'a

type 'a result = ('a * char list) option

type 'a t = Parser of (char list -> 'a result)

val parse : 'a t -> char list -> 'a result

val parse_string : 'a t -> string -> 'a result

val get_char : char t

val map : ('a -> 'b) -> 'a t -> 'b t

val ( <$> ) : ('a -> 'b) -> 'a t -> 'b t

val pure : 'a -> 'a t

val empty : 'a t

val apply : ('a -> 'b) t -> 'a t -> 'b t

val ( <*> ) : ('a -> 'b) t -> 'a t -> 'b t

val ( <* ) : 'a t -> 'b t -> 'a t

val ( *> ) : 'a t -> 'b t -> 'b t

val product : 'a t -> 'b t -> ('a * 'b) t

val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t

val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t

val either : 'a t -> 'a t -> 'a t

val ( <|> ) : 'a t -> 'a t -> 'a t

val bind : 'a t -> ('a -> 'b t) -> 'b t

val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t

val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t

val satisfy : (char -> bool) -> char t

val get_two : char list t

val match_char : char -> char t

val match_lowercase_ascii : char t

val match_capital_ascii : char t

val match_digit : char t

val match_latin_ascii : char t

val match_alphanumetic_ascii : char t

val match_space : char -> bool

val get_middle : char t

val get_string : string -> string t

val many : 'a t -> 'a list t

val some : 'a t -> 'a list t

val match_nat : int t

val match_int : int t

val match_identifier : string t
