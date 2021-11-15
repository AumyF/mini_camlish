type exp =
  | VarRef of string
  | Let of string * exp * exp
  | IntLiteral of int
  | Plus of exp * exp
  | Times of exp * exp
  | Subtract of exp * exp
  | Div of exp * exp
  | BoolLiteral of bool
  | If of exp * exp * exp
  | Eq of exp * exp

type value = IntVal of int | BoolVal of bool

let emptyenv () = []

(* 環境に変数を追加する *)
let ext env x v = (x, v) :: env

(* 環境に変数が含まれているか探す。なかったら例外 *)
let rec lookup x env =
  match env with
  | [] -> failwith "unbound variable"
  | (y, v) :: t -> if x = y then v else lookup x t

(* let rec eval1 e =
   match e with
   | IntLiteral n -> n
   | Plus (e1, e2) -> eval1 e1 + eval1 e2
   | Times (e1, e2) -> eval1 e1 * eval1 e2
   | Subtract (e1, e2) -> eval1 e1 - eval1 e2
   | Div (e1, e2) -> eval1 e1 / eval1 e2 *)
(* | _ -> failwith "unimplemented expression" *)

let rec eval2 e =
  let binop f (e1, e2) =
    match (eval2 e1, eval2 e2) with
    | IntVal n1, IntVal n2 -> IntVal (f n1 n2)
    | _ -> failwith "integer expected"
  in
  match e with
  | IntLiteral n -> IntVal n
  | Plus (e1, e2) -> binop ( + ) (e1, e2)
  | Times (e1, e2) -> binop ( * ) (e1, e2)
  | Subtract (e1, e2) -> binop ( - ) (e1, e2)
  | Div (e1, e2) ->
      binop
        (fun n1 n2 -> if n2 = 0 then failwith "division by zero" else n1 / n2)
        (e1, e2)
  | Eq (e1, e2) -> (
      match (eval2 e1, eval2 e2) with
      | IntVal n1, IntVal n2 -> BoolVal (n1 = n2)
      | BoolVal n1, BoolVal n2 -> BoolVal (n1 = n2)
      | _ -> failwith "types unmatch")
  | If (predicate, then_exp, else_exp) -> (
      match eval2 predicate with
      | BoolVal true -> eval2 then_exp
      | BoolVal false -> eval2 else_exp
      | _ -> failwith "predicate is not of type bool")
  | _ -> failwith ""

let rec eval3 e env =
  let binop f (e1, e2) env =
    let eval e = eval3 e env in
    match (eval e1, eval e2) with
    | IntVal n1, IntVal n2 -> IntVal (f n1 n2)
    | _ -> failwith "integer expected"
  in
  match e with
  | VarRef x -> lookup x env
  | IntLiteral n -> IntVal n
  | Plus (e1, e2) -> binop ( + ) (e1, e2) env
  | Times (e1, e2) -> binop ( * ) (e1, e2) env
  | Subtract (e1, e2) -> binop ( - ) (e1, e2) env
  | Div (e1, e2) ->
      binop
        (fun n1 n2 -> if n2 = 0 then failwith "division by zero" else n1 / n2)
        (e1, e2) env
  | Eq (e1, e2) -> (
      match (eval3 e1 env, eval3 e2 env) with
      | IntVal n1, IntVal n2 -> BoolVal (n1 = n2)
      | BoolVal n1, BoolVal n2 -> BoolVal (n1 = n2)
      | _ -> failwith "types unmatch")
  | If (predicate, then_exp, else_exp) -> (
      match eval3 predicate env with
      | BoolVal true -> eval3 then_exp env
      | BoolVal false -> eval3 else_exp env
      | _ -> failwith "predicate is not of type bool")
  | Let (varname, varexp, rest) ->
      let env = ext env varname (eval3 varexp env) in
      eval3 rest env
  | _ -> failwith ""

let eval3_with_emptyenv e = eval3 e []

let string_of_value v =
  match v with IntVal n -> string_of_int n | BoolVal b -> string_of_bool b
