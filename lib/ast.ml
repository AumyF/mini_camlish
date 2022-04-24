module Expression = struct
  type t =
    | VarRef of string
    | Let of string * t * t
    | IntLiteral of int
    | Plus of t * t
    | Times of t * t
    | Subtract of t * t
    | Div of t * t
    | BoolLiteral of bool
    | If of t * t * t
    | Eq of t * t
    | Function of string * t
    | Apply of t * t
  [@@deriving eq, show]
end

type value =
  | IntVal of int
  | BoolVal of bool
  | FunVal of string * Expression.t * (string * value) list

let emptyenv () = []

(* 環境に変数を追加する *)
let ext env x v = (x, v) :: env

(* 環境に変数が含まれているか探す。なかったら例外 *)
let rec lookup x env =
  match env with
  | [] -> failwith ("unbound variable " ^ x)
  | (y, v) :: t -> if x = y then v else lookup x t

let rec eval3 e env =
  let binop f (e1, e2) env =
    let eval e = eval3 e env in
    let v2 = eval e2 in
    let v1 = eval e1 in
    match (v1, v2) with
    | IntVal n1, IntVal n2 -> IntVal (f n1 n2)
    | _ -> failwith "integer expected"
  in
  let open Expression in
  match e with
  | VarRef x -> lookup x env
  | IntLiteral n -> IntVal n
  | BoolLiteral b -> BoolVal b
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
  | Function (p, body) -> FunVal (p, body, env)
  | Apply (ef, earg) -> (
      let arg = eval3 earg env in
      match eval3 ef env with
      | FunVal (p, body, env_of_fn) -> eval3 body (ext env_of_fn p arg)
      | _ -> failwith "expected function")

(* | _ -> failwith "unimplemented" *)

let eval3_with_emptyenv e = eval3 e []

let string_of_value v =
  match v with
  | IntVal n -> string_of_int n
  | BoolVal b -> string_of_bool b
  | FunVal (p, body, _) -> Printf.sprintf "%s -> %s" p Expression.(show body)
