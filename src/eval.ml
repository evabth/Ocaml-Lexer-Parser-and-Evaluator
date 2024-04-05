open Types

(* Provided functions - DO NOT MODIFY *)

(* Adds mapping [x:v] to environment [env] *)
let extend env x v = (x, ref v) :: env

(* Returns [v] if [x:v] is a mapping in [env]; uses the
   most recent if multiple mappings for [x] are present *)
let rec lookup env x =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value) :: t -> if x = var then !value else lookup t x

(* Creates a placeholder mapping for [x] in [env]; needed
   for handling recursive definitions *)
let extend_tmp env x = (x, ref (Int 0)) :: env

(* Updates the (most recent) mapping in [env] for [x] to [v] *)
let rec update env x v =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value) :: t -> if x = var then value := v else update t x v

(* Part 1: Evaluating expressions *)

(* Evaluates MicroCaml expression [e] in environment [env],
   returning an expression, or throwing an exception on error *)

let rec record_lookup var llist = match llist with 
  |(id, value) :: tl -> if var = id then value else record_lookup var tl
  | [] -> raise (SelectError "Label not found")


let rec eval_expr env e =
  match e with 
  | Int num -> Int num
  | Bool boolean -> Bool boolean
  | String str -> String str
  | ID id -> (lookup env id)
  | Fun (var, expr) -> Closure (env, var, expr)
  | Not (expr) -> let expr1 = (eval_expr env expr) in 
    (match expr1 with 
    |Bool boolean -> Bool ( not boolean)
    | _ -> raise (TypeError "Should be boolean") )

  | Binop (op, expr1, expr2) -> let expr3 = eval_expr env expr1 in 
    let expr4 = eval_expr env expr2 in 
    (match expr3 with
    |Int num1 ->
      (match expr4 with 
      |Int num2 -> 
        (match op with
        | Add -> Int (num1+num2)
        | Sub -> Int (num1-num2)
        | Mult -> Int (num1*num2)
        | Div -> if num2 != 0 then Int (num1/num2) else raise (DivByZeroError)
        | Greater -> Bool (num1 > num2)
        | Less -> Bool (num1 < num2)
        | GreaterEqual -> Bool (num1 >= num2)
        | LessEqual -> Bool (num1 <= num2)
        | Equal -> Bool (num1 = num2)
        | NotEqual -> Bool (num1 != num2)
        | _ -> raise (TypeError "eval Binop failed"))
      | _ -> raise (TypeError "eval Binop failed")) 
    |String str1 -> 
      (match expr4 with 
      |String str2 ->
        (match op with 
        |Concat -> String (str1 ^ str2)
        | _ -> raise (TypeError "eval binop failed") )
      | _ -> raise (TypeError "eval binop failed") ) 

    |Bool boolean1 ->
      (match expr4 with 
      |Bool boolean2 ->
        (match op with 
        | Or -> Bool (boolean1 || boolean2)
        | And -> Bool (boolean1 && boolean2)
        | _ -> raise (TypeError "eval binop failed") )
      |_ |Int _|String _|ID _|Fun (_, _)|Not _|Binop (_, _, _)|If (_, _, _)|
      App (_, _)|Let (_, _, _, _)|Closure (_, _, _)|Record _|Select (Lab _, _)  -> raise (TypeError "eval binop failed") )

    |_ |Int _|String _|ID _|Fun (_, _)|Not _|Binop (_, _, _)|If (_, _, _)|
    App (_, _)|Let (_, _, _, _)|Closure (_, _, _)|Record _|Select (Lab _, _) -> raise (TypeError "eval Binop failed") )
    
    

  | If (expr1, expr2, expr3) -> 
    (match (eval_expr env expr1) with 
    |Bool boolean ->
      if boolean then (eval_expr env expr2) else (eval_expr env expr3)
    | _ -> raise (TypeError "eval binop failed") )
  | App (expr1, expr2)->  
    (match eval_expr env expr1 with 
    |Closure (fun_env, fun_var, fun_expr) -> 
      let arg = eval_expr env expr2 in 
      eval_expr (extend fun_env fun_var arg) fun_expr
    | _ -> raise (TypeError "not a function") )

  | Let (var, boolean, expr1, expr2) -> 
    if boolean then 
      failwith "hard part to code"(* recursive case*) (*i dont know what to write here*)
    else let env1 = extend env var (eval_expr env expr1) in 
    (eval_expr env1 expr2)
  | Closure (env, var, expr) -> Closure (env, var, expr)
  | Record list -> Record (list)
  | Select (label, expr) -> (match (eval_expr env expr) with 
    |Record llist -> (record_lookup label llist)
    |_ |Int _|String _|ID _|Fun (_, _)|Not _|Binop (_, _, _)|If (_, _, _)|
      App (_, _)|Let (_, _, _, _)|Closure (_, _, _)|Record _|Select (Lab _, _) -> raise (TypeError "Not a record") 
  )

(* Part 2: Evaluating mutop directive *)

(* Evaluates MicroCaml mutop directive [m] in environment [env],
   returning a possibly updated environment paired with
   a value option; throws an exception on error *)
let eval_mutop env m = failwith "unimplemented"
