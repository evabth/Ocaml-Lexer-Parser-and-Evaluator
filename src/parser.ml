open Types
open Utils

(* Provided functions - DO NOT MODIFY *)

(* Matches the next token in the list, throwing an error if it doesn't match the given token *)
let match_token (toks : token list) (tok : token) =
  match toks with
  | [] -> raise (InvalidInputException (string_of_token tok))
  | h :: t when h = tok -> t
  | h :: _ ->
      raise
        (InvalidInputException
           (Printf.sprintf "Expected %s from input %s, got %s"
              (string_of_token tok)
              (string_of_list string_of_token toks)
              (string_of_token h)))

(* Matches a sequence of tokens given as the second list in the order in which they appear, throwing an error if they don't match *)
let match_many (toks : token list) (to_match : token list) =
  List.fold_left match_token toks to_match

(* Return the next token in the token list as an option *)
let lookahead (toks : token list) =
  match toks with [] -> None | h :: t -> Some h

(* Return the token at the nth index in the token list as an option*)
let rec lookahead_many (toks : token list) (n : int) =
  match (toks, n) with
  | h :: _, 0 -> Some h
  | _ :: t, n when n > 0 -> lookahead_many t (n - 1)
  | _ -> None

(* Part 2: Parsing expressions *)

let rec parse_expr toks = 
  match lookahead toks with
  |Tok_If -> parse_if toks
  |Tok_Let -> parse_let toks
  |Tok_Fun -> parse_fun toks
  | _ -> parse_or toks


let rec parse_if toks =(*Garanteed that the first element is an if*)
  let (toks1, expr1) = parse_expr (match_token toks Tok_If) in 
  let (toks2, expr2) = parse_expr (match_token toks1 Tok_Then) in
  let (toks3, expr3) = parse_expr (match_token toks2 Tok_Else) in 
  (toks3, If (expr1,expr2,expr3))
    

let rec parse_let toks =
  let toks1 = match_token toks Tok_Let in 
  if lookahead toks1 = Tok_Rec then
    let toks2 = match_token toks1 Tok_Rec in 
    let id = match lookahead toks2 with
    |Tok_ID id -> id
    | _ -> raise InvalidInputException "Parse let failed" in
    let toks3 = match_token toks2 Tok_Equal in
    let (toks4, expr4) = parse_expr toks3 in
    let toks5 = match_token toks4 Tok_In in 
    let (toks6, expr6) = parse_expr toks5 in 
    (toks6, Let(id, true, expr4, expr6))
  else 
    let id = match lookahead toks1 with
    |Tok_ID id -> id
    | _ -> raise InvalidInputException "Parse let failed" in
    let toks2 = match_token toks1 Tok_Equal in
    let (toks3, expr3) = parse_expr toks2 in
    let toks4 = match_token toks3 Tok_In in 
    let (toks5, expr5) = parse_expr toks4 in 
    (toks5, Let(id, false, expr3, expr5))


let rec parse_fun toks = 
  let toks1 = match_token toks Tok_Fun in 
  let id = match lookahead toks1 with 
    |Tok_ID id -> id
    | _ -> raise InvalidInputException "Parse Function failed" in
  let toks2 = match_token toks1 Tok_ID in 
  let toks3 = match_token toks2 Tok_Arrow in
  let (toks4, expr4) = parse_expr toks3 in 
  (toks4, Fun (id,expr4))


let rec parse_or toks = 
  let (toks1, expr1) = parse_and toks in 
  if lookahead toks1 = Tok_Or then
    let toks2 = match_token toks1 Tok_Or in
    let (toks3, expr3) = parse_or toks3 in
    (toks3,Binop(Or, expr1, expr3))
  else 
    (toks1, expr1)


let rec parse_and toks = 
  let (toks1, expr1) = parse_equal toks in 
  if lookahead toks1 = Tok_And then
    let toks2 = match_token toks1 Tok_And in
    let (toks3, expr3) = parse_and toks3 in
    (toks3,Binop(And, expr1, expr3))
  else 
    (toks1, expr1)

let rec parse_equal toks = 
  let (toks1, expr1) = parse_relation toks in 
  if lookahead toks1 = Tok_Equal then
    let toks2 = match_token toks1 Tok_Equal in
    let (toks3, expr3) = parse_and toks3 in
    (toks3,Binop(Equal, expr1, expr3))
  else 
    (toks1, expr1)

let rec parse_relation toks = 
  let (toks1, expr1) = parse_add toks in
  match lookahead toks1 with 
  |Tok_Less -> let (toks2, expr2) = parse_relation (match_token toks1 Tok_Less) in
    (toks2, Binop(Less, expr1, expr2))
  |Tok_Greater -> let (toks2, expr2) = parse_relation (match_token toks1 Tok_Greater) in
    (toks2, Binop(Greater, expr1, expr2))
  |Tok_LessEqual -> let (toks2, expr2) = parse_relation (match_token toks1 Tok_LessEqual) in
    (toks2, Binop(LessEqual, expr1, expr2))
  |Tok_GreaterEqual -> let (toks2, expr2) = parse_relation (match_token toks1 Tok_GreaterEqual) in
    (toks2, Binop(GreaterEqual, expr1, expr2))
  | _ -> (toks1,expr1)

  (*this is where you left off*)
let rec parse_add toks = 
  let (toks1,expr1) = parse_concat toks in 
  match lookahead toks1 with 
  |Tok_Add
  |Tok_Sub
  | _ -> (toks1,expr1)

let rec parse_concat toks =
  failwith "not done"


let rec parse_primary toks = 
  match lookahead toks with
  |Tok_Int num -> (match toks Tok_Int, Int num)
  |Tok_Bool boolean -> (match toks Tok_bool, Bool boolean)
  |Tok_String str -> (match toks Tok_String, String str)
  |Tok_ID id -> (match toks Tok_ID, ID id)
  |Tok_LParen -> let (new_toks, expr) = parse_expr (match_token tok Tok_LParen) in 
    (match_token Tok_RParen, new_expr)
  |_ -> raise InvalidInputException "Parse Primary failed"

  

(* Part 3: Parsing mutop *)

let rec parse_mutop toks = failwith "unimplemented"
