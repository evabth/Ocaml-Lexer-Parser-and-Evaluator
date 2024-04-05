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

  let rec parse_record_body toks = 
    match lookahead toks with 
    |Some(Tok_ID id) -> let toks1 = match_token (match_token toks (Tok_ID id)) Tok_Equal in 
      let (toks2, expr2) = parse_expr toks1 in 
      (match lookahead toks2 with 
      | Some(Tok_Semi) -> 
        (match parse_record_body (match_token toks2 Tok_Semi) with 
        |(toks3, Record (llist)) -> (toks3, Record (((Lab id, expr2) :: llist)) ) 
        |(_, _) -> raise (InvalidInputException "parse record body error") )

      | Some(_) -> (toks2, Record([(Lab id, expr2)]))
      | None -> (toks2, Record([(Lab id, expr2)])) )

    |Some (Tok_RCurly) -> (toks,Record([]))
      
    | Some(_) -> raise (InvalidInputException "Wrong record input") 
    | None -> raise (InvalidInputException "Wrong record input") in

  let rec parse_record toks = 
    let (toks1, expr1) = parse_record_body toks in 
      (match_token toks1 Tok_RCurly , expr1 ) in 

  let rec parse_primary toks = 
    match lookahead toks with
    |Some(Tok_Int num) -> (match_token toks (Tok_Int num), Int num)
    |Some(Tok_Bool boolean) -> (match_token toks (Tok_Bool boolean), Bool boolean)
    |Some(Tok_String str) -> (match_token toks (Tok_String str), String str)
    |Some(Tok_ID id) -> (match_token toks (Tok_ID id), ID id)
    |Some(Tok_LParen) -> let (new_toks, expr) = parse_expr (match_token toks Tok_LParen) in (*check parse expr*)
      (match_token new_toks Tok_RParen, expr)(*this isn't done*)
    |Some(Tok_LCurly) -> parse_record (match_token toks Tok_LCurly)
    |Some(_) -> raise (InvalidInputException "Parse Primary failed") 
    |None -> raise (InvalidInputException "Parse Primary failed")  in 

  let rec parse_select toks =
    let (toks1, expr1) = parse_primary toks in 
    match lookahead toks1 with 
    |Some(Tok_Dot) -> let toks2 = match_token toks1 Tok_Dot in
      (match lookahead toks2 with 
      | Some(Tok_ID id) -> (match_token toks2 (Tok_ID id), Select(Lab id, expr1))
      | Some(_) -> raise (InvalidInputException "not valid select")
      | None -> raise (InvalidInputException "not valid select") )
    | Some(_) -> (toks1,expr1) 
    | None -> (toks1,expr1) in (*dont know what to do with the none statement*)


  let rec parse_app toks =
    let (toks1, expr1) = parse_select toks in 
    match lookahead toks1 with 
    |Some (Tok_Int _ )-> let (toks2, expr2) = parse_primary toks1 in
      (toks2, App(expr1,expr2))
    |Some(Tok_Bool _) -> let (toks2, expr2) = parse_primary toks1 in
      (toks2, App(expr1,expr2))
    |Some (Tok_String _) -> let (toks2, expr2) = parse_primary toks1 in
      (toks2, App(expr1,expr2))
    |Some (Tok_ID _ )-> let (toks2, expr2) = parse_primary toks1 in
      (toks2, App(expr1,expr2))
    |Some (Tok_LParen ) -> let (toks2, expr2) = parse_primary toks1 in
      (toks2, App(expr1,expr2))
    |Some (Tok_LCurly )-> let (toks2, expr2) = parse_primary toks1 in
      (toks2, App(expr1,expr2))
    | Some(_) -> (toks1,expr1) 
    | None -> (toks1,expr1) in 


  let rec parse_unary toks =
    match lookahead toks with 
    | Some(Tok_Not) ->  let (toks1, expr1) = parse_unary (match_token toks Tok_Not) in 
      (toks1, Not(expr1))
    | Some(_) -> parse_app toks 
    | None -> raise (InvalidInputException "parse_unary failed") in  (*this could be an issue please check if "" or the empty string is a valid expression and what error it should have*)
    


  let rec parse_concat toks =
    let (toks1,expr1) = parse_unary toks in 
    match lookahead toks1 with 
    |Some(Tok_Concat) -> let (toks2,expr2) = parse_concat (match_token toks1 Tok_Concat) in
      (toks2, Binop(Concat, expr1, expr2))
    | Some(_) -> (toks1, expr1) 
    | None -> (toks1,expr1) in 


  let rec parse_mult toks = 
    let (toks1, expr1) = parse_concat toks in 
    match lookahead toks1 with 
    |Some (Tok_Mult) -> let (toks2, expr2) = parse_mult (match_token toks1 Tok_Mult) in
      (toks2, Binop(Mult,expr1,expr2))
    |Some (Tok_Div) -> let (toks2,expr2) = parse_mult (match_token toks1 Tok_Div) in
      (toks2, Binop(Div,expr1,expr2))
    |Some (_) -> (toks1,expr1) 
    |None -> (toks1, expr1) in 


  let rec parse_add toks = 
    let (toks1,expr1) = parse_mult toks in 
    match lookahead toks1 with 
    |Some(Tok_Add) -> let (toks2, expr2) = parse_add (match_token toks1 Tok_Add) in 
      (toks2, Binop(Add, expr1, expr2))
    |Some(Tok_Sub) -> let (toks2, expr2) = parse_add (match_token toks1 Tok_Sub) in 
      (toks2, Binop(Sub, expr1, expr2))
    | Some(_) -> (toks1,expr1) 
    | None -> (toks1,expr1) in


  let rec parse_relation toks = 
    let (toks1, expr1) = parse_add toks in
    match lookahead toks1 with 
    |Some(Tok_Less) -> let (toks2, expr2) = parse_relation (match_token toks1 Tok_Less) in
      (toks2, Binop(Less, expr1, expr2))
    |Some(Tok_Greater) -> let (toks2, expr2) = parse_relation (match_token toks1 Tok_Greater) in
      (toks2, Binop(Greater, expr1, expr2))
    |Some(Tok_LessEqual) -> let (toks2, expr2) = parse_relation (match_token toks1 Tok_LessEqual) in
      (toks2, Binop(LessEqual, expr1, expr2))
    |Some(Tok_GreaterEqual) -> let (toks2, expr2) = parse_relation (match_token toks1 Tok_GreaterEqual) in
      (toks2, Binop(GreaterEqual, expr1, expr2))
    | Some(_) -> (toks1,expr1) 
    |None -> (toks1,expr1) in 


  let rec parse_equal toks = 
    let (toks1, expr1) = parse_relation toks in 
    match lookahead toks1 with 
    |Some(Tok_Equal) -> let (toks2, expr2) = parse_equal (match_token toks1 Tok_Equal) in
      (toks2,Binop(Equal, expr1, expr2))
    |Some(Tok_NotEqual) -> let (toks2, expr2) = parse_equal (match_token toks1 Tok_NotEqual) in
    (toks2,Binop(NotEqual, expr1, expr2))
    | Some(_) -> (toks1, expr1)
    | None -> (toks1, expr1) in 

  
  let rec parse_and toks = 
    let (toks1, expr1) = parse_equal toks in 
    if lookahead toks1 = Some(Tok_And) then
      let toks2 = match_token toks1 Tok_And in
      let (toks3, expr3) = parse_and toks2 in
      (toks3 ,Binop(And, expr1, expr3))
    else 
      (toks1, expr1) in 


  let rec parse_or toks = 
    let (toks1, expr1) = parse_and toks in 
    if lookahead toks1 = Some(Tok_Or) then
      let toks2 = match_token toks1 Tok_Or in
      let (toks3, expr3) = parse_or toks2 in
      (toks3,Binop(Or, expr1, expr3))
    else 
      (toks1, expr1) in 


  let rec parse_fun toks = 
    let toks1 = match_token toks Tok_Fun in 
    let id = match lookahead toks1 with 
      |Some(Tok_ID id) -> id
      | Some(_) -> raise (InvalidInputException "Parse Function failed")
      | None -> raise (InvalidInputException "Parse Function failed") in 
    let toks2 = match_token toks1 (Tok_ID id) in 
    let toks3 = match_token toks2 Tok_Arrow in
    let (toks4, expr4) = parse_expr toks3 in 
    (toks4, Fun (id,expr4)) in 




  let rec parse_let toks =
    let toks1 = match_token toks Tok_Let in 
    if lookahead toks1 = Some(Tok_Rec) then
      let toks2 = match_token toks1 Tok_Rec in 
      let id = match lookahead toks2 with
      |Some(Tok_ID id) -> id
      | Some(_) -> raise (InvalidInputException "Parse let failed") 
      | None -> raise (InvalidInputException "Parse let failed") in 
      let toks3 = match_token (match_token toks2 (Tok_ID id)) Tok_Equal in
      let (toks4, expr4) = parse_expr toks3 in
      let toks5 = match_token toks4 Tok_In in 
      let (toks6, expr6) = parse_expr toks5 in 
      (toks6, Let(id, true, expr4, expr6))
    else 
      let id = match lookahead toks1 with
      | Some(Tok_ID id) -> id
      | Some(_) -> raise (InvalidInputException "Parse let failed") 
      | None -> raise (InvalidInputException "Parse let failed") in 
      let toks2 = match_token (match_token toks1 (Tok_ID id)) Tok_Equal in
      let (toks3, expr3) = parse_expr toks2 in
      let toks4 = match_token toks3 Tok_In in 
      let (toks5, expr5) = parse_expr toks4 in 
      (toks5, Let(id, false, expr3, expr5)) in 


  let rec parse_if toks =(*Garanteed that the first element is an if*)
    let (toks1, expr1) = parse_expr (match_token toks Tok_If) in 
    let (toks2, expr2) = parse_expr (match_token toks1 Tok_Then) in
    let (toks3, expr3) = parse_expr (match_token toks2 Tok_Else) in 
    (toks3, If (expr1,expr2,expr3)) in 




  
  match lookahead toks with
  |Some(Tok_If) -> parse_if toks
  |Some(Tok_Let) -> parse_let toks
  |Some(Tok_Fun) -> parse_fun toks
  | _ -> parse_or toks



(* Part 3: Parsing mutop *)

let rec parse_mutop toks = 
  match lookahead toks with
  | Some(Tok_DoubleSemi) -> (match_token toks Tok_DoubleSemi, NoOp )
  | Some(Tok_Def) -> let toks1 = (match_token toks Tok_Def) in 
    (match lookahead toks1 with
    | Some(Tok_ID id) -> 
      let (toks2, expr2) = parse_expr (match_token (match_token toks1 (Tok_ID id)) Tok_Equal) in 
      (match_token toks2 Tok_DoubleSemi, Def (id, expr2))
    | Some(_) -> raise (InvalidInputException "no id")
    | None -> raise (InvalidInputException "no id") )

  |Some(Tok_DoubleSemi) ->  (match_token toks Tok_DoubleSemi, NoOp)
  |Some(_) -> let (toks1, expr1) = parse_expr toks in 
    (match_token toks1 Tok_DoubleSemi, Expr(expr1))
  |None -> raise (InvalidInputException "No input")
