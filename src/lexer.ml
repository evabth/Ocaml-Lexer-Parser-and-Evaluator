open Types

(* Part 1: Lexer - IMPLEMENT YOUR CODE BELOW *)

let tokenize input = 
  let white_space = Str.regexp " \\|\t\\|\n" in 
  let re_rparen = Str.regexp ")" in
  let re_lparen = Str.regexp "(" in 
  let re_rcurly = Str.regexp "\\}" in 
  let re_lcurly = Str.regexp "\\{" in
  let re_dot = Str.regexp "\\." in
  let re_equal = Str.regexp "=" in 
  let re_not_equal = Str.regexp "<>" in 
  let re_greater = Str.regexp ">" in 
  let re_less = Str.regexp "<" in 
  let re_greater_equal = Str.regexp ">=" in(*be careful when matching this, might be the case that this must be calculated before equals*)
  let re_less_equal = Str.regexp "<=" in 
  let re_or = Str.regexp "||" in
  let re_and = Str.regexp "&&" in
  let re_not = Str.regexp "not\\([ \t\n]\\|$\\)" in
  let re_if = Str.regexp "if\\([ \t\n]\\|$\\)" in 
  let re_then = Str.regexp "then\\([ \t\n]\\|$\\)" in 
  let re_else = Str.regexp "else\\([ \t\n]\\|$\\)" in 
  let re_add = Str.regexp "\\+" in
  let re_sub = Str.regexp "-" in 
  let re_mult = Str.regexp "\\*" in 
  let re_div = Str.regexp "/" in 
  let re_concat = Str.regexp "\\^" in 
  let re_let = Str.regexp "let\\([ \t\n]\\|$\\)" in 
  let re_rec = Str.regexp "rec\\([ \t\n]\\|$\\)" in 
  let re_in = Str.regexp "in\\([ \t\n]\\|$\\)" in 
  let re_def = Str.regexp "def\\([ \t\n]\\|$\\)" in 
  let re_fun = Str.regexp "fun\\([ \t\n]\\|$\\)" in
  let re_arrow = Str.regexp "->" in 
  let re_pos_int = Str.regexp "[0-9]+" in
  let re_neg_int = Str.regexp "(\\(-[0-9]+\\))" in
  let re_bool = Str.regexp "\\(true\\|false\\)\\([ \t\n]\\|$\\)" in 
  let re_string = Str.regexp "\"\\([^\"]*\\)\"" in 
  let re_id = Str.regexp "[a-zA-Z][a-zA-Z0-9]*" in
  let re_doublesemi = Str.regexp ";;" in 
  let re_semi = Str.regexp ";" in 

  let rec tok pos = 

    if pos >= String.length input then
      []

    else if (Str.string_match white_space input pos) then 
      tok (pos+1)

    else if (Str.string_match re_pos_int input pos) then 
      let token = int_of_string (Str.matched_string input) in 
      (Tok_Int token) :: tok (Str.match_end ())

    else if (Str.string_match re_neg_int input pos) then  (*fix this for negative numbers *)
      let token = int_of_string (Str.matched_group 1 input) in 
      (Tok_Int token) :: tok (Str.match_end ())

    else if (Str.string_match re_rparen input pos) then
      (Tok_RParen) :: tok (pos+1)

    else if  (Str.string_match re_lparen input pos ) then 
      (Tok_LParen) :: tok (pos+1)
    
    else if (Str.string_match re_rcurly input pos) then 
      (Tok_RCurly) :: tok (pos+1)
    
    else if (Str.string_match re_lcurly input pos) then
      (Tok_LCurly) :: tok (pos+1)

    else if (Str.string_match re_dot input pos) then 
      (Tok_Dot) :: tok (pos+1)

    else if (Str.string_match re_equal input pos) then 
      (Tok_Equal) :: tok (pos+1) 

    else if  (Str.string_match re_equal input pos) then 
      (Tok_Equal) :: tok (pos+1)

    else if (Str.string_match re_not_equal input pos ) then 
      (Tok_NotEqual) :: tok (pos+2)

    else if (Str.string_match re_greater_equal input pos) then  (*switched the position of the evaluation here in order to make sure that it is not wrong*)
      (Tok_GreaterEqual) :: tok  (pos+2)

    else if (Str.string_match re_less_equal input pos ) then 
      (Tok_LessEqual) :: tok (pos+2)

    else if (Str.string_match re_greater input pos ) then 
      (Tok_Greater) :: tok (pos+1)

    else if (Str.string_match re_less input pos ) then 
      (Tok_Less) :: tok (pos+1)
      
    else if (Str.string_match re_or input pos ) then 
      (Tok_Or) :: tok (pos+2)

    else if (Str.string_match re_and input pos ) then  
      (Tok_And) :: tok (pos+2)

    else if (Str.string_match re_not input pos) then 
      (Tok_Not) :: tok (pos+3)
    
    else if (Str.string_match re_if input pos ) then 
      (Tok_If) :: tok (pos+2)
    
    else if (Str.string_match re_then input pos ) then 
      (Tok_Then) :: tok (pos + 4)  

    else if (Str.string_match re_else input pos ) then 
      (Tok_Else) :: tok (pos + 4) 

    else if (Str.string_match re_arrow input pos) then
      (Tok_Arrow) :: tok (pos + 2)    

    else if (Str.string_match re_add input pos ) then 
      (Tok_Add) :: tok (pos + 1)
    
    else if (Str.string_match re_sub input pos) then
      (Tok_Sub) :: tok (pos +1)

    else if (Str.string_match re_mult input pos) then
      (Tok_Mult) :: tok (pos + 1)

    else if (Str.string_match re_div input pos) then 
      (Tok_Div) :: tok (pos + 1)

    else if (Str.string_match re_concat input pos) then 
      (Tok_Concat) :: tok (pos + 1)

    else if (Str.string_match re_let input pos ) then 
      (Tok_Let) :: tok (pos + 3)

    else if (Str.string_match re_rec input pos) then 
      (Tok_Rec) :: tok (pos + 3)

    else if (Str.string_match re_in input pos ) then
      (Tok_In) :: tok (pos + 2)

    else if (Str.string_match re_def input pos ) then
      (Tok_Def) :: tok (pos + 3)
    
    else if (Str.string_match re_fun input pos) then 
      (Tok_Fun) :: tok (pos + 3)

    else if (Str.string_match re_bool input pos ) then 
      let token = bool_of_string (Str.matched_group 1 input) in
      (Tok_Bool token) :: tok (Str.match_end ())

    else if (Str.string_match re_string input pos) then 
      let token = (Str.matched_group 1 input) in 
      (Tok_String token) :: tok (Str.match_end ())

    else if (Str.string_match re_id input pos ) then 
      let token = (Str.matched_string input) in 
      (Tok_ID token) :: tok (Str.match_end ())
      
    else if (Str.string_match re_doublesemi input pos) then 
      (Tok_DoubleSemi) :: tok (pos + 2)

    else if (Str.string_match re_semi input pos) then 
      (Tok_Semi) :: tok (pos + 1)

    else raise (InvalidInputException "String Cannot be tokenized") in 
  tok 0





  
  
  
  
  
  
  

  
