(* File: one_step_cps_trans_rules.ml *)
(* Author: Elsa L. Gunter *)
(* Share and Enjoy *)

open Picoml_exp_and_cps
open Picoml_cps_parse
open Picoml_cps_lex
open One_step_cps_trans
        
let exp_from_string prompt_string =
  let lexbuf = Lexing.from_string prompt_string
  in (*Picoml_cps_parse.*)expression (*Picoml_cps_lex.*)token lexbuf
let cont_from_string prompt_string =
  let lexbuf = Lexing.from_string prompt_string
  in (*Picoml_cps_parse.*)continuation token lexbuf

let mk_rule (name, exp_string, side_cond) =
  let input_exp = exp_from_string exp_string in
  let k = ContMetaVar in
  let step = top_one_step_cps_trans input_exp k in
  (name^":\n "^(string_of_exp_cps (CPS_Trans(input_exp,k)))^
      " => "^(string_of_exp_cps step)^"\n"^
      (match side_cond
       with None -> "\n"
         | Some msg -> ("  Provided: "^msg^"\n")))

let cps_trans_rules =
  List.map mk_rule
    [("CPS_Trans_Var", "x", Some "x is a variable");
     ("CPS_Trans_Const", "c", Some "c is a constant");
     ("CPS_Trans_If", "if e1 then e2 else e3", Some "a is fresh for e2, e3 and K");
     ("CPS_Trans_App", "e1 e2", Some "a is fresh for e1 and K, and b is fresh for a and K");
     ("CPS_Trans_Binop", "e1 + e2", Some "a is fresh for e1 and K, and b is fresh for a and K,\n    where + stands for any binary primitive operator");
     ("CPS_Trans_Monop", "~ e1", Some "a is fresh for K, where ~ stands for any unary operator");
     ("CPS_Trans_Fun", "fun x -> e", None);
     ("CPS_Trans_LetIn", "let x = e1 in e2", None);
     ("CPS_Trans_LetRecIn", "let rec f x = e1 in e2", None)]
