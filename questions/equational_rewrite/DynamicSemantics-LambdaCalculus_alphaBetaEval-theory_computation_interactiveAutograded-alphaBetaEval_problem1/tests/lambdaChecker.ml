(* File: lambdaChecker.ml *)
(* Author: Elsa L. Gunter *)
(* Copyright: 2016 *)
(* Share and Enjoy *)

open Lambda
open Lambda_parse
open Lambda_lex
open Solution

let parse s = Lambda_parse.exp Lambda_lex.token (Lexing.from_string s);; 

let step_from_string step_string =
  let lexbuf = Lexing.from_string step_string
  in Lambda_parse.input (fun lb -> Lambda_lex.token lb) lexbuf

let checkStep
    lambda evalf alpha_value beta_value problem_string student_string =
  try
    (match step_from_string (problem_string ^" "^ student_string)
          (*     with (lhs, ((tag,rhs),[])) ->*)
     with ((_,lhs),(tag,rhs)::[]) ->
          checkInput lambda evalf alpha_value beta_value (lhs, tag, rhs)
       | (_,_) ->
       ParseError
         ("Input Error: Multiple steps of evaluation in the same round of input.\n"^
         "Please retype your entry, putting in one step at a time and pushing\n"^
         "the Check botton after each step.\n"))
  with _ -> (ParseError "Parse Error: Please retype your entry.\n")


let internalCheckAnswer lambda evalf alpha_value beta_value (problem_string, student_string) =
  match checkStep lambda evalf alpha_value beta_value problem_string student_string
  with ParseError parse_err_msg ->
    (true, false, false, student_string, parse_err_msg , 0)
    | Failed failure_msg ->
    (false, false, false, student_string, failure_msg,(-1))
    | Success (is_complete, score) ->
      (false, true, is_complete, student_string, "", score)

let checkEagerAnswer (problem_string, student_string) =
  internalCheckAnswer (*"$\\lambda$"*) "%" Eager Solution.alphaValue Solution.betaValue (problem_string, student_string)

let checkLazyAnswer (problem_string, student_string) =
  internalCheckAnswer (*"$\\lambda$"*) "%" Lazy Solution.alphaValue Solution.betaValue (problem_string, student_string)

let checkAlphaBetaAnswer (problem_string, student_string) =
  internalCheckAnswer (*"$\\lambda$"*) "%" AlphaBeta Solution.alphaValue Solution.betaValue (problem_string, student_string)
(*
"(% x. x (% y. x y))((% u. u)(%w. w)) -b-> (%x. x(% y. x y))(%w. w)"
        *)
