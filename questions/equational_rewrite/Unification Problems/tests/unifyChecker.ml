(* File: unifyCheck.ml *)
(* Author: Elsa L. Gunter *)
(* Copyright: 2016 *)
(* Share and Enjoy *)

open UnifySteps
open Unifyparse


let step_from_string step_string =
  let lexbuf = Lexing.from_string step_string
  in full_input
  (fun lb ->
    match Unifylex.token lb
    with EOF -> raise Unifylex.EndInput
      | r -> r)
  lexbuf

let checkStep problem_string student_string =
  try (full_input_result
         (step_from_string (problem_string ^" "^ student_string)))
  with _ -> (ParseError "Parse Error: Please retype your entry.\n")

let checkAnswer (problem_string, student_string) =
  match checkStep problem_string student_string
  with ParseError parse_err_msg ->
    (true, false, false, student_string, parse_err_msg , 0)
    | Failure failure_msg ->
    (false, false, false, student_string, failure_msg,(-1))
    | Success (is_complete, score) ->
      (false, true, is_complete, student_string, "", score)

