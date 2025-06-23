(* File: regexCheck.ml *)
(* Author: Elsa L. Gunter *)
(* Copyright: 2016 *)
(* Share and Enjoy *)

open Regexast
open Regexint

let regexp_from_string regexp_string =
    let lex = Lexing.from_string regexp_string
    in Regexparse.exp Regexlex.token lex


let checkAnswer (problem_string, student_string, problem_value) =
(* returns
(is_parse_error,
 is_correct,
 is_the_last_step,
 student_string,
 error_message,
 score)
*)
  try
    let problem_regexp = regexp_from_string problem_string
    in
    (try
       let student_regexp = regexp_from_string student_string
       in
       let stu_string = string_of_regex student_regexp
       in
       (match equiv problem_regexp student_regexp
        with check ->
          let msg = string_of_check check in
          (match check
           with Equiv _ ->
             (false, true, true, stu_string, msg, problem_value)
             | _ -> (false, false, false, stu_string, msg, (-1))
          )
       )
     with
       | _ -> (true, false, false, student_string, 
               "Parse Error: Please retype your entry.\n", 0)
    )
  with _ -> (true, false, false, student_string,
             "This should not be possible.  Please alert staff. "
             ^ "The string in question is \"" ^ problem_string ^ "\"\n", 0)
