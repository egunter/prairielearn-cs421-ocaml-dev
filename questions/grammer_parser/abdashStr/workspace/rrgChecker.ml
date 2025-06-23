(* File: rrgCheck.ml *)
(* Author: Elsa L. Gunter *)
(* Copyright: 2016 *)
(* Share and Enjoy *)

open Rrg
open Rrgparse
open Rrglex

let get_all_token_options s =
    let b = Lexing.from_string (s^"\n") in
    let rec g () =
        match (try Some (token b) with _ -> None) with
            Some EOF -> []
            | None -> [None]
            | t -> t :: g ()
        in
    g ();;

let grammar_from_string rrg_string =
    let lex = Lexing.from_string rrg_string
    in Rrgparse.main Rrglex.token lex

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
    let problem_grammar = grammar_from_string problem_string
    in
    (try
       let student_grammar = grammar_from_string student_string
       in
       let joint_alphabet =
         set_union terminal_compare (get_alphabet (fst student_grammar))
           (get_alphabet (fst problem_grammar))
       in
       (match
           build_and_check_product true (dfa_of_rrg joint_alphabet problem_grammar)
             (dfa_of_rrg joint_alphabet student_grammar)
        with (_, [], [], _) ->
            (false, true, true, student_string, "Correct.\n", problem_value)
          | (_,path::_, _, _) ->
            (false, false, false, student_string,
             ("The string "^(string_of_path (List.rev path))^
              " is accecpted by the given grammar, which it should not.\n"),
             (-1))
          | (_,[], path::_, _) ->
            (false, false, false, student_string,
             ("The string "^(string_of_path (List.rev path))^
              " is not accecpted by the given grammar, but is required to.\n"),
             (-1))
       )
     with
       | _ -> (true, false, false, student_string, 
               "Parse Error: Please retype your entry.\n", 0)
    )
  with _ -> (true, false, false, student_string,
             "This should not be possible.  Please alert staff. "
            (* ^ "The string in question is \"" ^ problem_string ^ "\"\n"*), 0)
