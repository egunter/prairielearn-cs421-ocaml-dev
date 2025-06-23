(*
File: picomlCPSTransGrader.ml
Author: Elsa L. Gunter 
Share and Enjoy
*)

open Utils
open Gen_repl
open Gen_env
open Picoml_exp_and_cps
open Picoml_cps_parse
open Picoml_cps_lex
open One_step_cps_trans
open One_step_cps_trans_rules
open Problem_points
open Problem


type 'a parse = Ok of 'a | ParseError of string

type state =
    {history_string:string; rev_student_inputs:string list;
     accumulated_score:int;
     step_num:int; parse_err_num:int; num_trials_done:int;
     current_exp_cps: exp_cps}


let step_from_string ans_string =
  let lexbuf = Lexing.from_string ans_string
  in
  try
    Ok ((*Picoml_cps_parse.*)interactive (*Picoml_cps_lex.*)token lexbuf)
  with (*Parsing.Parse_error*) _ ->
    ParseError ("Parse Error: "^ans_string^" \ndoes not parse.\n")

let problem_from_string prompt_string =
  let lexbuf = Lexing.from_string prompt_string
  in (*Picoml_cps_parse.*)exp_cps (*Picoml_cps_lex.*)token lexbuf

(*
let step_from_input {prompt; student_string; step_num; this_step_attempt_num} =
  (step_num, this_step_attempt_num, problem_from_string prompt, step_from_string student_string)
*)

let compare_exp_cps stu_cps_exp comp_cps_exp =
  if (List.exists (exp_cps_equiv stu_cps_exp) comp_cps_exp)
  then (true, (*Problem_points.*)score_per_part)
  else (false, 0)

let notify to_print history_string ans message next_prompt =
  let text = (message ^ "\n" ^ next_prompt ^"\n") in
  let _ = if to_print then print_string text else () in
      history_string ^ ("\n" ^ ans ^"\n" ^ text)

(* All needs changing below here *)
(* This should just be all_one_step_cps_trans.

let correct_result_of_step current_exp_cps =
  let possible_next_steps = all_one_step_cps_trans current_exp_cps in
  match possible_next_steps 
  with  v ->
    failwith ("You shouldn't see this; we are done at "^(string_of_exp current_exp))
    | Eval (exp, m) ->
      (match one_step_eval (exp,m)
       with None -> failwith "Can't take a step with the given input."
         | Some exp' -> exp')
    | _ -> failwith "Can't take a step with the given input."
*)
let is_done exp_cps = match all_one_step_cps_trans exp_cps with [] -> true | _ -> false

let takeStudentStep to_print
    ({history_string; rev_student_inputs; accumulated_score;
      step_num; parse_err_num; num_trials_done;
      current_exp_cps}(* as state*))
    student_input =
  let correct_steps = all_one_step_cps_trans current_exp_cps in
  let first_correct_step =
    (match correct_steps with [] ->
      raise
        (Failure "Grader Error: There should still be at least one step left at this point.\n")
      | (x :: __) -> x) in
  let correct_steps_string = 
    "["^(String.concat ",\n " (List.map string_of_exp_cps correct_steps))^"]\n" in
  let new_rev_student_inputs = student_input :: rev_student_inputs in
  match step_from_string student_input
  with ParseError msg ->
    if parse_err_num + 1 >= (*Problem_points.*)max_parse_errors_per_step
    then
      let are_done = is_done first_correct_step in 
      let error_response =
        msg^"\n\n"^
          "The possible correct next step in the translation is any of the following:\n" ^
          correct_steps_string^
          (if are_done
           then "That completes the problem.\n" (* Should we say goodbye here? *)
           else
              "\nWe will proceed from the first of the above possible correct next steps of translation.\n\n"^
                (string_of_exp_cps first_correct_step)^
                "\n\nLet's move on to the next part of the question...\nPlease put the next step of translation that comes after it.\n ") in
      ({history_string =
          (notify to_print history_string student_input
             error_response "");
        rev_student_inputs = new_rev_student_inputs;
        accumulated_score = accumulated_score;
        step_num = step_num+1; parse_err_num = 0; num_trials_done = 0;
        current_exp_cps = first_correct_step},
       are_done)
    else
      let error_response = msg^"\n\nYou have "^
        (string_of_int(max_parse_errors_per_step - (parse_err_num + 1)))^
        " tries left to get the parsing correct.\nPlease try again:\n" in
      ({history_string =
          (notify to_print history_string student_input
             error_response "");
        rev_student_inputs = new_rev_student_inputs;
        accumulated_score = accumulated_score;
        step_num = step_num;
        parse_err_num = parse_err_num + 1;
        num_trials_done = num_trials_done;
        current_exp_cps = current_exp_cps}, 
       false)
        
    | Ok student_next_response ->
      (match student_next_response
       with None ->
         ({history_string =
             (notify to_print history_string "\n\n"
                "You have asked to quit.  Your partial answer will be graded.\n\n"
                "");
             rev_student_inputs = new_rev_student_inputs;
             accumulated_score = accumulated_score;
             step_num = step_num;
             parse_err_num = 0;
             num_trials_done = 0;
             current_exp_cps = current_exp_cps}, 
          false)
         (* false because there is more to do, but should this be false because
            no more will be be done? *)
         | Some student_next_exp_cps ->
           let (is_correct, score_inc) = compare_exp_cps student_next_exp_cps correct_steps 
           in
           if is_correct
           then
             let are_done = is_done first_correct_step in 
             ({history_string =
                 (notify to_print history_string student_input
                    "\n\nThat is correct!\n"
                    (if are_done then "That completes the problem.\n"
                     else "Please enter the result of the next step.\n\n"));
               rev_student_inputs = new_rev_student_inputs;
               accumulated_score = accumulated_score+score_inc;
               step_num = step_num + 1;
               parse_err_num = 0;
               num_trials_done = 0;
               current_exp_cps = student_next_exp_cps}, 
              are_done)
           else if num_trials_done + 1 >= max_trials_per_step 
           then 
             let are_done = is_done first_correct_step in  
             let error_response =
               "That is not correct.  You have run out of tries.\nThe possible correct next step in translation is any of the following:\n" ^
                 correct_steps_string^
                 (if are_done
                  then "That completes the problem.\n" (* Should we say goodbye here? *)
                  else
                     "\nWe will proceed from the first of the above possible correct steps of translation.\n\n"^
                (string_of_exp_cps first_correct_step)^
                "\n\nLet's move on to the next part of the question...\nPlease put the next step of translation that comes after it.\n\n ") in
             ({history_string =
                 (notify to_print history_string student_input
                    error_response "");
               rev_student_inputs = new_rev_student_inputs;
               accumulated_score = accumulated_score;
               step_num = step_num+1; parse_err_num = 0; num_trials_done = 0;
               current_exp_cps = first_correct_step},
              are_done)
           else
             let error_response = "\n\nThat is not correct. \nYou have "^
               (string_of_int(max_trials_per_step - (num_trials_done + 1)))^
               " tries left to get this step right.\nPlease try again:\n" in
             ({history_string =
                 (notify to_print history_string student_input
                    error_response "");
               rev_student_inputs = new_rev_student_inputs;
               accumulated_score = accumulated_score;
               step_num = step_num;
               parse_err_num = 0;
               num_trials_done = num_trials_done + 1;
               current_exp_cps = current_exp_cps}, 
              false)
      )

let header =
  "\nWelcome to the Tester for Stepwise Translation from Picoml to Continuation Passsing Style (CPS). \n\n"^
    "In the following, you will be given an OCaml expression to be evaluated and an\n"^
    "environment in which the evaluation is to start.  You are asked to give each\n"^
    "step of transformation, as described by the rules given in class for transforming OCaml\n"^
    "expressions into Continuation Passing style.  Those rules are given with somewhat more\n"^
    "formal syntax here.  We use K as a mathematical variable representing a continuation.\n"^
    "The rules are as follows:\n\n"^
    
    (String.concat "\n" cps_trans_rules)^

    "\n\nYou are asked to give the formal statement of each single step of translation\n"^
    "to be taken to compute the result of the whole original expression with the\n"^
    "continuation provided.  When you have typed in a step, the system will check your\n"^
    "answer and tell you if it failed to parse, was wrong or was right.  If it failed\n"^
    "to parse or was wrong, you will be given a few extra changes.  You are limited in\n"^
    "the number of times per step you may give an answer that does not parse, and the\n"^
    "number of wrong answers you give, but the number of times the answer does not\n"^
    "parse does not subtract from the number of times you are allowed to try with the\n"^
    "wrong answer.  Your answer should start  with => to indicate a step, followed by\n"^
    "a CPS expression, possibly with parts still to be translated, left in the [[exp]]_k\n"^
    "notation. Your answer is allowed to span several lines. To indicate that you have\n"^
    "completed your answer, you should put a period (.) at the end as the last thing\n"^
    "typed besides RETURN.\n"^
    "CAUTION: A syntax error caused by putting a period in the middle of your answer\n"^
    "can be cause the program to assume you are quitting, instead of just that it\n"^
    "is a syntax error.  Please look for misplaced periods before pressing the RETURN.\n\n"^

    "You will be given "^(string_of_int (*Problem_points.*)max_trials_per_step)^
    " attempts per evaluation step before it moves you on to the\n"^
    "next step.  If you make "^(string_of_int (*Problem_points.*)max_parse_errors_per_step)^
    " syntactic errors in a row in a given step, you will\n"^
    "also be moved on to the next step.\n\n"^

    "You may rerun this program as many times as you like, and each time will start the\n"^
    "same problem over from start.  All the work done in the VSCode terminal is for\n"^
    "you to learn how to do the problem.  We recommend that as you do your work, keep all\n"^
    "you steps in a file so you can edit them more easily and you will still have them if\n"^
    "you decide to restart.  When you are done, you must press the Save & Grade button in\n"^
    "the main question webpage for your work to be recorded and a grade to be entered.\n\n"^

    "Following is an initial expression and continuation for you to translate step by\n"^
    "step followed by a prompt > after which you should put the next step of translation,\n"^
    "and so forth.\n\n"^
    problem_string ^"\n"

let initial_state = 
  {history_string = header; rev_student_inputs = []; accumulated_score = 0;
   step_num = 1; parse_err_num = 0; num_trials_done = 0;
  current_exp_cps = problem_from_string problem_string}

let rec rev_build_answer_exp_cps exp_cps rev_steps =
  match (all_one_step_cps_trans exp_cps)
  with [] -> exp_cps :: rev_steps
    | (next :: more) -> rev_build_answer_exp_cps next (exp_cps :: rev_steps)

let answer_from steps =
  match steps
  with [] -> ""
    | last_exp_cps :: rest_rev_exp_cps_list ->
      List.fold_left
        (fun s -> fun exp_cps -> (string_of_exp_cps exp_cps)^"\n=> "^s)
        (string_of_exp_cps last_exp_cps)
        rest_rev_exp_cps_list

let print_answer_string () =
  let str =
    answer_from (rev_build_answer_exp_cps (problem_from_string problem_string) [])
  in (print_string (str ^ "\n\n"); str)

let rev_answer = rev_build_answer_exp_cps (problem_from_string problem_string) []

let number_of_steps = List.length rev_answer - 1

let mk_finish_and_total (state, are_done) =
  let total_points_possible =
    Problem_points.score_per_part * (*Problem_points.*)number_of_steps in
  let total_points_string = string_of_int total_points_possible in
  let student_score_string = string_of_int state.accumulated_score in
  let grade_results =
    "Total: [" ^ student_score_string ^ " / " ^ total_points_string ^ "]\n\n" in
  let did_finish_string =
    if are_done
    then "\nYou have completed the assignment.  \n\n"
    else "\nYour assignment was submitted incomplete.  \n\n"
  in
    did_finish_string ^ grade_results

let generate_report_from_state (state, are_done) = 
  state.history_string ^ (mk_finish_and_total (state, are_done))
    
let grade_student_inputs student_inputs =
  let results =
    List.fold_left
      (fun (acc_state, done_by_now) -> (takeStudentStep false acc_state))
      (initial_state, (is_done initial_state.current_exp_cps))
      student_inputs
  in generate_report_from_state results

let mk_grader student_inputs = print_string (grade_student_inputs student_inputs)

let mk_student_inputs filename rev_student_inputs =
  let file_contents =
    "let student_inputs =\n"^"[\""^
      (String.concat "\";\n \"" (List.rev rev_student_inputs))^"\"]\n" in
  let outchan = open_out filename in
  let _ = output_string outchan file_contents in
  let _ = flush outchan in
  close_out outchan 

let interact () =
  let ((state, are_done) as final) =
    Gen_repl.read_eval_print_loop
      (takeStudentStep true)
      "> "
      header
      initial_state
  in
  let ending = mk_finish_and_total final in
  let _ = print_string ending in
  let new_history = state.history_string ^ ending in
  let new_state = {state with history_string = new_history} in
  let grade_report_chan = open_out "grade_report.txt" in
  let _ = output_string grade_report_chan new_history in
  let _ = flush grade_report_chan in
  let _ = close_out grade_report_chan in
  let _ = mk_student_inputs (Problem.problem_name^".ml") state.rev_student_inputs in
  (new_state, are_done)
