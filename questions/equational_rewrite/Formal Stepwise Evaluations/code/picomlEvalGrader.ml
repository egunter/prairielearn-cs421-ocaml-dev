(*
File: picomlEvalGrader.ml
Author: Elsa L. Gunter 
Share and Enjoy
*)


open Utils
open Gen_repl
open Gen_env
open Picoml_exp_and_val
open Picomlparse
open Picomllex
open Picoml_types
open Picoml_type_system
open Picoml_eval
open Picoml_eval_rules
open Problem_points
open Problem


type 'a parse = Ok of 'a | ParseError of string

type state =
    {history_string:string; rev_student_inputs:string list;
     accumulated_score:int;
     step_num:int; parse_err_num:int; num_trials_done:int;
     current_exp: exp}

let step_from_string ans_string =
  let lexbuf = Lexing.from_string ans_string
  in
  try
    Ok ((*Picomlparse.*)interactive (*Picomllex.*)token lexbuf)
  with (*Parsing.Parse_error*) _ ->
    ParseError ("Parse Error: "^ans_string^" \ndoes not parse.\n")

let problem_from_string prompt_string =
  let lexbuf = Lexing.from_string prompt_string
  in (*Picomlparse.*)expression (*Picomllex.*)token lexbuf

(*
let step_from_input {prompt; student_string; step_num; this_step_attempt_num} =
  (step_num, this_step_attempt_num, problem_from_string prompt, step_from_string student_string)
*)

let compare_exps stu_exp comp_exp =
  let stu_cev = canonicalize_exp stu_exp in
  let comp_cev = canonicalize_exp comp_exp in
  if (stu_cev = comp_cev)
  then (true, (*Problem_points.*)score_per_part)
  else (false, 0)

let notify to_print history_string ans message next_prompt =
  let text = (message ^ "\n" ^ next_prompt ^"\n") in
  let _ = if to_print then print_string text else () in
      history_string ^ ("\n" ^ ans ^"\n" ^ text)

(* All needs changing below here *)
let correct_result_of_step current_exp =
  match current_exp
  with Val v ->
    failwith ("You shoudln't see this; we are done at "^(string_of_exp current_exp))
    | Eval (exp, m) ->
      (match one_step_eval (exp,m)
       with None -> failwith "Can't take a step with the given input."
         | Some exp' -> exp')
    | _ -> failwith "Can't take a step with the given input."

let is_done exp = match exp with Val _ -> true | _ -> false

let takeStudentStep to_print
    ({history_string; rev_student_inputs; accumulated_score;
      step_num; parse_err_num; num_trials_done;
      current_exp}(* as state*))
    student_input =
  let correct_step = correct_result_of_step current_exp in
  let new_rev_student_inputs = student_input :: rev_student_inputs in
  match step_from_string student_input
  with ParseError msg ->
    if parse_err_num + 1 >= (*Problem_points.*)max_parse_errors_per_step
    then
      let are_done = is_done correct_step in 
      let error_response =
        "\n"^msg^"\n\n"^
          "The correct next step in evaluation is:\n" ^
          (string_of_exp correct_step)^
          (if are_done
           then "\nThat completes the problem.\n" (* Should we say goodbye here? *)
           else
              "\nWe will proceed from the above correct step of evaluation.\nLet's move on to the next part of the question...\nPlease put the next step of evaluation that comes after it.\n ") in
      ({history_string =
          (notify to_print history_string student_input
             error_response "");
        rev_student_inputs = new_rev_student_inputs;
        accumulated_score = accumulated_score;
        step_num = step_num+1; parse_err_num = 0; num_trials_done = 0;
        current_exp = correct_step},
       are_done)
    else
      let error_response = "\n"^msg^"\n\nYou have "^
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
        current_exp = current_exp}, 
       false)
        
    | Ok student_next_response ->
      (match student_next_response
       with None ->
         ({history_string =
             (notify to_print history_string "\n"
                "\nYou have asked to quit.  Your partial answer will be graded.\n\n"
                "");
             rev_student_inputs = new_rev_student_inputs;
             accumulated_score = accumulated_score;
             step_num = step_num;
             parse_err_num = 0;
             num_trials_done = 0;
             current_exp = current_exp}, 
          false)
         (* false because there is more to do, but should this be false because
            no more will be be done? *)
         | Some student_next_exp ->
           let (is_correct, score_inc) =
             compare_exps student_next_exp correct_step 
           in
           if is_correct (* Should do a diff, .... but no time .... *)
           then
             let are_done = is_done correct_step in 
             ({history_string =
                 (notify to_print history_string student_input
                    "\nThat is correct!\n"
                    (if are_done then "That completes the problem.\n"
                     else "Please enter the result of the next step.\n\n"));
               rev_student_inputs = new_rev_student_inputs;
               accumulated_score = accumulated_score+score_inc;
               step_num = step_num + 1;
               parse_err_num = 0;
               num_trials_done = 0;
               current_exp = student_next_exp}, 
              are_done)
           else if num_trials_done + 1 >= max_trials_per_step 
           then 
             let are_done = is_done correct_step in  
             let error_response =
               "\nThat is not correct.  You have run out of tries.\nThe correct next step in evaluation is:\n" ^
                 (string_of_exp correct_step)^
                 (if are_done
                  then "\nThat completes the problem.\n" (* Should we say goodbye here? *)
                  else
                     "\nWe will proceed from the above correct step of evaluation.\nLet's move on to the next part of the question...\nPlease put the next step of evaluation that comes after it.\n\n ") in
             ({history_string =
                 (notify to_print history_string student_input
                    error_response "");
               rev_student_inputs = new_rev_student_inputs;
               accumulated_score = accumulated_score;
               step_num = step_num+1; parse_err_num = 0; num_trials_done = 0;
               current_exp = correct_step},
              are_done)
           else
             let error_response = "\nThat is not correct. \nYou have "^
               (string_of_int(max_trials_per_step - (num_trials_done + 1)))^
               " tries left to get this step right.\nPlease try again:\n" in
             ({history_string =
                 (notify to_print history_string ("\n"^student_input^"\n")
                    error_response "");
               rev_student_inputs = new_rev_student_inputs;
               accumulated_score = accumulated_score;
               step_num = step_num;
               parse_err_num = 0;
               num_trials_done = num_trials_done + 1;
               current_exp = current_exp}, 
              false)
      )

let header =
  "\nWelcome to the Picoml Formal Evaluation Tester \n\n"^
    "In the following, you will be given an OCaml expression to be evaluated and an\n"^
    "environment in which the evaluation is to start.  You are asked to give each\n"^
    "step of evaluation, as described by the rules given in class for one-step\n"^
    "evaluation of OCaml expressions.  The rules are as follows:\n\n"^
    
    (String.concat "\n"
       (List.map (fun (label, eval_rule) -> (label^":\t"^eval_rule)) eval_rules))^

    "\n\nYou are asked to give the formal statement of each single step of evaluation\n"^
    "to be taken to compute the value of the whole original expression in the\n"^
    "environment provided.  When you have typed in a step, the system will check your\n"^
    "answer and tell you if it failed to parse, was wrong or was right.  If it failed\n"^
    "to parse or was wrong, you will be give a few extra changes.  You are limited in\n"^
    "the number of times per step you may give an answer that does not parse, and the\n"^
    "number of wrong answers you give, but the number of times the answer does not\n"^
    "parse does not subtract from the number of times you are allowed to try with the\n"^
    "wrong answer.  Your answer should start  with => to indicate a step, followed by\n"^
    "an Eval expression, which is allowed to span several lines.   You will need to\n"^
    "describe environments for the instances of Eval and for closures.  You should use\n"^
    "the same notation for environments and closures that was used in the WA for\n"^
    "environment calculations.  To indicate that you have completed your answer, you\n"^
    "should put a period (.) at the end as the last thing typed besides Return.\n"^
    "CAUTION: A syntax error caused by putting a period in the middle of your answer\n"^
    "can be cause the program to assume you are quitting, instead of just that it\n"^
    "is a syntax error.  Please look for misplaced periods before hitting the RETURN.\n\n"^

    "You will be given "^(string_of_int (*Problem_points.*)max_trials_per_step)^
    " attempts per evaluation step before it moves you on to the\n"^
    "next step.  If you make "^(string_of_int (*Problem_points.*)max_parse_errors_per_step)^
    " syntactic errors in a row in a given step, you will\n"^
    "also be moved on to the next step.\n\n"^

    "You may rerun this program as many times as you like, and each time will start the\n"^
    "same problem over from start.  All the work done in the VSCode terminal is for\n"^
    "you to learn how to do the problem.  When you are done, you must press the\n"^
    "Save & Grade button in the main question webpage for your work to be recorded\n"^
    "and a grade to be entered.\n\n"^

    "Following is an initial Eval expression for you to evaluate step by step followed\n"^
    "by a prompt > after which you should put the next step of evaluation, and so forth.\n\n"^
    problem_string ^"\n"

let initial_state = 
  {history_string = header; rev_student_inputs = []; accumulated_score = 0;
   step_num = 1; parse_err_num = 0; num_trials_done = 0;
  current_exp = problem_from_string problem_string}

let rec rev_build_answer_exps exp rev_steps =
  match exp with Val _ -> exp :: rev_steps
    | _ -> rev_build_answer_exps (correct_result_of_step exp) (exp :: rev_steps)

let answer_from steps =
  match steps
  with [] -> ""
    | last_exp :: rest_rev_exp_list ->
      List.fold_left
        (fun s -> fun exp -> (string_of_exp exp)^"\n=> "^s)
        (string_of_exp last_exp)
        rest_rev_exp_list

let print_answer_string () =
  let str =
    answer_from (rev_build_answer_exps (problem_from_string problem_string) [])
  in (print_string (str ^ "\n\n"); str)

let rev_answer = rev_build_answer_exps (problem_from_string problem_string) []

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
      (initial_state, (is_done initial_state.current_exp))
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
