(* File: lambdaGrader.ml *)
(* Author: Elsa L. Gunter *)
(* Copyright: 2016 *)
(* Share and Enjoy *)

open Lambda
open Lambda_parse
open Lambda_lex
open Solution
open Student

let problem_from_string s =
  Lambda_parse.question_prompt Lambda_lex.token (Lexing.from_string s);; 

let steps_from_string step_string =
  let lexbuf = Lexing.from_string step_string
  in Lambda_parse.input (fun lb -> Lambda_lex.token lb) lexbuf

let steps_from_input {prompt; answer; step} =
  try Some (step, problem_from_string prompt,
            steps_from_string answer)
  with _ -> None

let string_of_step lambda problem tag exp = 
  string_of_lambda lambda problem ^ "\n" ^ string_of_tag tag ^ " " ^
    string_of_lambda lambda exp ^ "\n"

let rec checkFullInput lambda evalf alpha_value beta_value
    cum_score cum_mistakes cum_msg was_done problem steps =
   (* This should give back a pair of the score from the steps, the
      number of mistakes, and a string of the result up to this point. *)
   match steps
   with [] -> (cum_score, cum_mistakes, cum_msg, was_done)
     | (tag,exp) :: more_steps ->
       let (next_score,next_mistakes,next_msg,done_at_this_step) = 
         match checkInput lambda evalf alpha_value beta_value (problem, tag, exp)
         with Success (is_done, score) ->
           let inc_score = if was_done then 0 else score
           in
           (inc_score, 0,
            (string_of_step lambda problem tag exp^
               "This is correct.\nIncremental score: "^string_of_int inc_score^"\n"^
               (if is_done && not was_done
                then "You have reached the last step.\n" else "")^"\n\n"),
            is_done)
           | Failed msg ->
             (0, 1,
              (string_of_step lambda problem tag exp ^"\nFailed: "^msg^"\n"), false)
           | ParseError msg -> raise (Failure "This can't happen here.")
       in
       checkFullInput lambda evalf alpha_value beta_value
         (cum_score+next_score) (cum_mistakes+next_mistakes) (cum_msg ^ next_msg)
         (was_done || done_at_this_step) exp more_steps

let checkAnswer lambda evalf alpha_value beta_value
   (* cum_score cum_mistakes cum_msg was_done *) ({step; prompt; answer} as input) =
  let header = ("Step "^string_of_int step^":\n" (*^prompt^"\n"^ answer*))
  in
  match ((steps_from_input input):(int * Lambda.lam *
   ((Lambda.tag option * Lambda.lam) * (Lambda.tag * Lambda.lam) list))
  option)
  with None -> (0, 0,
                (header^"\n"^"Parse Error: "^answer^" has parse error.\n"),
                false)
    | Some (step,problem,inpt) ->
      (match inpt
       with ((None, exp), steps) ->
         if exp = problem
         then checkFullInput lambda evalf alpha_value beta_value
           0 0 header false problem steps
         else
           (0, 1,
            (header^"\n"^
               "Your first step is missing =a= and -B->, but it must have one of these.\n\n"),
            false)
         | ((Some tag, exp), steps) ->
           if exp = problem
           then
             checkFullInput lambda evalf alpha_value beta_value 0 0 
               header false problem steps
           else
             checkFullInput lambda evalf alpha_value beta_value 0 0 
               header false problem ((tag,exp)::steps))

   
(* Do I trust the prompt at each step? I am going to because I will
   assume either the student did the problem one step at a time, or
   all at once as a block.  This may need to be revisited. *)

let rec checkInputs lambda evalf alpha_value beta_value max_tries
    cum_score cum_mistakes cum_msg was_done tries inputs =
  match inputs
  with [] -> (cum_score, cum_mistakes, cum_msg, was_done)
    | input::more_inputs ->
      let is_last_in_step_num =
        (more_inputs = [] || (not ((List.hd more_inputs).step = input.step)))
      in
      let (next_score,next_mistakes,next_msg,next_was_done) =
        checkAnswer lambda evalf alpha_value beta_value input
      in
      let (next_cum_score, next_cum_mistakes, next_tries) =
        match next_mistakes
        with 0 -> (* Could be right or could be a parse error *)
          (match next_score
           with 0 -> (cum_score, cum_mistakes, tries)
             | _ -> 
               if is_last_in_step_num
               then (cum_score + next_score, cum_mistakes, tries)
               else (*raise (Failure "No mistakes and we got points but we didn't move on?!!")*)
                 (cum_score, cum_mistakes, tries)
)
          | _ -> (* We made mistakes in one step.  Are we out of tries? *)
            (match tries
             with 0 -> (* We are out of tries. *)
               (cum_score + next_score, cum_mistakes + next_mistakes, 0)
               | _ -> (* We have tries left.  If they moved on, then keep points;
                         they must have given up. *)
                 if is_last_in_step_num
                 then (cum_score + next_score, cum_mistakes, tries - 1)
                 else (cum_score, cum_mistakes, tries - 1)
            (* They tried again. Record nothing, dec tries *)
            )
      in
      checkInputs lambda evalf alpha_value beta_value max_tries
        next_cum_score next_cum_mistakes (cum_msg^"\n"^next_msg)
        (was_done || next_was_done) (if is_last_in_step_num then max_tries else next_tries)
        more_inputs

let make_report lambda evalf alpha_value beta_value max_tries max_score student_inputs =
  let (cum_score, cum_mistakes, cum_msg, was_done) =
    checkInputs lambda evalf alpha_value beta_value max_tries
      0 0 "" false max_tries student_inputs
  in
  let score =
    if was_done then (max_score - cum_mistakes)
    else min cum_score (max_score - cum_mistakes)
  in
  "Your total score, before errors, is capped by "^(string_of_int max_score)^"\n"^
    cum_msg ^
      "\nTotal: ["^ string_of_int score ^ " / "^
      (string_of_int max_score)^"]\n\nExtra: [0 / 0]\n"

let _ = print_string (make_report "%" Solution.evalStrategy Solution.alphaValue Solution.betaValue Solution.maxAttempts Solution.maxScore Student.student_inputs)
