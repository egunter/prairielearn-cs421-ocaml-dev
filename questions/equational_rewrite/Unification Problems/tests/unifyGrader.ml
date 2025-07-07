(* File: unifyGrader.ml *)
(* Author: Elsa L. Gunter *)
(* Copyright: 2016 *)
(* Share and Enjoy *)

open UnifySteps
open Unifygenparse
open Unifylex
open Student
open Problem_points

let get_steps inputs =
  List.fold_left
    (fun (step_num, steps) input ->
      if input.step = step_num
      then (step_num, steps)
      else (input.step, input::steps))
    ((-1),[])
    (List.rev inputs.inputHistory)

let get_all_steps inputs =
  snd
  (List.fold_left
    (fun (step_num, steps) input ->
      match steps
      with [] -> (input.step, [[input]])
        | (first_step::more_steps) ->
          if input.step = step_num
          then (input.step, (input::first_step)::more_steps)
          else (input.step, ([input]::(List.rev first_step)::more_steps )))
    ((-1),[])
    (List.rev inputs))

let steps_from_string step_string =
  let lexbuf = Lexing.from_string step_string
  in ((full_input (fun lb -> Unifylex.token lb) lexbuf):((problem*subst_seq) option * input_step list))

let problem_from_string problem_string =
  let lexbuf = Lexing.from_string problem_string
  in ((question_prompt (fun lb -> Unifylex.token lb) lexbuf):(problem*subst_seq))

let steps_from_input {prompt; answer; step} =
  try Some (step, problem_from_string prompt,
            steps_from_string answer)
  with _ -> None

let checkInput input =
  match (steps_from_input input)
  with None ->
    (("Step "^string_of_int input.step^":\n"^input.prompt ^"\n"^ input.answer),
     ParseError ("Parse Error: "^input.answer^" has parse error.\n"))
    | Some (step,prob,inp) ->
      ("Step "^string_of_int step^":\n"^string_of_full_input inp,
       full_input_result (prob,inp))


let checkSteps stepListList =
  let first_pass =
  List.map (fun stepList -> checkInput (List.hd ((*List.rev*) stepList))) stepListList
  in match List.hd (List.rev first_pass)
    with (_,Success(_,_)) -> (first_pass, [])
      | (_,_) -> (first_pass,List.hd(List.rev stepListList))
        
let checkStudentInputs student_inputs =
  let stepListList = get_all_steps student_inputs in
  checkSteps stepListList

let is_done_score_of_result result =
  match result with Success(b,n) -> (b,n) | _ -> (false,0)

    (*
let build_first_report stepListList =
  match 
*)

let make_new_input step_num prior_input next_input =
  {step = step_num;
   prompt = prior_input.answer;
   answer = next_input.answer}

let can_parse_step step_string =
  try
    let _ =
      Unifygenparse.step
        (fun lb -> Unifylex.token lb) (Lexing.from_string step_string)
    in true
  with _ -> false

(*
let rec reinterpret_stepList step_num prior_input stepList
 (*results addtional_score*) =
  (*
    Assume list goes from first to last, so it has already been reversed
    a second time. 
  *)
  (* Question: Do we assume the prior_input parses.  If yes, then that
     is an invariant to establish and maintain, but it is also an
     invariant upon which we may rely.  To establish it will require a
     wrapper function.
     Answer: We will assume the invariant and write the wrapper.
  *)

  match stepList
  with [] -> ([],0)
    | next_input :: rem_tries ->
      let (_,result) as outcome =
        checkInput {next_input with step = step_num} in
      let (outcomes, addtional_score) =
      (match result
       with ParseError _  ->
        ( (reinterpret_stepList (step_num+1) prior_input rem_tries) :
            ((string * result) list * int))
         | Success(_,_) ->
           ( (reinterpret_stepList (step_num+1) next_input rem_tries) :
    ((string * result) list * int) )
         | _ -> (* Maybe DetailedFailure needs to be broken out *)
           let alt_input = make_new_input (step_num+1) prior_input next_input in
           let (_,alt_result) as alt_outcome = checkInput alt_input in
           let (alt_outcomes, alt_addtional_score) =
             ( (reinterpret_stepList (step_num+2) alt_input rem_tries) :
    ((string * result) list * int) )
           in (((alt_outcome :: alt_outcomes),
               (alt_addtional_score+score_of_result alt_result)) :
    ((string * result) list * int) )
      )
      in
      (((outcome :: outcomes), (addtional_score+score_of_result result)) :
    ((string * result) list * int))

let rec full_reinterpret_stepList step_num stepList =
  match stepList with [] -> ([],0)
    | input::more_inputs ->
      let (_,result) as outcome =
        checkInput {input with step = step_num} in
      let (outcomes, addtional_score) =
        (match result
         with ParseError _  ->
           full_reinterpret_stepList (step_num + 1) more_inputs
           | _ -> reinterpret_stepList (step_num + 1) input more_inputs)
      in ((outcome :: outcomes), (addtional_score+score_of_result result))
*)
(*
#use "tmp.ml";;
let test_inputs = get_all_steps amy
let test_list = List.hd (List.rev test_inputs)
let test_list1 =
  List.filter (fun input -> can_parse_step (input.answer)) test_list
let tmp = reinterpret_stepList 2 (List.rev test_inputs);;
*)

(* This is too complicated and getting bits wrong. ---ELG

let make_report student_inputs max_score =
  let stepListList =get_all_steps student_inputs in
  let (results, alt_end_results) = checkSteps stepListList in
  let (cum_score, progress_report, final_result) =
  List.fold_left
    (fun (total,str,res) (entry,result) ->
      (total + score_of_result result,
       str^"\n"^entry ^ "\n"^(string_of_result result), result))
    (0,"",Success(false,0))
    results
  in
  let (report,score) =
    match final_result
    with Success(true,_) ->
      (progress_report, max_score)
      | Success(false,_) ->
        (progress_report, cum_score)
      | _ ->
        let next_step_num = 1 + ((List.hd alt_end_results).step) in 
        let (alt_report, additional_score) =
          full_reinterpret_stepList next_step_num (List.rev alt_end_results) in
        (progress_report^
       "Your maximum attainable score is being reduced by two for you errors."^
         "\n\nTrying alternate interpretations of the last sequence of inputs:\n"
         ^List.fold_left
           (fun rep (str,res) ->
             rep^"\n"^"\n"^str ^ "\n"^(string_of_result res)) "" alt_report,
         min (cum_score + additional_score) (max_score - 2))
  in
  "Your total score is capped by "^(string_of_int max_score)^"\n\n"^
    report ^
      "\nTotal: ["^ string_of_int score ^ " / "^
      (string_of_int max_score)^"]\n\nExtra: [0 / 0]\n"
*)


let make_report student_inputs max_score =
  let (report, score, has_no_errors, is_done) =
    List.fold_left
      (fun (s,points,no_errs,is_done) -> fun input ->
        let (s',r) = checkInput input in
        let (finished,score) = is_done_score_of_result r 
        in (s^s'^"\n\n"^UnifySteps.string_of_result r^"\n",
            points + score,
            no_errs && not (score = 0),
            finished))
      ("",0,true,false)
      student_inputs
  in
  "Your total score is capped by "^(string_of_int max_score)^"\n"^
    report ^
    "\nTotal: ["^
    string_of_int (if is_done (* && has_no_errors *)
      then max_score else (min score (max_score-1))) ^ " / "^
    (string_of_int max_score)^"]\n\nExtra: [0 / 0]\n"


let _ = print_string (make_report Student.student_inputs max_problem_points)
