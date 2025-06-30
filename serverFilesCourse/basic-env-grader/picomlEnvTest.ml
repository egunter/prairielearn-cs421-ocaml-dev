(*
  interactive-parser.ml - DO NOT EDIT
*)

open Picoml_eval
open Picomlparse
open Picomllex
open PicomlEnvChecker
open Student

(* Try to detect if something is getting piped in *)
let is_interactive = 0 = (Sys.command "[ -t 0 ]")

let code_test =
 [
        "2 = 1 + 1;;";
        "let a = 10;;";
        "let x = 11 + a;;";
        "let f y = x - y + a;;";
        "let b = (f x) \n  in \n (let a = 5 \n  in\n  a + b + x );;";
        "let x = 1;;";
        "let g u v = f (x + u - v);;";
        "let f = (g 3 (f 5));;"
    ]

type reader = ReadEnd | ReadSome of string * (unit -> reader)

let stu_reader () =
	let rec reader_from_list l = match l with
		| [] -> ReadEnd
		| h :: t -> ReadSome (h.ans, fun () -> reader_from_list t)
	in reader_from_list student_inputs.inputHistory

let notify (message, ans_opt) =
  let answerMessage =
    match ans_opt with Some answer -> "Your answer was: " ^ answer ^ "\n"
      | None -> ""
  in print_string ("\n" ^ answerMessage ^ "\n" ^ message  ^ "\n" )

let advance_read reader = match reader () with
	| ReadEnd -> raise (Failure "System error -- Student's recorded results were unsufficient to finish running the tester.")
	| ReadSome(s, reader') -> (s, reader')

let rec next_question updatedQuestion trials reader =
  let max_trials_per_question = 3 in
  let new_trials = trials + 1 in
  let _ = print_string "\n\nEnter the resulting environment:\n> " in
  let _ = flush_all ()  in (match reader () with
  | ReadEnd -> (0, fun () -> ReadEnd)
  | ReadSome(answer, reader') -> 
  (match checkAnswer (updatedQuestion,  answer)
   with (success, canon_stu_answer, canon_comp_answer, score_inc) ->
     if success
     then (notify("That is correct!", Some canon_stu_answer);
           (score_inc, reader'))
     else if new_trials >= max_trials_per_question
     then (notify(("You have reached the maximum number of tries.  Your answer last answer was:<br />" ^
                      canon_stu_answer (* checkResults[1] *) ^
                      "<br />which is incorrect.<br />The correct environment is:<br />" ^
                      canon_comp_answer (* checkResults[2] *) ^
                      "\nLet's move on to the next part of the question..."),
                  Some canon_stu_answer);
           (score_inc, reader'))
     else
       (notify(("That is incorrect! (" ^
                   (string_of_int (max_trials_per_question - new_trials)) ^
                   " attemps remaining)"),
               Some canon_stu_answer);
        next_question updatedQuestion (trials+1) reader'))
  )
         
let test_code code reader = 
  let _ = 
    List.map print_endline
      ["\nWelcome to the Picoml Evaluation Environment Tester \n";
       "In the following, you will be given a series of declarations";
       "together with the output OCaml would print.  After each declaration,";
       "you are asked to type in the environment in effect after all the";
       "declarations up to that point have been executed.\n\n"]
  in
  let (_,total,_) =
   List.fold_left
     (fun (questionSoFar, score, reader) -> fun c ->
       let _ = print_endline c in
       let updatedQuestion = questionSoFar ^ c in 
       let (score_inc, reader') = next_question updatedQuestion 0 reader in
       (updatedQuestion, score + score_inc, reader'))
     ("", 0, reader)
     code
  in (print_string "\nYour final score on the problem is: ";
      print_int total;
      print_string "\n\n";
      print_string ("\nTotal: [" ^ (string_of_int total) ^ " / 16]\n\nExtra: [0 / 0]\n\n")
)

let _ = test_code code_test stu_reader

