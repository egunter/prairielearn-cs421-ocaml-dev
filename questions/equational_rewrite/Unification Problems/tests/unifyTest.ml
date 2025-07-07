open Unifyparse
open Unifylex
open UnifyChecker
open Problem_points

let notify (message, ans_opt) =
  let answerMessage =
    match ans_opt with Some answer -> "Your answer was: " ^ answer ^ "\n"
      | None -> ""
  in print_string ("\n" ^ answerMessage ^ "\n" ^ message  ^ "\n" )

let rec next_question updatedQuestion trials =
(*  let max_trials_per_question = 3 in*)
  let new_trials = trials + 1 in
  let _ = print_string "\n\nEnter the resulting environment:\n> " in
  let _ = flush_all ()  in
  let answer = read_line () in 
  (match checkAnswer (updatedQuestion,  answer)
   with (is_parse_err, success, completed, canon_stu_answer, msg, score_inc) ->
     if success
     then (notify("That is correct!", Some canon_stu_answer);
           score_inc)
     else if new_trials >= max_trials_per_question
     then (notify(("You have reached the maximum number of tries.  Your answer last answer was:<br />" ^
                      canon_stu_answer (* checkResults[1] *) ^
                      "<br />which is incorrect.<br />The correct environment is:<br />" ^
                      canon_comp_answer (* checkResults[2] *) ^
                      "\nLet's move on to the next part of the question..."),
                  Some canon_stu_answer);
           score_inc)
     else
       (notify(("That is incorrect! (" ^
                   (string_of_int (max_trials_per_question - new_trials)) ^
                   " attemps remaining)"),
               Some canon_stu_answer);
        next_question updatedQuestion (trials+1)))
           
let test_code code = 
  let _ = 
    List.map print_endline
      ["\nWelcome to the Picoml Evaluation Environment Tester \n";
       "In the following, you will be given a series of declarations";
       "together with the output OCaml would print.  After each declaration,";
       "you are asked to type in the environment in effect after all the";
       "declarations up to that point have been executed.\n\n"]
  in
  let (_,total) =
   List.fold_left
     (fun (questionSoFar,score) -> fun c ->
       let _ = print_endline c in
       let updatedQuestion = questionSoFar ^ c in 
       let score_inc = next_question updatedQuestion 0 in
       (updatedQuestion, score + score_inc))
     ("",0)
     code
  in (print_string "\nYour final score on the problem is: ";
      print_int total;
      print_string "\n\n")
