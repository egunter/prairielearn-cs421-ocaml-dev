
open Genset
open Genmap
open Ptgload
open Ptgcheck
open Solution
open Ptggettree

(*let max_score = 10*)

let mistake_penalty = 1
let fringe_penalty = 2
	(* does the parse tree start with the correct non-terminal *)
let head_nt_penalty = 1


(*
  Total points gathered from correct non-terminals:   7

  Invalid non-terminal productions used:    				  -2 (-1 per unique mistake)
    * exp ::= prop + prop
    * prop ::= d

  Target string was `d+d+d`
  The tree represents an incorrect string:            -2
  	String represented was `d+d`

  The starting non-terminal should be `exp`
	The tree uses the incorrect starting non-terminal:  -1
    Non-terminal used was `prop`

	Total: [2 / 10]
  Extra: [0 / 0]
*)


let rec string_sep_list l = match l with
	| [] -> ""
	| [s] -> s
	| s :: t -> s ^ " " ^ (string_sep_list t)

let rec print_mistake_set ms = match take_set ms with
	| None -> ()
	| Some ((head, child_list), s') ->
		print_string ("    * " ^ head ^ " ::= " ^ (string_sep_list child_list) ^ " \n");
		print_mistake_set s';;

let print_result (correct_nt,
	(ms_total, mistake_set),
	(fringe_sol, fringe_stu, fc),
	(head_sol, head_stu, hc)) =

  print_string ("  The starting non-terminal should be `" ^ head_sol ^ "`\n");
  (if hc
   then
      (print_string
         "     The tree uses the correct starting non-terminal.\n")
   else (print_string ("  The tree uses the incorrect starting non-terminal:  -"
                       ^ (string_of_int head_nt_penalty) ^ "\n");
	 print_string ("    Non-terminal used was `" ^ head_stu ^ "`\n\n"))
  );

  print_string ("\n  Target string was `" ^ fringe_sol ^ "`\n");
  (if fc
   then (print_string "     The tree represents the correct string.\n\n")
   else (print_string ("  The tree represents an incorrect string:            -"
                       ^ (string_of_int fringe_penalty) ^ "\n");
	 print_string ("    String represented was `" ^ fringe_stu ^ "`\n\n"))
  );
  
  print_string ("  Total points gathered from correct non-terminals:   "
                ^ (string_of_int correct_nt) ^ "\n\n");

  (if ms_total = 0 then ()
   else print_string ("  Invalid non-terminal productions used:    				  -" ^
                         (string_of_int ms_total) ^
		         " (-" ^ (string_of_int mistake_penalty) ^ " per unique mistake)\n"));
  print_mistake_set mistake_set

let print_final_score score =
	print_string ("Total: [" ^ (string_of_int score) ^ " / " ^ 
                         (string_of_int max_score)^"]\n");
	print_string ("Extra: [0 / 0]\n\n");;

(*
(* This is not the algorithm I requested.  ---ELG *)
let score (Problem(gram, fringe, head)) tree = let Tree(l, _) = tree
	in let (correct_nt, mistake_set) = analyze_tree gram tree (empty_set mistake_equality)
	in let fringe_read = get_fringe tree
	in let fringe_correct = (fringe_read = fringe)
	in let head_correct = (head = l)
	in let ms_penalty = (size_set mistake_set) * mistake_penalty
	in let penalty = ms_penalty
		+ (if fringe_correct then 0 else fringe_penalty)
		+ (if head_correct then 0 else head_nt_penalty)
	in let local_max = if correct_nt > max_score then max_score else correct_nt
	in let final_score = if penalty > local_max then 0 else local_max - penalty
	in print_result (correct_nt,
		(ms_penalty, mistake_set),
		(fringe, fringe_read, fringe_correct),
		(head, l, head_correct)
	); final_score;;
*)
let score (Problem(gram, fringe, head, sol_no_parse)) tree = let Tree(l, _) = tree
	in let (correct_nt, mistake_set) = analyze_tree gram tree (empty_set mistake_equality)
	in let fringe_read = get_fringe tree
	in let fringe_correct = (fringe_read = fringe)
	in let head_correct = (head = l)
	in let ms_penalty = (size_set mistake_set) * mistake_penalty
	in let penalty = ms_penalty
		+ (if fringe_correct then 0 else fringe_penalty)
	        + (if head_correct then 0 else head_nt_penalty)
        in let local_max = max_score - penalty
	in let final_score = (let x = min local_max correct_nt in max x 0)
	in print_result (correct_nt,
		(ms_penalty, mistake_set),
		(fringe, fringe_read, fringe_correct),
		(head, l, head_correct)
	); final_score;;

let score_forest (stu_no_parse,forest) =
  match lookup_map (grammar_bank) (current) with
    | None -> print_string "  This submission indicates a major bug in the grader. Please report this issue.\n\n"; print_final_score 0
    | Some p ->
      (match p with (Problem(gram, fringe, head, sol_no_parse)) ->
        if stu_no_parse && sol_no_parse
        then
        (print_string
           "Student selected \"There is no parse tree for this string:\", which is correct.\n\n\n";
         print_final_score max_score)
        else
          match forest
          with [] ->
            (print_string "Student gave no tree, but a parse is possible.\n\n\n";
             print_final_score 0)
            | (label, tree) :: more ->
              ((if more = [] then ()
                else print_string
                  "More than one tree given, grading topmost one.\n\n\n";);
               print_final_score (score p tree)))

(*
let rec score_answer ans = match ans with
	| NoTree -> print_string "  You did not give a parse tree, which is incorrect.\n\n"; print_final_score 0
	| ErrorTree _ -> print_string "  This submission needs hand-grading.\n\n"; print_final_score 0
	| BadForest _ -> print_string "  This submission indicates a bug in our grader. Please report this issue.\n\n"; print_final_score 0
	| SingleTree tree -> score_tree tree
	| GoodForest [] -> print_string "  This submission indicates a bug in our grader. Please report this issue.\n\n"; print_final_score 0
	| GoodForest (tr :: _) -> print_string "  This submission has multiple trees needs hand-grading to be properly interpreted. A rudimentary pass was done with the first tree from the list.\n\n"; score_tree tr
	| ExtraInfo [] -> print_string "  This submission indicates a bug in our grader. Please report this issue.\n\n"; print_final_score 0
	| ExtraInfo (tr :: _) -> score_answer tr
	| _ -> print_string "  This submission indicates a bug in our grader. Please report this issue.\n\n"; print_final_score 0;;
*)

let _ =
  let (stu_no_parse, student_tree_description) = 
    tree_description_from_file Solution.problem_file_name
  in
  let forest = make_trees student_tree_description
  in score_forest (stu_no_parse, forest)



(* I do not know from where we get Student.result ---ELG
let _ = match Student.result with`
	| MalformAns -> print_string "  Could not read the JSON produced from this submission.\n\n"; print_final_score 0
	| NoTreeAns 1 -> print_string "  You said that there was no parse tree, which is incorrect.\n\n"; print_final_score 0
	| NoTreeAns _ -> print_string "  You did not give a parse tree, which is incorrect.\n\n"; print_final_score 0
	| SomeTreeAns ans -> score_answer ans;;

*)
