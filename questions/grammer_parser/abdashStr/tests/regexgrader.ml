
open Regexast
open Regexparse
open Regexlex
open Regexint
(*open Student*)
open Solution

let regexp_file = problem_name^"_regexp"
  
type grade =
	| NoParse
	| BadAlpha
	| TooLib of string
	| TooCons of string
	| Correct

let parse_exp s =
  let lex = Lexing.from_string s
  in Regexparse.exp Regexlex.token lex

let parse_file file =
  let chan = open_in file in
  let lex = Lexing.from_channel chan in
  let regexp = Regexparse.exp Regexlex.token lex in
  let _ = close_in chan in regexp             

let rec get_alpha_set r s = match r with
	| Sym c -> add_set s c
	| Concat [] -> s
	| Concat (r' :: t) -> get_alpha_set (Concat t) (get_alpha_set r' s)
	| Union rs -> fold_set (fun s' r' -> get_alpha_set r' s') s rs
	| Star r' -> get_alpha_set r' s

let stu_grade file =
	try let stu_regex = parse_file file in
		let sol_regex = parse_exp regexp_solution in
		if eq_set (get_alpha_set stu_regex (empty_set (=))) (get_alpha_set sol_regex (empty_set (=))) 
		then match equiv stu_regex sol_regex with
			| Equiv _ -> Correct
			| NE1 s -> TooLib s
			| NE2 s -> TooCons s
		else BadAlpha
	with _ -> NoParse

(*
"Your input is not a regular expression:                     0"
"Your input is a regular expression:                         1"

"Input alphabet cannot be analyzed:                          0"
"Your input does not use the correct alphabet:               0"
"Your input utilizes the correct alphabet:                   1"

"The regular expression cannot be analyzed:                  0"
"The regular expression accepts a string it should not:      0"
"The regular expression rejects a string it should not:      0"
"The regular expression accepts the correct set of strings:  3"


"Total: ["  " / 5]"

"Extra: [0 / 0]"
*)

let reg_check rc = match rc with
	| 0 -> "Your input is not a regular expression:                     0\n"
	| 1 -> "Your input is a regular expression:                         1\n"
	| _ -> "BUG WITH GRADING SCRIPT. PLEASE REPORT."

let alph_check ac = match ac with
	| 0 -> "Input alphabet cannot be analyzed:                          0\n"
	| 1 -> "Your input does not use the correct alphabet:               0\n"
	| 2 -> "Your input utilizes the correct alphabet:                   1\n"
	| _ -> "BUG WITH GRADING SCRIPT. PLEASE REPORT."

let cor_check cc s = match cc with
	| 0 -> "The regular expression cannot be analyzed:                  0\n"
	| 1 -> "The regular expression accepts a string it should not:      0\n" ^ " Example includes: " ^ s ^ "\n"
	| 2 -> "The regular expression rejects a string it should not:      0\n" ^ " Example includes: " ^ s ^ "\n"
	| 3 -> "The regular expression accepts the correct set of strings:  3\n"
	| _ -> "BUG WITH GRADING SCRIPT. PLEASE REPORT."

let print_select (is_reg, good_alph, correct, bad_sample, score) =
	((reg_check is_reg)
	^ (alph_check good_alph)
	^ (cor_check correct bad_sample), score, 5)

let print_grade grade = let select = match grade with
	| NoParse -> (0, 0, 0, "", 0)
	| BadAlpha -> (1, 1, 0, "", 1)
	| TooLib s -> (1, 2, 1, s, 2)
	| TooCons s -> (1, 2, 2, s, 2)
	| Correct -> (1, 2, 3, "", 5)
	in print_select select;;

let regexp_results = print_grade (stu_grade regexp_file);;
