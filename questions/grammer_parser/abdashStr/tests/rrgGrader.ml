(* File: rrgGrader.ml *)
(* Author: Elsa L. Gunter *)
(* Copyright: 2016 *)
(* Share and Enjoy *)

open Rrg
open Rrgparse
open Rrglex
(* open Student // We wil read this in from $(MPNAME)_rrg *)
open Solution

(* In student.ml
type input = {answer: string; parsed: string; attempts: int}
type student_inputs = {inputHistory: input list}

In solution.ml
type points = {contained_in:int; contains:int; intersects: int; is_right_regular: int}
*)
let rrg_file = problem_name^"_rrg"

let ext_grammar_from_file file =
  let chan = open_in file in
  let lex = Lexing.from_channel chan in
  let grammar = Rrgparse.ext_main Rrglex.token lex in
  let _ = close_in chan in grammar

let grammar_from_string rrg_string =
    let lex = Lexing.from_string rrg_string
    in Rrgparse.main Rrglex.token lex

let sock n = if n < 0 then 0 else n

let report_not_contained_in errors points =
  match errors
  with [] -> (("There are no strings accepted that shouldn't be:  "^
                  (string_of_int points.contained_in) ^"\n\n"),points.contained_in)
    | _ ->
      let err_pts = sock (points.contained_in - (List.length errors)) in
      (("There are strings accepted that shouldn't be:     "^
           (string_of_int err_pts) ^"\n"^
           (string_of_list "  The following are examples: " ", " "\n\n"
              (fun path -> string_of_path (List.rev path)) errors)),
       err_pts)

let report_not_contains errors points =
  match errors
  with [] ->
    (("There are no strings not accepted that should be: "^
         (string_of_int points.contained_in) ^"\n\n"),points.contained_in)
    | _ ->
      let err_pts = sock (points.contains - (List.length errors)) in
      (("There are strings not accepted that should be:     "^
           (string_of_int err_pts) ^"\n"^
           (string_of_list "  The following are examples: " ", " "\n\n"
              (fun path -> string_of_path (List.rev path)) errors)),
       err_pts)

let report_not_intersects intersection points =
  match intersection with _::_ -> 
    ("Your input contains some of the right language:   "^
        (string_of_int points.intersects) ^"\n",
     points.intersects)
    | _ ->
      ("Your input contains none of the right language!    0\n",0)

let gradeAnswer (problem_string, student_file, points) =
  let max_total =
    (*string_of_int*)(points.is_right_regular + points.intersects +
                    points.contained_in + points.contains)
  in
  let problem_grammar = grammar_from_string problem_string
  in
 (* "Your last input for the problem was:\n"^student_string^"\n\n"^ *)
    (match 
        (try Some (ext_grammar_from_file student_file)
         with _ -> None)
     with Some (student_ext_rules, student_start_symbol) ->
       let is_right_reg =
         List.for_all
           (fun (nt,ext_prod) -> is_ext_production_production ext_prod)
           student_ext_rules
       in
       let (rrg_msg, rrg_pts) =
         if is_right_reg
         then ("Your input is a right regular grammar:            "^
                  (string_of_int points.is_right_regular) ^"\n",
               points.is_right_regular)
         else
           let pts = (max(points.is_right_regular - 2) 0)
           in 
           ("Your input is an extended right regular grammar,\n"^
               "  but not a right regular grammar, as required:   "^
               (string_of_int pts)^"\n", pts)
       in
       let student_rr_rules =
         if is_right_reg
         then
           List.map
             (fun (nt, ext_prod) -> (nt, ext_production_to_production ext_prod))
             student_ext_rules
         else ext_rules_to_rr_rules student_ext_rules
       in
       let student_grammar = (student_rr_rules, student_start_symbol) in
       let joint_alphabet =
         set_union terminal_compare (get_alphabet student_rr_rules)
           (get_alphabet (fst problem_grammar))
       in
       (match
           build_and_check_product
             false
             (dfa_of_rrg joint_alphabet problem_grammar)
             (dfa_of_rrg joint_alphabet student_grammar)
        with (_, contained_in_errs, contains_errs, intersects) ->
          let (int_msg, int_pts) = report_not_intersects intersects points in
          let (ctn_in_msg, ctn_in_pts) =
            report_not_contained_in contained_in_errs points in
          let (ctns_msg, ctns_pts) =
            report_not_contains contains_errs
              {points with contains =
                  if int_pts = 0 then 0 else points.contains}  in
          let score = (*string_of_int*) (rrg_pts + int_pts + ctn_in_pts + ctns_pts)
          in
          ((rrg_msg ^ int_msg ^ ctn_in_msg ^ ctns_msg), score, max_total)
             (*"Total: ["^score^" / "^max_total^"]\n\nExtra: [0 / 0]\n"*)
       )
       | None ->
         (("Your input is only context free,\n"^
             "  not a right regular grammar, as required:       0\n\n" ^
             "Hand grading from here:\n"^
             "Does you input contain any of the right language: \n" ^
             "Are any strings accepted that shouldn't be (2pts):\n\n"^
             "Are all strings accepted that should be (2pts):   \n\n"),
             (*"Total: [ 0 / " ^ max_total ^ "]\n\nExtra: [0 / 0]\n"*)
             0, max_total))

let rrg_results =
  (*print_string*) (gradeAnswer (rrg_solution, rrg_file, points))
