
(* An invariant we expect any time a list is representing a set is
that the list is sorted with unique entries *)

(* Generic list as as set functions *)

let rec insert_uniq compare x lst =
  match lst with [] -> [x]
    | y::ys ->
      let n = compare x y 
      in
      if n < 0 then x::lst
      else if n = 0 then x::ys (*prefer the inserted one*)
      else y::(insert_uniq compare x ys)

let rec sort_uniq compare lst =
  match lst with [] -> []
    | (x::xs) -> insert_uniq compare x (sort_uniq compare xs)

let set_minus lst1 lst2 = List.filter (fun x -> not(List.mem x lst2)) lst1

let set_diff lst1 lst2 = (set_minus lst1 lst2, set_minus lst2 lst1)

let string_of_list lb sep rb string_of_elt lst =
  let rec mid_str_of_list l =
      match l with [] -> ""
        | [x] -> string_of_elt x
        | x :: xs -> string_of_elt x ^ sep ^ mid_str_of_list xs
  in lb^(mid_str_of_list lst)^rb

let assoc_compare compare (key1,val1) (key2,val2) = compare key1 key2

let update_assoc compare = insert_uniq (assoc_compare compare) 

type var = string

type constructor = string

type term = Var of var | Const of constructor * (term list)

let rec string_of_term tm =
  match tm with Var v -> v
    | Const (c, []) -> c
    | Const (c, tms) -> 
      c^string_of_list "(" ", " ")" string_of_term tms

let rec occurs v tm = 
  match tm with Var x -> v = x
    | Const(_, args) ->
      List.fold_left (fun b arg_tm -> b || occurs v arg_tm) false args

type equation = term * term

let string_of_equation ((tm1, tm2):equation) =
  "("^(string_of_term tm1)^" = "^(string_of_term tm2)^")"

type problem = equation list

let string_of_problem : problem -> string =
  string_of_list "{" ", " "}" string_of_equation

type simple_substitution = (var * term)

let string_of_simple_substitution (v,tm) = v^" -> "^string_of_term tm

type substitution = simple_substitution list

let string_of_substitution : substitution -> string =
  string_of_list "{" ", " "}" string_of_simple_substitution

let subst_fun (s:substitution) n = (try List.assoc n s with _ -> Var n)

let rec term_subst (s:substitution) (t:term) =
    match t with Const(c, args) -> Const(c, List.map (term_subst s) args)
      | Var v -> subst_fun s v

let subst_compose (s2:substitution) (s1:substitution) : substitution =
  List.fold_right
    (update_assoc compare)
    (List.map (fun (v,residue) -> (v, term_subst s2 residue)) s1)
    s2

(* Example:
subst_compose [("a",Const("K",[Var "x"]));("x",Const("R",[Var "a"]))]
 [("a" , Const("A",[Var "a"; Var "x"])); ("d",Const("D", [Var "x"]))];;
*)

type subst_seq =
    Subst of substitution
  | Compose of substitution * subst_seq

let rec string_of_subst_seq subst_seq =
  match subst_seq
  with Subst subst -> string_of_substitution subst
    | Compose (subst, subst_seq) ->
      (string_of_substitution subst) ^ " o " ^ (string_of_subst_seq subst_seq)

let rec reduce_subst_seq subst_seq =
  match subst_seq with Subst subst -> (subst : substitution)
    | Compose (subst, subst_seq) ->
      subst_compose subst (reduce_subst_seq subst_seq)
        
type student_problem =
    Problem of problem * subst_seq
(*  | Final of subst_seq *)
  | Fail

let string_of_problem_and_subst (problem, subst_seq) =
  "Unify "^(string_of_problem problem)^" o "^string_of_subst_seq subst_seq

let string_of_student_problem student_problem =
  match student_problem
  with Problem (problem, subst_seq) ->
    "= "^(string_of_problem_and_subst (problem, subst_seq))
    | Fail -> "= Fail"

type step_label = Delete | Decompose | Orient | Eliminate

let string_of_step_label step_label =
  match step_label
  with Delete -> "Delete"
    | Decompose-> "Decompose"
    | Orient -> "Orient"
    | Eliminate -> "Eliminate"
        
type input_step =
    MidStep of (problem * subst_seq) * step_label * equation * bool (* = present *)
  | FailStep of step_label * equation * bool
  | LastStep of substitution

let string_of_input_step input_step =
  match input_step
  with MidStep ((problem,subst_seq), step_label, equation, had_start_eq) ->
    "= " ^ (string_of_problem_and_subst (problem,subst_seq)) ^ " by " ^
      (string_of_step_label step_label)^
      " on " ^ (string_of_equation equation)
    | FailStep (step_label, equation, had_start_eq) ->
      "= Fail " ^ (string_of_step_label step_label)^
      " on " ^ (string_of_equation equation)
    | LastStep substitution -> "= " ^ (string_of_substitution substitution)

type full_input = (problem * subst_seq)option * input_step list

let string_of_full_input (question_opt, input_steps) =
  (match question_opt with Some (problem, subst_seq) ->
    string_of_problem_and_subst (problem, subst_seq) ^"\n"
    | None -> "") ^
    string_of_list "" "\n" "" string_of_input_step input_steps

(* I need to revisit this ---ELG *)
type result =
    Success of bool * int
  | DoesNotApply of step_label * equation
  | CantFail of step_label
  | DoesNotFailOn of step_label * equation
  | ShouldFail of step_label * equation * string
  | DetailedFailure of string * ((int * int) * (int * int))
  | NotDone
  | ReasonEquationNotInProblem of equation
  | NotOrigProblem of (*student_problem * student_problem*) ((problem * subst_seq) * (problem * subst_seq))
  | TooFar of int
  | ParseError of string

let string_of_result result =
  match result
  with Success(_,n) -> "Successful step, partial score = "^string_of_int n^"\n"
    | DoesNotApply (step_label, eq) ->
      (string_of_step_label step_label) ^" does not apply to "^
        (string_of_equation eq)^"\n"
    | CantFail step_label ->
      (string_of_step_label step_label) ^
        ", correctly called, can not fail.\n"
    | DoesNotFailOn (step_label, eq) ->
      (string_of_step_label step_label) ^" does not apply to "^
        (string_of_equation eq)^"\n"
    | DetailedFailure (s,_) -> s^"\n"
    | ShouldFail (step_label, equation, reason) -> reason
    | NotDone -> "Unification constraint set is not empty.\nShow the step of (Unify {} o subst) before going straight to the final step.\n"
    | ReasonEquationNotInProblem eq ->
      "The given equation "^(string_of_equation eq)^
        " is not in the previous unification problem.\n"
    | NotOrigProblem (problem, start_problem) ->
      "The restated problem\n"^((*string_of_student_problem*)string_of_problem_and_subst problem)^
        "\nis not the same as the original given problem\n"^
        ((*string_of_student_problem*)string_of_problem_and_subst start_problem)
    | TooFar n -> "You have steps past the end of the problem.\n"
    | ParseError s -> s^"\n"
 
(* I should probably use these instead.  Maybe another day ---ELG
type ('a,'b)gen_basic_error = {right : 'a; wrong : 'b}
type 'a basic_err = ('a,'a)gen_basic_error

(* What can go wrong with a single problem?
1 First, the reason can be wrong:
*)
type reason_error =
(*
  a) The equation can fail to be in the original problem.
*)
    ReasonEquationNotInOriginalProblem of equation
(*
     Then what do we think?
     i) They are not trying to solve this problem but maybe their answer
        to a previous run at this problem 
        A) We can try using the answer to the previous attempt at this
           prompt in place of the prompt to see if they tried to force
           their way on
     ii) Probably they replaced the equation with the result to be added back
        into the constraint set.  Can we / do we want to look for this?
        A) For now, I will not.
*)
  | WrongReasonForGivenEquation of (step_label basic_errr * equation)
(*
  b) The equation is there, but can not be used with the given reason
     i) Assume that it is the right equation for the wrong stated reason
        A) Replace the reason with the reason that does potentially apply
           I) Handle Eliminate a = a -> Fail specially
     ii) Assume that it is the right reason for the wrong equation
        A) I think I have to assume it is not this case, because
           otherwise we have to guess what equation they might have
           been trying to use.  We could do that, but would require
           search every equation for a match that is likely to still
           be broken.
*)

(*
2 The resultant constraint set is not the correct result of the original
  problem.
*)
type constraint_error =
    
(*
  a) The costraint can be wrong by not having all the equations of the
     original problem constraint, minus the equation in the reason,
     transformed by the reason process.
     i) If the reason is Eliminate, this could mean they didn't do the
         substitution, or they didn't do it correctly.
     ii) This could be a sign that this answer is trying to be the answer
         to the previous (incorrect) answer.

*)
    EquationsOmittedFromAnswerConstraintsButInProblem of equation list
(*
  b) The constraints can be wrong by having the "reason" equation.
*)
  | ReasonEquationPresentInAnswerConstraints of equation
(*
  c) The constaint can be wrong by having equations that are not in the
     original problem as it is transformed by the reason, or the additions
     specifically made by the reason.
*)
  | EquationsIncorrectlyAddedToAnswerConstraints of equation list

(*
3) The accumulated substitution is not the correct result of updating
   the problem substitution
*)
type substituion_error =
(*
  a) The domain of the answer substituion may fail to conatin the
     domain of the problem substituion.
*)
  SubstitutionFromProblemOmmittedInAnswer of simple_substitution list
(*
  b) The substitution in the answer has the wrong range for a variable
     in the domain of the original substitution
     i) if the "reason" is Eliminate, this probably means they didn't
        compose/update properly
     ii) if the "reason" is not Eliminate, this might indicate they were
         working with their previous (incorrect) answer instead of the
         current problem.
*)
  | SubstitutionFromProblemHaveWrongImageInAnswer of simple_substitution list
(*
  c) The substitution adds incorrect substitutions, not part of the
     update for the reason
*)
  | SubsitutionsNotFromProblemOrReasonInAnswer of simple_substitution list
(*
  d) A substitution from the reason has incorrect image
*)
  | SubstitutionsFromReasonWrongInAnswer or simple_substitution list
*)

let report_equation_errs label errs =
  match errs with [] -> ""
    | _ -> string_of_list label ",\n" "" string_of_equation errs

let report_substitution_errs label errs =
  match errs with [] -> ""
    | _ -> string_of_list label  ",\n" "" string_of_simple_substitution errs

let not_in_constraints_errors_to_string errs =
  report_equation_errs
  "The following equations are not in the new constraints, but should be:\n"
  errs

let in_constraints_errors_to_string errs =
  report_equation_errs
  "The following equations are in the new constraints, but shouldn't be:\n"
  errs

let constraints_errors errs =
  match errs with ([],[]) -> ("No constraint errors.\n", (0,0))
    | (in_errs, not_in_errs) ->
      ((in_constraints_errors_to_string in_errs) ^ "\n" ^
          (not_in_constraints_errors_to_string not_in_errs),
       (List.length in_errs, List.length not_in_errs))

let in_substitution_errors_to_string errs =
  report_substitution_errs
  "The following mappings are in the substitution, but shouldn't be:\n"
  errs

let not_in_substitution_errors_to_string errs =
  report_substitution_errs
  "The following mappings are not in the substitution, but should be:\n"
  errs

let substitution_errors errs =
  match errs with ([],[]) -> ("No substitution errors.\n", (0,0))
    | (in_errs, not_in_errs) ->
      ((in_substitution_errors_to_string in_errs) ^ "\n" ^
          (not_in_substitution_errors_to_string not_in_errs),
       (List.length in_errs, List.length not_in_errs))
(*
let problem_error errs =  
  match errs with ([],[]) -> "No error (this shouldn't print).\n"
    | (err1::_,_) -> "The equation "^(string_of_equation err1)^
                     " should not be in the problem set.\n"
    | ([], err2::_) -> "The equation "^(string_of_equation err2)^
                       " is missing from the problem set.\n"

let subst_error errs =
  match errs with  ([],[]) -> "No error (this shouldn't print).\n"
    | (err1::_,_) -> "Your substitution has "^
      (string_of_simple_substitution err1)^", which it should not.\n"
    | ([], err2::_) -> "Your substitution does not have "^
      (string_of_simple_substitution err2)^", which it should.\n"

let check_student_against_solution
    student_problem student_subst solution_problem solution_subst score =
  match (set_diff student_problem solution_problem)
  with  ([],[]) -> 
    (match set_diff (reduce_subst_seq student_subst)
                    (reduce_subst_seq solution_subst)
     with ([],[]) -> Success (false, score)
       | (err1, err2) -> Failure (subst_error (err1, err2)))
    | (err1,err2) ->
      Failure(problem_error(err1,err2))
      *)
let check_student_against_solution
    student_problem student_subst solution_problem solution_subst score =
  let (constraint_errs, (num_constr_in_errs, num_constr_not_in_errs)) =
    constraints_errors (set_diff student_problem solution_problem) in
  let (substitution_errs, (num_subst_in_errs, num_subst_not_in_errs)) =
    substitution_errors
      (set_diff
         (reduce_subst_seq student_subst)
         (reduce_subst_seq solution_subst)) in
  match ((num_constr_in_errs, num_constr_not_in_errs),
         (num_subst_in_errs, num_subst_not_in_errs))
  with ((0,0),(0,0)) -> Success(false, score)
    | ((_,_),(_,_)) ->
    DetailedFailure(constraint_errs^"\n"^substitution_errs,
     ((num_constr_in_errs, num_constr_not_in_errs),
      (num_subst_in_errs, num_subst_not_in_errs)))

let delete_check start_lst_minus_eq start_subst eq student_problem =
  match eq
  with (tm1,tm2) -> 
    if tm1 = tm2
    then
      (match student_problem
       with Problem (stu_problem, stu_subst) ->
         check_student_against_solution
           stu_problem stu_subst start_lst_minus_eq start_subst 1
         | Fail -> CantFail Delete)
    else DoesNotApply (Delete, eq)

let decompose_check start_lst_minus_eq start_subst eq stu_problem =
  match eq
  with (Const (c1, args1), Const (c2, args2)) ->
    (match stu_problem, c1 = c2, List.length args1 = List.length args2 
     with Problem (stu_problem, stu_subst), const_eq, arity_ok ->
       if const_eq
       then
         if arity_ok 
         then
           check_student_against_solution stu_problem stu_subst
             ((List.combine args1 args2)@start_lst_minus_eq) start_subst 2
         else ShouldFail
           (Decompose, eq,
            ("Decompose: Different arities for constructor "^c1^
                " in the terms in the\nequation "^
                (string_of_equation eq)^"\n"))
       else ShouldFail 
           (Decompose, eq,
            ("Decompose: Constructors "^c1^" and "^c2^
              " are not the same in the\nequation "^
              (string_of_equation eq)^"\n"))
       | (Fail, true, true) -> DoesNotFailOn (Decompose, eq)
       | (Fail, _, _) -> Success (true,2))
    | _ -> DoesNotApply (Decompose, eq)

let orient_check start_lst_minus_eq start_subst eq stu_problem =
  match eq with (Const (c,args), Var v) ->
    (match stu_problem
     with Problem (stu_problem, stu_subst) ->
       check_student_against_solution stu_problem stu_subst
         ((Var v, Const (c,args))::start_lst_minus_eq) start_subst 1
       | Fail -> CantFail Orient)
    | _ -> DoesNotApply (Orient, eq)

(* Put in terms of stu_problem *)
let eliminate_check start_lst_minus_eq start_subst eq stu_problem =
  match eq
  with (Var v, tm) -> 
    if not(occurs v tm) 
    then
      let subst_inc = [(v, tm)] in
      let solution_problem = 
        sort_uniq compare
        (List.map
           (fun (tm1, tm2) ->
             (term_subst subst_inc tm1, term_subst subst_inc tm2))
           start_lst_minus_eq)
      in 
      let solution_subst = Compose (subst_inc, start_subst) in
      (match stu_problem
       with Problem (stu_problem, stu_subst) ->
         check_student_against_solution stu_problem stu_subst
           solution_problem solution_subst 3
         | Fail -> DoesNotFailOn (Eliminate, eq))
    else
      (if tm = Var v
       then DoesNotApply (Eliminate, eq)
       else
          match  stu_problem
          with Problem (stu_problem, stu_subst) ->
            ShouldFail 
              (Eliminate, eq,
               ("Eliminate: Failed to detect an occurs-check error in the\nequation\n  "^
              (string_of_equation eq)^"\n"))
            | Fail -> Success (true,3))
    | _ -> DoesNotApply (Eliminate, eq)

let complete start_problem start_subst student_subst =
  match start_problem
  with [] -> 
    (match set_diff student_subst (reduce_subst_seq start_subst)
     with ([],[]) -> Success (true,3)
       | (err1, err2) ->
         let (subst_err_msg, (num_in_errs, num_out_errs)) =
           substitution_errors (err1, err2) in
         DetailedFailure
           (subst_err_msg, ((0,0),(List.length err1, List.length err2))))
    | _ -> NotDone

let step_result (start_problem, start_subst) (student_problem, step_label, eq)=
  let (single_eq, start_lst_minus_eq) =
    List.partition (fun e -> e = eq) start_problem
  in
  (match single_eq with [] -> ReasonEquationNotInProblem eq
    | _ ->
      (match step_label
       with Delete ->
         (delete_check start_lst_minus_eq start_subst eq student_problem)
         | Decompose ->
           (decompose_check start_lst_minus_eq start_subst eq student_problem)
         | Orient ->
           (orient_check start_lst_minus_eq start_subst eq student_problem)
         | Eliminate ->
           (eliminate_check start_lst_minus_eq start_subst eq student_problem)))


let full_step_result ((start_problem, start_subst), input_step) =
  match input_step
  with MidStep ((problem,subst), step_label, eq, _)->
    step_result
      (start_problem, start_subst)
      (Problem (problem,subst), step_label, eq)
    | FailStep (step_label, eq, _)->
      step_result (start_problem, start_subst) (Fail, step_label, eq)
    | LastStep student_subst ->
      (complete start_problem start_subst student_subst)

let rec steps_result full_start_problem steps part_score =
  match steps with [] -> Success(false,part_score)
    | input_step::more_steps ->
      (match full_step_result (full_start_problem, input_step)
       with Success (is_done, new_score) ->
         (if is_done then
           (match more_steps with [] -> Success(is_done, new_score + part_score)
             | _ -> TooFar (new_score + part_score))
          else
           (match input_step
            with  MidStep ((prob,subst), _, _, _)->
              steps_result (prob,subst) more_steps (new_score + part_score)
              | _ -> raise (Failure "Shouldn't be able to happen.")))
         | res -> res)


let rec full_input_result ((full_start_problem:(problem*subst_seq)), ((question_opt:(problem*subst_seq) option), steps)) =
  match question_opt
  with None -> steps_result full_start_problem steps 0
    | Some problem ->
      if (problem = full_start_problem)
      then
        (match problem with (*Problem*) (start_problem, start_subst) ->
            steps_result (start_problem, start_subst) steps 0
         (* | Fail -> TooFar 0*))
      else NotOrigProblem (problem, full_start_problem)
