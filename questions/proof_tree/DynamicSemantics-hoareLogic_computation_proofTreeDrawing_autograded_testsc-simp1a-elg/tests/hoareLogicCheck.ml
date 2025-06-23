(* File: hoareLogicCheck.ml *)
(* Author: John Lee & Elsa Gunter *)
(* Copyright 2017 *)


open Genutils
open Gencheck
open Common
open Solution
open Treedata
open Student

open Interface
open Hltest

  (*
    #################################
     HOARE TRIPLE TO TERM CONVERSION
    #################################
  *)

let to_unit_parsed (p: 'a parsed): unit parsed list = match p with
  | ParseEmpty -> [ParseEmpty]
  | SyntaxError s -> [SyntaxError s]
  | _ -> []

let binop_to_term b =
  let s = string_of_bin_op b in TermElem(s, [], ConcrSynSrc s)

(*
let rec exp_to_term exp =
  let src = ExpSrc exp in match exp with
  | Ident x -> TermElem("ident", [TermElem(x, [], ConcrSynSrc x)], src)
  | IntConst i -> let s = string_of_int i in
    TermElem("i_const", [TermElem(s, [], ConcrSynSrc s)], src)
  | MonOpAppExp(IntNegOp, e) -> TermElem("negop", [exp_to_term e], src)
  | BinOpAppExp(b, e1, e2) -> TermElem("binop", [binop_to_term b; exp_to_term e1; exp_to_term e2], src)
*)

type hl_obj =
  ExpObj of exp
  | BExpObj of booleanexp

let e_obj e = TermObj(ExpObj e, ExpSrc e)

let rec bexp_to_term b =
  let src = BoolExpSrc b in match b with
  | BoolConst true -> TermElem("true", [], src)
  | BoolConst false -> TermElem("false", [], src)
  | NotExp b -> TermElem("notop", [bexp_to_term b], src)
  | AndExp(b1, b2) -> TermElem("andop", [bexp_to_term b1; bexp_to_term b2], src)
  | OrExp(b1, b2) -> TermElem("orop", [bexp_to_term b1; bexp_to_term b2], src)
  | LessExp(e1, e2) -> TermElem("lessop", [e_obj e1; e_obj e2], src)
  | LessEqExp(e1, e2) -> TermElem("leqop", [e_obj e1; e_obj e2], src)
  | GreaterExp(e1, e2) -> TermElem("greatop", [e_obj e1; e_obj e2], src)
  | GreaterEqExp(e1, e2) -> TermElem("geqop", [e_obj e1; e_obj e2], src)
  | EqExp(e1, e2) -> TermElem("eqop", [e_obj e1; e_obj e2], src)
  | ImpExp(b1, b2) -> TermElem("impop", [bexp_to_term b1; bexp_to_term b2], src)

let rec cmd_to_term cmd =
  let src = CmdSrc cmd in match cmd with
  | AssignCommand(x, e) -> TermElem("assign", [TermElem(x, [], ConcrSynSrc x); e_obj e], src)
  | SeqCommand(c1, c2) -> TermElem("seq", [cmd_to_term c1; cmd_to_term c2], src)
  | IfCommand(b, c1, c2) -> TermElem("if_cmd", [bexp_to_term b; cmd_to_term c1; cmd_to_term c2], src)
  | WhileCommand(b, c) -> TermElem("while", [bexp_to_term b; cmd_to_term c], src)

let h_triple_to_parsed_term ht = match ht with
  HTriple(p_p, r_p, q_p) -> (match (p_p, r_p, q_p) with
    | (ParseOk p, ParseOk r, ParseOk q) ->
      let src = HTSrc ht
      in JudgeParse (TermElem("judgment", [cmd_to_term r; bexp_to_term p; bexp_to_term q], src))
    | _ -> JudgeErr ((to_unit_parsed r_p) @ (to_unit_parsed p_p) @ (to_unit_parsed q_p))
  )

let sc_to_parsed_term sc_str = match process_h_sc sc_str with
  ParseOk sc_val -> JudgeParse (bexp_to_term sc_val)
  | ParseEmpty -> JudgeParse (TermElem("no_sc", [], ErrSrc "no_sc"))
  | _ -> JudgeErr [SyntaxError sc_str]

  (*
    ##############################
     HOARE LOGIC RULE DEFINITIONS
    ##############################
  *)

type hl_rule = (hl_obj, source, unit) syntax_rule

  (* rules for arithmetic expressions *)

let rec exp_subst e' x e = match e' with
  IntConst i -> IntConst i
  | Ident x' -> if x = x' then e else Ident x'
  | MonOpAppExp(op, e_c) -> MonOpAppExp(op, exp_subst e_c x e)
  | BinOpAppExp(op, e1, e2) -> BinOpAppExp(op, exp_subst e1 x e, exp_subst e2 x e)

let rec term_subst term x e = match term with
  | TermElem(e_con, child_list, src) ->
    TermElem(e_con, List.map (fun child -> term_subst child x e) child_list, src)
  | TermObj(ExpObj e', src) ->
    let e_n = exp_subst e' x e in TermObj(ExpObj e_n, src)
  | TermObj(e', src) -> TermObj(e', src)

let rec term_eq _ t1 t2 = match (t1, t2) with
  | (TermElem(c1, cl1, _), TermElem(c2, cl2, _)) ->
    c1 = c2 && term_list_eq cl1 cl2
  | (TermObj(e1, _), TermObj(e2, _)) -> e1 = e2
  | _ -> false
and term_list_eq cl1 cl2 = match (cl1, cl2) with
  | ([], []) -> true
  | (c1 :: ct1, c2 :: ct2) -> (term_eq "" c1 c2) && term_list_eq ct1 ct2 
  | _ -> false

let assign_sc env =
  match (lookup_env env "e", lookup_env env "px") with
  | (Some (TermObj (ExpObj e, _)), Some (px_term)) ->
    (match (lookup_env env "x", lookup_env env "p") with
      | (Some (TermElem(x, [], _)), Some(p_term)) ->
        if term_eq "" px_term (term_subst p_term x e) then None else Some ()
      | _ -> Some ()
    )
  | _ -> Some () 

let assign_rule: hl_rule = SynRule("Assign", "assign",
  PatElem("judgment", [pat_cons "assign" ["x"; "e"]; PatVar "px"; PatVar "p"]), [],
  [SCPred("assign_sc", assign_sc)]
)

let seq_rule: hl_rule = SynRule("Seq", "seq",
  PatElem("judgment", [pat_cons "seq" ["s1"; "s2"]; PatVar "p"; PatVar "r"]), [
    PatElem("judgment", [PatVar "s1"; PatVar "p"; PatVar "q"]);
    PatElem("judgment", [PatVar "s2"; PatVar "q"; PatVar "r"])
  ], []
)

let if_rule: hl_rule = SynRule("IfThenElse", "if_cmd",
  PatElem("judgment", [pat_cons "if_cmd" ["b"; "s1"; "s2"]; PatVar "p"; PatVar "q"]), [
    PatElem("judgment", [PatVar "s1"; pat_cons "andop" ["p"; "b"]; PatVar "q"]);
    PatElem("judgment", [PatVar "s2"; PatElem("andop", [PatVar "p"; pat_cons "notop" ["b"]]); PatVar "q"])
  ], []
)

let while_rule: hl_rule = SynRule("While", "while",
  PatElem("judgment", [pat_cons "while" ["b"; "s"]; PatVar "p"; PatElem("andop", [PatVar "p"; pat_cons "notop" ["b"]])]), [
    PatElem("judgment", [PatVar "s"; pat_cons "andop" ["p"; "b"]; PatVar "p"])
  ], []
)


let pre_str_sc env =
  match (lookup_env env "sc", lookup_env env "p", lookup_env env "px") with
  | (Some (TermElem("impop", [p_t; px_t], BoolExpSrc e)), Some p_term, Some px_term) ->
    if (term_eq "" p_t p_term) && (term_eq "" px_t px_term) then
      if validCheck e then None else Some ()
    else Some ()
  | _ -> Some ()

let pre_str_rule: hl_rule = SynRule("PreStr", "any",
  PatElem("judgment", [PatVar "s"; PatVar "p"; PatVar "q"]), [
    PatElem("judgment", [PatVar "s"; PatVar "px"; PatVar "q"])
  ], [SCPred("pre_str_sc", pre_str_sc)]
)

let post_weak_sc env =
  match (lookup_env env "sc", lookup_env env "q", lookup_env env "qx") with
  | (Some (TermElem("impop", [qx_t; q_t], BoolExpSrc e)), Some q_term, Some qx_term) ->
    if (term_eq "" q_t q_term) && (term_eq "" qx_t qx_term) then
      if validCheck e then None else Some ()
    else Some ()
  | _ -> Some ()

let post_weak_rule: hl_rule = SynRule("PostWeak", "any",
  PatElem("judgment", [PatVar "s"; PatVar "p"; PatVar "q"]), [
    PatElem("judgment", [PatVar "s"; PatVar "p"; PatVar "qx"])
  ], [SCPred("post_weak_sc", post_weak_sc)]
)

  (*
    ##############################
     COMPLETED PROBLEM DEFINITION
    ##############################
  *)

let string_of_failed_sc s = match s with
  | "assign_sc" -> "Performing the expected substitution on post-condition does not produce expected pre-condition for assignment."
  | "pre_str_sc" -> "Pre-strengthening side condition either does not match judgments, or was shown to be invalid through testing."
  | "post_weak_sc" -> "Post-weakening side condition either does not match judgments, or was shown to be invalid through testing."
  (*| "pre_str_sc" -> "Pre-strengthening side condition either does not match judgments, or could not be proven by the solver."*)
  | _ -> raise (Failure ("BUG: tdcheck - Unknown side condition error. String \"" ^ s ^ "\" is not a recognized sc string."))

  (* NOTE: rules with the same label must use the same number of points *)

let hoareLogic_pdef : (h_triple, hl_obj, source, unit) problem_def = {
  process = process_h_triple;
  judgment_to_term = h_triple_to_parsed_term;
  sc_val = sc_to_parsed_term;
  rule_list = [
    (3, pre_str_rule); (3, post_weak_rule);
    (3, assign_rule); (7, while_rule); (5, if_rule); (3, seq_rule)
    (*(1, ident_rule); (1, num_rule); (2, negop_rule); (2, binop_rule); (2, compop_rule);
    (1, true_rule); (1, false_rule); (2, and_rule1); (2, and_rule2); (2, or_rule1); (2, or_rule2);
      (2, not_rule1); (2, not_rule2);
    (2, skip_rule); (3, assign_rule); (3, seq_rule); (5, if_rule1); (5, if_rule2);
      (7, while_rule1); (7, while_rule2)*)
  ];
  eq_poly_fun = term_eq;
  string_of_judgment = string_of_h_triple;
  string_of_source = string_of_source;
  string_of_cons = (fun x -> x);
  string_of_failed_sc = string_of_failed_sc
}

let _ = Interface.grade hoareLogic_pdef Student.tree

