(* File: natSemCheck.ml *)
(* Author: John Lee & Elsa Gunter *)
(* Copyright 2017 *)

open Genutils
open Gencheck
open Common
open Solution
open Treedata
open Student

open Interface

  (*
    #############################
     JUDGMENT TO TERM CONVERSION
    #############################
  *)

let to_unit_parsed (p: 'a parsed): unit parsed list = match p with
  | ParseEmpty -> [ParseEmpty]
  | SyntaxError s -> [SyntaxError s]
  | _ -> []

let binop_to_term b =
  let s = string_of_bin_op b in TermElem(s, [], ConcrSynSrc s)

let rec exp_to_term exp =
  let src = ExpSrc exp in match exp with
  | Ident x -> TermElem("ident", [TermElem(x, [], ConcrSynSrc x)], src)
  | Const i -> let s = string_of_int i in
    TermElem("i_const", [TermElem(s, [], ConcrSynSrc s)], src)
  | NegOp e -> TermElem("negop", [exp_to_term e], src)
  | BinOp(b, e1, e2) -> TermElem("binop", [binop_to_term b; exp_to_term e1; exp_to_term e2], src)

let bool_binop_to_term op =
  let s = string_of_bool_bin_op op in TermElem(s, [], ConcrSynSrc s)

let compop_to_term op =
  let s = string_of_compare_op op in TermElem(s, [], ConcrSynSrc s)

let rec bool_exp_to_term b =
  let src = BoolExpSrc b in match b with
  | True -> TermElem("true", [], src)
  | False -> TermElem("false", [], src)
  | NotOp b -> TermElem("notop", [bool_exp_to_term b], src)
  | BoolBinOp(op, b1, b2) -> TermElem("boolbinop", [bool_binop_to_term op; bool_exp_to_term b1; bool_exp_to_term b2], src)
  | Compare(op, e1, e2) -> TermElem("compop", [compop_to_term op; exp_to_term e1; exp_to_term e2], src)

let rec cmd_to_term cmd =
  let src = CmdSrc cmd in match cmd with
  | Skip -> TermElem("skip", [], src)
  | Assign(x, e) -> TermElem("assign", [TermElem(x, [], ConcrSynSrc x); exp_to_term e], src)
  | Seq(c1, c2) -> TermElem("seq", [cmd_to_term c1; cmd_to_term c2], src)
  | If(b, c1, c2) -> TermElem("if_cmd", [bool_exp_to_term b; cmd_to_term c1; cmd_to_term c2], src)
  | While(b, c) -> TermElem("while", [bool_exp_to_term b; cmd_to_term c], src)

let tte_to_term j = match j with
  | ExpTTE e -> exp_to_term e
  | CmdTTE c -> cmd_to_term c
  | BoolExpTTE b -> bool_exp_to_term b

let res_to_term r = match r with
  | IntResult i -> let s = string_of_int i in
    TermElem("i_val", [TermElem(s, [], ConcrSynSrc s)], ConcrSynSrc s)
  | BoolResult b -> let s = if b then "true" else "false" in
    TermElem("b_val", [TermElem(s, [], ConcrSynSrc s)], ConcrSynSrc s)
  | MemoryResult m ->
    TermElem("mem_val", [TermObj(m, MemorySrc m)], MemorySrc m)

let judgment_to_parsed_term jx = match jx with
  Judgment(jm_p, r_p) -> (match (jm_p, r_p) with
    | (ParseOk (j, m), ParseOk r) ->
      let src = JdgmtSrc jx
      in JudgeParse (TermElem("judgment", [tte_to_term j; TermObj(m, MemorySrc m); res_to_term r], src))
    | _ -> JudgeErr ((to_unit_parsed jm_p) @ (to_unit_parsed r_p))    
  )

  (*
    ####################################
     NATURAL SEMANTICS RULE DEFINITIONS
    ####################################
  *)

type nat_rule = (memory, source, unit) syntax_rule

  (* rules for arithmetic expressions *)

let ident_sc env =
  match (lookup_env env "m", lookup_env env "i") with
  | (Some (TermObj (mem, _)), Some (TermElem(i, _, _))) ->
    (match lookup_env env "r" with
      | Some(TermElem("i_val", [TermElem(v, _, _)], _)) ->
        (match lookup_env mem i with
          | Some v' -> if v' = (int_of_string v) then None else Some ()
          | None -> Some ()
        )
      | _ -> Some ()
    )
  | _ -> Some ()

let ident_rule: nat_rule = SynRule("Ident", "ident",
  PatElem("judgment", [pat_cons "ident" ["i"]; PatVar "m"; PatVar "r"]), [
  ], [SCPred("ident_sc", ident_sc)]
)

let num_sc env =
  match (lookup_env env "n", lookup_env env "r") with
  | (Some (TermElem(n, _, _)), Some (TermElem("i_val", [TermElem(v, _, _)], _))) ->
    if (int_of_string n) = (int_of_string v) then None else Some ()
  | _ -> Some () 

let num_rule = SynRule("Num", "i_const",
  PatElem("judgment", [pat_cons "i_const" ["n"]; PatVar "m"; PatVar "r"]), [
  ], [SCPred("num_sc", num_sc)]
)

let negop_sc env =
  match (lookup_env env "i", lookup_env env "ni") with
  | (Some (TermElem("i_val", [TermElem(vi, _, _)], _)), Some (TermElem("i_val", [TermElem(nvi, _, _)], _))) ->
    if -(int_of_string vi) = (int_of_string nvi) then None else Some ()
  | _ -> Some ()  

let negop_rule: nat_rule = SynRule("NegOp", "negop",
  PatElem("judgment", [pat_cons "negop" ["e"]; PatVar "m"; PatVar "ni"]), [
    PatElem("judgment", [PatVar "e"; PatVar "m"; PatVar "i"])
  ], [SCPred("negop_sc", negop_sc)]
)

let rec fun_of_binop m = match m with
  | " + " -> (fun i1 i2 -> i1 + i2)
  | " - " -> (fun i1 i2 -> i1 - i2)
  | " * " -> (fun i1 i2 -> i1 * i2)
  | _ -> raise (Failure "BUG: natSemCheck.ml - unknown binop checked in side condition")

let binop_sc env =
  match (lookup_env env "i1", lookup_env env "i2") with
  | (Some (TermElem("i_val", [TermElem(v1, _, _)], _)), Some (TermElem("i_val", [TermElem(v2, _, _)], _))) ->
    (match (lookup_env env "ix", lookup_env env "op") with
      | (Some (TermElem("i_val", [TermElem(vx, _, _)], _)), Some (TermElem(op, _, _))) ->
        let f = fun_of_binop op
        in if f (int_of_string v1) (int_of_string v2) = (int_of_string vx) then None else Some ()
      | _ -> Some ()
    )
  | _ -> Some ()

let binop_rule: nat_rule = SynRule("BinOp", "binop",
  PatElem("judgment", [pat_cons "binop" ["op"; "e1"; "e2"]; PatVar "m"; PatVar "ix"]), [
    PatElem("judgment", [PatVar "e1"; PatVar "m"; PatVar "i1"]);
    PatElem("judgment", [PatVar "e2"; PatVar "m"; PatVar "i2"])
  ], [SCPred("binop_sc", binop_sc)]
)

  (* rules for boolean expressions *)

let true_rule = SynRule("BoolConst", "true",
  PatElem("judgment", [pat_cons "true" []; PatVar "m"; PatElem("b_val", [PatElem("true", [])])]
  ), [], []
)

let false_rule = SynRule("BoolConst", "false",
  PatElem("judgment", [pat_cons "false" []; PatVar "m"; PatElem("b_val", [PatElem("false", [])])]
  ), [], []
)

let and_rule1 = SynRule("And", "boolbinop",
  PatElem("judgment", [PatElem("boolbinop", [PatElem(" & ", []); PatVar "e1"; PatVar "e2"]); PatVar "m"; PatElem("b_val", [PatElem("false", [])])]
  ), [
    PatElem("judgment", [PatVar "e1"; PatVar "m"; PatElem("b_val", [PatElem("false", [])])])
  ], []
)

let and_rule2 = SynRule("And", "boolbinop",
  PatElem("judgment", [PatElem("boolbinop", [PatElem(" & ", []); PatVar "e1"; PatVar "e2"]); PatVar "m"; PatElem("b_val", [PatElem("true", [])])]
  ), [
    PatElem("judgment", [PatVar "e1"; PatVar "m"; PatElem("b_val", [PatElem("true", [])])]);
    PatElem("judgment", [PatVar "e2"; PatVar "m"; PatElem("b_val", [PatElem("true", [])])])
  ], []
)

let or_rule1 = SynRule("Or", "boolbinop",
  PatElem("judgment", [PatElem("boolbinop", [PatElem(" or ", []); PatVar "e1"; PatVar "e2"]); PatVar "m"; PatElem("b_val", [PatElem("true", [])])]
  ), [
    PatElem("judgment", [PatVar "e1"; PatVar "m"; PatElem("b_val", [PatElem("true", [])])])
  ], []
)

let or_rule2 = SynRule("Or", "boolbinop",
  PatElem("judgment", [PatElem("boolbinop", [PatElem(" or ", []); PatVar "e1"; PatVar "e2"]); PatVar "m"; PatElem("b_val", [PatElem("false", [])])]
  ), [
    PatElem("judgment", [PatVar "e1"; PatVar "m"; PatElem("b_val", [PatElem("false", [])])]);
    PatElem("judgment", [PatVar "e2"; PatVar "m"; PatElem("b_val", [PatElem("false", [])])])
  ], []
)

let not_rule1 = SynRule("Not", "notop",
  PatElem("judgment", [pat_cons "notop" ["e"]; PatVar "m"; PatElem("b_val", [PatElem("true", [])])]
  ), [
    PatElem("judgment", [PatVar "e"; PatVar "m"; PatElem("b_val", [PatElem("false", [])])])
  ], []
)

let not_rule2 = SynRule("Not", "notop",
  PatElem("judgment", [pat_cons "notop" ["e"]; PatVar "m"; PatElem("b_val", [PatElem("false", [])])]
  ), [
    PatElem("judgment", [PatVar "e"; PatVar "m"; PatElem("b_val", [PatElem("true", [])])])
  ], []
)

let rec fun_of_compop m = match m with
  | " < " -> (fun i1 i2 -> i1 < i2)
  | " = " -> (fun i1 i2 -> i1 = i2)
  | _ -> raise (Failure "BUG: natSemCheck.ml - unknown comparison op checked in side condition")

let compop_sc env =
  match (lookup_env env "i1", lookup_env env "i2") with
  | (Some (TermElem("i_val", [TermElem(v1, _, _)], _)), Some (TermElem("i_val", [TermElem(v2, _, _)], _))) ->
    (match (lookup_env env "ix", lookup_env env "op") with
      | (Some (TermElem("b_val", [TermElem(vx, _, _)], _)), Some (TermElem(op, _, _))) ->
        let f = fun_of_compop op
        in if f (int_of_string v1) (int_of_string v2) = (vx = "true") then None else Some ()
      | _ -> Some ()
    )
  | _ -> Some ()

let compop_rule: nat_rule = SynRule("Rel", "compop",
  PatElem("judgment", [pat_cons "compop" ["op"; "e1"; "e2"]; PatVar "m"; PatVar "ix"]), [
    PatElem("judgment", [PatVar "e1"; PatVar "m"; PatVar "i1"]);
    PatElem("judgment", [PatVar "e2"; PatVar "m"; PatVar "i2"])
  ], [SCPred("compop_sc", compop_sc)]
)

  (* rules for commands *)

let skip_rule = SynRule("Skip", "skip",
  PatElem("judgment", [PatElem("skip", []); PatVar "m1"; PatVar "m2"]), [], []
)

let assign_sc env =
  match (lookup_env env "m1", lookup_env env "m2") with
  | (Some (TermObj (m1, _)), Some (TermObj (m2, _))) ->
    (match (lookup_env env "i", lookup_env env "v") with
      | (Some (TermElem(i, _, _)), Some(TermElem("i_val", [TermElem(v, _, _)], _))) ->
        let mx = add_env m1 i (int_of_string v)
        in if equiv_env mx m2 then None else Some ()
      | _ -> Some ()
    )
  | _ -> Some () 

let assign_rule: nat_rule = SynRule("Assign", "assign",
  PatElem("judgment", [pat_cons "assign" ["i"; "e"]; PatVar "m1"; pat_cons "mem_val" ["m2"]]), [
    PatElem("judgment", [PatVar "e"; PatVar "m1"; PatVar "v"])
  ], [SCPred("assign_sc", assign_sc)]
)

let seq_rule = SynRule("Seq", "seq",
  PatElem("judgment", [pat_cons "seq" ["s1"; "s2"]; PatVar "m1"; pat_cons "mem_val" ["m3"]]), [
    PatElem("judgment", [PatVar "s1"; PatVar "m1"; pat_cons "mem_val" ["m2"]]);
    PatElem("judgment", [PatVar "s2"; PatVar "m2"; pat_cons "mem_val" ["m3"]])
  ], []
)

let if_rule1 = SynRule("If", "if_cmd",
  PatElem("judgment", [pat_cons "if_cmd" ["b"; "s1"; "s2"]; PatVar "m1"; pat_cons "mem_val" ["m2"]]), [
    PatElem("judgment", [PatVar "b"; PatVar "m1"; PatElem("b_val", [PatElem("true", [])])]);
    PatElem("judgment", [PatVar "s1"; PatVar "m1"; pat_cons "mem_val" ["m2"]])
  ], []
)

let if_rule2 = SynRule("If", "if_cmd",
  PatElem("judgment", [pat_cons "if_cmd" ["b"; "s1"; "s2"]; PatVar "m1"; pat_cons "mem_val" ["m2"]]), [
    PatElem("judgment", [PatVar "b"; PatVar "m1"; PatElem("b_val", [PatElem("false", [])])]);
    PatElem("judgment", [PatVar "s2"; PatVar "m1"; pat_cons "mem_val" ["m2"]])
  ], []
)

let while_rule1 = SynRule("While", "while",
  PatElem("judgment", [pat_cons "while" ["b"; "e"]; PatVar "m1"; pat_cons "mem_val" ["m2"]]), [
    PatElem("judgment", [PatVar "b"; PatVar "m1"; PatElem("b_val", [PatElem("false", [])])])
  ], []
)

let while_rule2 = SynRule("While", "while",
  PatElem("judgment", [pat_cons "while" ["b"; "s"]; PatVar "m1"; pat_cons "mem_val" ["m3"]]), [
    PatElem("judgment", [PatVar "b"; PatVar "m1"; PatElem("b_val", [PatElem("true", [])])]);
    PatElem("judgment", [PatVar "s"; PatVar "m1"; pat_cons "mem_val" ["m2"]]);
    PatElem("judgment", [pat_cons "while" ["b"; "s"]; PatVar "m2"; pat_cons "mem_val" ["m3"]])
  ], []
)

  (*
    ###################
     TERM OBJ EQUALITY
    ###################
  *)

  (* equality function *)

let gamma_equiv g1 g2 = match (g1, g2) with
  | (TermObj (m1, _), TermObj (m2, _)) -> equiv_env m1 m2
  | _ -> false

let nat_poly_fun s =
  if s = "m" || s = "m1" || s = "m2" || s = "m3" then gamma_equiv
  else (fun x y -> x = y)

  (*
    ##############################
     COMPLETED PROBLEM DEFINITION
    ##############################
  *)

let string_of_failed_sc s = match s with
  | "num_sc" -> "Evaluated result does not match numeric constant."
  | "ident_sc" -> "Identifier either does not exist in environment or lookup does not match evaluated result."
  | "negop_sc" -> "Use of negation operator is invalid."
  | "binop_sc" -> "Use of binary operator is invalid."
  | "compop_sc" -> "Use of comparison / relation operator is invalid."
  | "assign_sc" -> "Use of assignment rule does not evaluate to the given memory result."
  | _ -> raise (Failure ("BUG: tdcheck - Unknown side condition error. String \"" ^ s ^ "\" is not a recognized sc string."))

  (* NOTE: rules with the same label must use the same number of points *)

let natSem_pdef : (judgment, memory, source, unit) problem_def = {
  process = process_judgment;
  judgment_to_term = judgment_to_parsed_term;
  rule_list = [
    (1, ident_rule); (1, num_rule); (2, negop_rule); (2, binop_rule); (2, compop_rule);
    (1, true_rule); (1, false_rule); (2, and_rule1); (2, and_rule2); (2, or_rule1); (2, or_rule2);
      (2, not_rule1); (2, not_rule2);
    (2, skip_rule); (3, assign_rule); (3, seq_rule); (5, if_rule1); (5, if_rule2);
      (7, while_rule1); (7, while_rule2)
  ];
  eq_poly_fun = nat_poly_fun;
  string_of_judgment = string_of_judgment;
  string_of_source = string_of_source;
  string_of_cons = (fun x -> x);
  string_of_failed_sc = string_of_failed_sc
}

let _ = Interface.grade natSem_pdef Student.tree

