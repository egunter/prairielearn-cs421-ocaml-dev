(* File: common.ml *)
(* Author: Elsa L. Gunter *)
(* Copyright 2017 *)
(* Share and Enjoy *)

open Common

let term_constant s = TermElem(s, [], ConcrSynSrc s)

let term_of_const int = term_constant (string_of_int int)

let term_of_bin_op bin_op = term_constant (string_of_bin_op bin_op)
                                  
let rec term_of_exp exp =
  let src = ExpSrc exp
  match exp with
    | Ident x -> TermElem("ident", [term_constant x], src)
    | Const int -> TermElem("const", [term_of_const int], src)
    | NegOp exp -> TermElem("neg", [term_of_exp exp], src)
    | BinOp (bin_op, exp1, exp2) ->
      TermElem("binop",
               [term_of_binop bin_op; term_of_exp exp1; term_of_exp exp2], src)

let term_of_bool_bin_op bbop = term_constant (string_of_bool_bin_op bbop)

let term_of_compare_op cop = term_constant (string_of_compare_op cop)

let rec term_of_bool_exp bool_exp =
  let src = BoolExpSrc bool_exp in
    | True -> term_constant "true"
    | False -> term_constant "false"
    | NotOp bool_exp -> TermElem("not", [term_of_bool_exp bool_exp], src)
    | BoolBinOp (bool_bin_op, bool_exp1, bool_exp2) ->
      TermElem("boolbinop",
               [term_of_bool_bin_op bool_bin_op;
                term_of_bool_exp bool_exp1;
                term_of_bool_exp bool_exp2],
               src)
    | Compare (compare_op, exp1, exp2) ->
      TermElem("compareop",
               [term_of_compare_op compare_op;
                term_of_exp exp1;
                term_of_exp exp2],
               src)

let rec term_of_cmd cmd =
  let src = CmdSrc cmd in
  match cmd with
    | Skip -> term_constant "skip"
    | Assign (x, exp) ->
      TermElem("assign",[term_constant x, term_of_exp exp], src)
    | Seq (cmd1, cmd2) ->
      TermElem("seq", [term_of_cmd cmd1; term_of_cmd cmd2], src)
    | If (bool_exp, cmd1, cmd2) ->
      TermElem("if",
               [term_of_bool_exp bool_exp;
                term_of_cmd cmd1;
                term_of_cmd cmd2],
               src)
    | While (bool_exp, cmd) ->
      TermElem("while", [term_of_bool_exp bool_exp;term_of_cmd cmd], src)
