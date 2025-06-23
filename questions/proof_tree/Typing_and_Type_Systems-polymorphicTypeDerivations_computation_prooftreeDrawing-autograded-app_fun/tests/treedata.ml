(* File: genutils.ml *)
(* Author: Elsa L. Gunter, John Lee *)
(* Copyright 2017 *)
(* Share and Enjoy *)

open Genutils
open Common
open Lex
open Parse

type source =
    ExpSrc of exp
  | EnvSrc of type_env
  | TySrc of monoTy
  | SubstSrc of substitution
  | JdgmtSrc of type_env * exp * monoTy
  | ConcrSynSrc of string
  | ErrSrc of string

let rec string_of_source src =
  match src
  with ExpSrc exp -> string_of_exp exp
  | EnvSrc type_env -> string_of_type_env type_env
  | TySrc monoTy -> string_of_monoTy monoTy
  | SubstSrc substitution -> string_of_substitution substitution
  | JdgmtSrc (gamma, e, ty) -> string_of_judgment (ExpJudgment (gamma, e, ty))
  | ConcrSynSrc string -> string
  | ErrSrc string -> "Error: "^string

type typing_stmt = TypingStmt of type_env parsed * exp parsed * monoTy parsed

let string_of_typing_stmt (TypingStmt (tyenv_p, exp_p, monoTy_p)) =
  (string_of_parsed string_of_type_env tyenv_p) ^ " |- "
  ^ (string_of_parsed string_of_exp exp_p) ^ " : "
  ^ (string_of_parsed string_of_monoTy monoTy_p) ^ "\n"

type side_cond = substitution

type proof_tree = ProofTree of proof_tree list * string * typing_stmt * side_cond parsed

  (* processing functions *)

let process kind processor input =
    try
      if input = "" then ParseEmpty else
      ParseOk (processor token (Lexing.from_string input))
    with Parsing.Parse_error ->
      (* prerr_endline ("Syntax error in " ^ kind ^ ": " ^ input); *)
      SyntaxError input

let process_node (node : unprocessed_node): typing_stmt =
  let left          = process "type env" type_env   node.str_left in
  let middle        = process "exp"      expression node.str_middle in
  let right         = process "monoty"   mono_ty    node.str_right in
  TypingStmt(left, middle, right)

let process_sc s_str = process "side_condition" side_condition s_str