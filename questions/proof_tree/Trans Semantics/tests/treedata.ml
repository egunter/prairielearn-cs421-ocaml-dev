(* File: genutils.ml *)
(* Author: Elsa L. Gunter*)
(* Copyright 2017 *)
(* Share and Enjoy *)

open Genutils
open Common
open Lex
open Parse

(*
type judgment =
    ExpJudgment of exp parsed * memory parsed * int parsed
  | BoolExpJudgment of bool_exp parsed * memory parsed * bool parsed
  | CmdJudgment of cmd parsed * memory parsed * memory parsed
*)

type judgment =
  Judgment of (thing_to_eval * memory) parsed * trans_result parsed

let string_of_term_mem_pair (j, m) =
   "("^ (string_of_thing_to_eval j)^", "^ (string_of_memory m)
    ^")"

let string_of_judgment (Judgment(jm, r)) =
    (string_of_parsed string_of_term_mem_pair jm) ^ " evals to "^(string_of_parsed string_of_trans_result r)
  (*
  "("^ (string_of_parsed string_of_thing_to_eval j)^", "^ (string_of_parsed string_of_memory m)
    ^") evals to "^(string_of_parsed string_of_eval_result r)
*)

(*
let string_of_judgment j =
  match j with
    ExpJudgment (exp, memory, int) ->
    "("^string_of_exp exp^", "^ string_of_memory memory
    ^") evals to "^string_of_int int
    | BoolExpJudgment (bool_exp, memory, bool) ->
    "("^string_of_bool_exp bool_exp^", "^ string_of_memory memory
    ^") evals to "^ (if bool then "true" else "false")
  | CmdJudgment (cmd, memory1, memory2) ->
    "("^string_of_cmd cmd^", "^ string_of_memory memory1
    ^") evals to "^ string_of_memory memory2
*)

type source =
    ExpSrc of exp
  | BoolExpSrc of bool_exp
  | CmdSrc of cmd
  | MemorySrc of int env
  | ResSrc of trans_result
  | JdgmtSrc of judgment
  | ConcrSynSrc of string
  | ErrSrc of string

let rec string_of_source src =
  match src
  with ExpSrc exp -> string_of_exp exp
  | BoolExpSrc bool_exp -> string_of_bool_exp bool_exp
  | CmdSrc cmd -> string_of_cmd cmd
  | MemorySrc memory -> string_of_memory memory
  | ResSrc r -> string_of_trans_result r
  | JdgmtSrc j -> string_of_judgment j
  | ConcrSynSrc string -> string
  | ErrSrc string -> "Error: "^string


let process kind processor input =
    try
      (*(print_string ("parsing: " ^ input ^ "\n"));*)
      if input = "" then ParseEmpty else
      ParseOk (processor token (Lexing.from_string input))
    with Parsing.Parse_error ->
      (* prerr_endline ("Syntax error in " ^ kind ^ ": " ^ input); *)
      SyntaxError input

let process_judgment (node: unprocessed_node): judgment =
  let jm = process "term_mem_pair" term_mem_pair node.str_left
  in let r = process "trans_result"  trans_result node.str_middle
  in Judgment(jm, r)