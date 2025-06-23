(* File: genutils.ml *)
(* Author: Elsa L. Gunter*)
(* Copyright 2017 *)
(* Share and Enjoy *)

open Genutils
open Common
open Lex
open Parse

type h_triple =
    HTriple of booleanexp parsed * command parsed * booleanexp parsed

let string_of_h_triple ht =
  match ht with
    HTriple (p, r, q) ->
    "{" ^ (string_of_parsed string_of_bexp p) ^ "} " ^ (string_of_parsed string_of_com r)
    ^ " {" ^ (string_of_parsed string_of_bexp q) ^ "}"

type source =
    ExpSrc of exp
  | BoolExpSrc of booleanexp
  | CmdSrc of command
  | HTSrc of h_triple
  | ConcrSynSrc of string
  | ErrSrc of string

let rec string_of_source src =
  match src
  with ExpSrc exp -> string_of_exp exp
  | BoolExpSrc bool_exp -> string_of_bexp bool_exp
  | CmdSrc cmd -> string_of_com cmd
  | HTSrc ht -> string_of_h_triple ht
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

let process_h_triple (node: unprocessed_node): h_triple =
  let p = process "bool_exp" bool_exp node.str_left
  in let r = process "program" program node.str_middle
  in let q = process "bool_exp" bool_exp node.str_right
  in HTriple(p, r, q)

let process_h_sc sc_str = process "bool_exp" bool_exp sc_str