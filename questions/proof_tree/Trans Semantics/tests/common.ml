(* File: common.ml *)
(* Author: Elsa L. Gunter and Daniil Plyukhin *)
(* Copyright 2017 *)
(* Share and Enjoy *)

open Genutils

(* expressions for IMP *)

type bin_op = PlusOp | MinusOp | TimesOp
let string_of_bin_op b =
  match b with PlusOp -> " + " | MinusOp -> " - " | TimesOp -> " * "
    
type exp =
| Ident of string
| Const of int
| NegOp of exp
| BinOp of bin_op * exp * exp

let rec string_of_exp exp =
  match exp with
    | Ident string -> string
    | Const int -> string_of_int int
    | NegOp exp -> "-"^(paren_string_of_exp exp)
    | BinOp (bin_op, exp1, exp2) ->
      ((paren_string_of_exp exp1) ^ (string_of_bin_op bin_op)
       ^ (paren_string_of_exp exp2))

and paren_string_of_exp exp =
  match exp with Ident _ | Const _ -> string_of_exp exp
    | _ -> "(" ^ string_of_exp exp ^ ")"

type compare_op = LtOp | EqOp

let string_of_compare_op cop =
  match cop with LtOp -> " < " | EqOp -> " = "

type bool_bin_op = AndOp | OrOp (*| IffOp | ImpOp (* For Hoare Logic *)*)

let string_of_bool_bin_op bop =
  match bop with AndOp -> " & " | OrOp -> " or " (*| IffOp -> " = " (* | ImpOp -> "==>" *)*)

type bool_exp =
| True
| False
| NotOp of bool_exp
| BoolBinOp of bool_bin_op * bool_exp * bool_exp
| Compare of compare_op * exp * exp

let rec string_of_bool_exp bexp =
  match bexp with
    | True -> "true"
    | False -> "false"
    | NotOp bool_exp -> "not "^(paren_string_of_bool_exp bool_exp)
    | BoolBinOp (bop, bool_exp1, bool_exp2) ->
      (paren_string_of_bool_exp bool_exp1)^(string_of_bool_bin_op bop)^
        (paren_string_of_bool_exp bool_exp2)
    | Compare (cop, exp1, exp2) ->
      ((paren_string_of_exp exp1) ^ (string_of_compare_op cop)
       ^ (paren_string_of_exp exp2))
and paren_string_of_bool_exp bexp =
  match bexp with True -> "true" | False -> "false"
    | _ -> "("^(string_of_bool_exp bexp)^")"

type cmd =
   | Skip
   | Assign of string * exp           (* ident := e1 *)
   | Seq of cmd * cmd                 (* cmds ; cmds *)
   | If of bool_exp * cmd * cmd       (* if b then cmds else cmds fi *)
   | While of bool_exp * cmd          (* while b do cmds od *)

let rec string_of_cmd cmd =
  match cmd with
   | Skip -> "skip"
   | Assign (string, exp) -> string^" := "^(string_of_exp exp)
   | Seq (cmd1, cmd2) -> (string_of_cmd cmd1)^";\n "^(string_of_cmd cmd2)
   | If (bool_exp, cmd1, cmd2) ->
     "if "^(string_of_bool_exp bool_exp)^"\nthen "^(string_of_cmd cmd1)
     ^"else "^(string_of_cmd cmd2)^" fi"
   | While (bool_exp, cmd) ->
     "while "^(string_of_bool_exp bool_exp)^" do\n"^(string_of_cmd cmd)^"\nod"

type memory = int env
let string_of_memory memory = string_of_env " -> " string_of_int memory

(* In Genutils

(* Util functions *)
let rec drop y = function
   []    -> []
 | x::xs -> if x=y then drop y xs else x::drop y xs

let rec delete_duplicates = function
   []    -> []
 | x::xs -> x::delete_duplicates (drop x xs)

(* environments *)
type 'a env = (string * 'a) list

let string_of_env string_of_entry gamma = 
  let rec string_of_env_aux gamma =
    match gamma with
       []        -> ""
     | (x,y)::xs -> x^ " : "^ string_of_entry y^
                    match xs with [] -> "" | _  -> ", "^
                                                   string_of_env_aux xs
  in
    "{"^ string_of_env_aux gamma^ "}"

(*environment operations*)
let rec lookup mapping x =
  match mapping with
     []        -> None
   | (y,z)::ys -> if x = y then Some z else lookup ys x

let make_env x y = ([(x,y)]:'a env)
let lookup_env (gamma:'a env) x = lookup gamma x
let sum_env (delta:'a env) (gamma:'a env) = ((delta@gamma):'a env)
let ins_env (gamma:'a env) x y = sum_env (make_env x y) gamma
*)



type label =  Var_label | Const_label | BinOp_label
              | BoolConst_label | And_label | Or_label | Not_label | Rel_label
              | Skip_label | Assign_label | Seq_label | If_label | While_label
              | NoLabel

let label_of_string = function
    "Const" -> Const_label
  | "Var" -> Var_label
  | "BinOp" -> BinOp_label
  | "BoolConst" ->BoolConst_label
  | "And" -> And_label
  | "Or" -> Or_label
  | "Not" -> Not_label
  | "Rel" ->  Rel_label
  | "Skip" -> Skip_label
  | "Assign" -> Assign_label
  | "If" -> If_label
  | "While" -> While_label
  | "Seq" -> Seq_label
  | _ -> NoLabel

                  
let string_of_label = function
    Const_label   -> "Const"
  | Var_label     -> "Var"
  | BinOp_label   -> "BinOp"
  | BoolConst_label -> "BoolConst"
  | And_label -> "And"
  | Or_label -> "Or"
  | Not_label -> "Not"
  | Rel_label -> "Rel"
  | Skip_label -> "Skip"
  | Assign_label -> "Assign"
  | If_label -> "If"
  | While_label -> "While"
  | Seq_label -> "Seq"
  | NoLabel -> ""


type thing_to_eval =
    ExpTTE of exp
  | BoolExpTTE of bool_exp
  | CmdTTE of cmd

let string_of_thing_to_eval tte =
  match tte with
    | ExpTTE exp -> string_of_exp exp
    | BoolExpTTE bool_exp -> string_of_bool_exp bool_exp
    | CmdTTE cmd -> string_of_cmd cmd

type trans_result =
    InterRes of thing_to_eval * memory
  | MemRes of memory
let string_of_trans_result r =
  match r with
    | MemRes memory -> string_of_memory memory
    | InterRes (tte, memory) -> "(" ^ string_of_thing_to_eval tte ^ ", " ^ (string_of_memory memory) ^ ")"


