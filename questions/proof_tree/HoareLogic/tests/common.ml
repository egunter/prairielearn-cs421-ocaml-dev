(* File: common.ml *)

(* expressions for hoarelogic *)
type mon_op = IntNegOp 

let string_of_mon_op m =
    match m with IntNegOp -> "~"

type bin_op = IntPlusOp | IntMinusOp | IntTimesOp | IntDivOp
           | ModOp

let string_of_bin_op = function 
     IntPlusOp  -> " + "
   | IntMinusOp -> " - "
   | IntTimesOp -> " * "
   | IntDivOp -> " / "
   | ModOp -> " mod "



(* Condition - Pre & Post *)
type exp = 
   | IntConst of int 
   | Ident of string 
   | MonOpAppExp of mon_op * exp
   | BinOpAppExp of bin_op * exp * exp

type booleanexp = 
   | BoolConst of bool
   | AndExp of booleanexp * booleanexp
   | OrExp of booleanexp * booleanexp
   | NotExp of booleanexp
   | LessExp of exp * exp
   | GreaterExp of exp * exp
   | LessEqExp of exp * exp
   | GreaterEqExp of exp * exp
   | EqExp of exp * exp
   | ImpExp of booleanexp * booleanexp



let rec string_of_exp stmt = 
  match stmt with
  | IntConst i -> string_of_int i
  | Ident i -> i
  | MonOpAppExp(m,e) -> (string_of_mon_op m) ^ (paren_string_of_exp e)
  | BinOpAppExp(b,e1,e2) -> ((paren_string_of_exp e1) ^ " " ^ (string_of_bin_op b) ^ " " ^ (paren_string_of_exp e2))
and paren_string_of_exp e = 
  match e with
    | IntConst i -> string_of_int i
    | Ident s -> s
  | _ -> "(" ^ string_of_exp e ^ ")"

let rec string_of_bexp bexp= 
  match bexp with
  | BoolConst b -> (if b then "true" else "false")
  | AndExp(b1,b2) -> (paren_string_of_bexp b1) ^ " & " ^ (paren_string_of_bexp b2)
  | OrExp(b1, b2) -> (paren_string_of_bexp b1) ^ " or " ^ (paren_string_of_bexp b2)
  | NotExp b -> "not " ^ (paren_string_of_bexp b)
  | LessExp(e1,e2) -> (string_of_exp e1) ^ " < " ^ (string_of_exp e2)
  | GreaterExp(e1,e2) -> (string_of_exp e1) ^ " > " ^ (string_of_exp e2)
  | LessEqExp(e1,e2) -> (string_of_exp e1) ^ " <= " ^ (string_of_exp e2)
  | GreaterEqExp(e1,e2) -> (string_of_exp e1) ^ " >= " ^ (string_of_exp e2)
  | EqExp(e1,e2) -> (string_of_exp e1) ^ " = " ^ (string_of_exp e2)
  | ImpExp(b1,b2) -> (string_of_bexp b1) ^ " --> " ^ (string_of_bexp b2)
and paren_string_of_bexp bbexp = 
  match bbexp with
  | BoolConst b -> (if b then "true" else "false")
  | _ -> "(" ^ string_of_bexp bbexp ^ ")"

(*
let string_of_cond exp = 
  match exp with
  | Bool_exp b -> string_of_bexp b
  | Exp_exp e -> string_of_exp e
*)

(* commmand *)
type command = 
   (*| SkipCommand*)
   | SeqCommand of command * command 
   | AssignCommand of string * exp
   | IfCommand of booleanexp * command * command
   | WhileCommand of booleanexp * command

let rec string_of_com command = 
  match command with (*
  | SkipCommand -> "skip" *)
  | SeqCommand(c1,c2) -> (string_of_com c1) ^ "; " ^ (string_of_com c2)
  | AssignCommand(s,e) -> s ^ ":=" ^ (string_of_exp e)
  | IfCommand(b,c1,c2) -> "if " ^ (string_of_bexp b) ^ " then " ^ (string_of_com c1) ^ " else " ^ (string_of_com c2) ^ " fi"
  | WhileCommand(b,c) -> "while " ^ (string_of_bexp b) ^ " do " ^ (string_of_com c) ^ " od"

(********************* utils *************************)
(*
type 'a parsed =
  | ParseOk of 'a
  | ParseEmpty
  | SyntaxError of string

let string_of_parsed to_str obj =
  match obj with
  | ParseEmpty -> "<<no parse: empty field>>"
  | SyntaxError s -> "Syntac Error: " ^ s
  | ParseOk thing -> to_str thing


(*type 'a cond = (string * 'a) list*)

(* lookup TermElem *)
type 'a cond = (string * 'a) list

let rec lookup mapping x =
  match mapping with
     []        -> None
   | (y,z)::ys -> if x = y then Some z else lookup ys x

let lookup_cond (condition: 'a cond) x = lookup condition x
*)

(***************** Proof tree node stuff *************)


type label = Assign | Sequence | If | While | Pre_streng | Post_weak | NoLabel

let label_of_string = function
  | "Assign" -> Assign
  | "Sequence" -> Sequence
  | "If" -> If
  | "While" -> While
  | "Pre_streng" -> Pre_streng
  | "Post_weak" -> Post_weak
  | _ -> NoLabel

let string_of_label = function
  | Assign -> "Assign"
  | Sequence -> "Sequence"
  | If -> "If"
  | While -> "While"
  | Pre_streng -> "Pre_streng"
  | Post_weak -> "Post_weak"
  | NoLabel -> ""
(*
type node =
  { label  : label;
    left   : booleanexp parsed;
    middle : command parsed;
    right  : booleanexp parsed;
    sideCondition : booleanexp parsed}



let string_of_node node =
  "{str_label = \""        ^ string_of_label node.label ^ "\"; " ^
  "str_left = \""          ^ string_of_parsed string_of_bexp node.left ^ "\"; " ^
  "str_middle = \""        ^ string_of_parsed string_of_com node.middle ^ "\"; " ^
  "str_right = \""         ^ string_of_parsed string_of_bexp node.right ^ "\"; " ^
  "str_sideCondition = \"" ^ string_of_parsed string_of_bexp node.sideCondition ^ "\"}"
*)
