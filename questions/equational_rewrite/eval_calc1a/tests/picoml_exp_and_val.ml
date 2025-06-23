(* 
File: picoml_exp_and_val.ml
Author: Elsa L. Gunter
Share and Enjoy
*)

open Gen_env

(* expressions for PicoML *)
type const = TrueConst | FalseConst | IntConst of int | FloatConst of float
           | StringConst of string | NilConst | UnitConst

let string_of_const c =
    match c 
    with IntConst n    -> if n < 0 then "(~"^string_of_int(abs n) else string_of_int n
       | TrueConst     -> "true"
       | FalseConst    -> "false"
       | FloatConst f  -> string_of_float f
       | StringConst s -> "\"" ^ s ^ "\""
       | NilConst      -> "[]"
       | UnitConst     -> "()"

type basic_value =
    UnitVal
  | TrueVal | FalseVal
  | IntVal of int
  | FloatVal of float
  | StringVal of string

let string_of_basic_value v =
   match v with
    UnitVal           -> "()"
  | IntVal n          -> if n < 0 then ("~"^ string_of_int (abs n))
                         else string_of_int n 
  | FloatVal r        -> string_of_float r
  | TrueVal	      ->  "true"
  | FalseVal	      ->  "false"
  | StringVal s       ->  "\"" ^ String.escaped s ^ "\""

let const_of_basic_value bv =
  match bv
  with UnitVal -> UnitConst
    | TrueVal -> TrueConst
    | FalseVal -> FalseConst
    | IntVal n -> IntConst n
    | FloatVal r -> FloatConst r
    | StringVal s -> StringConst s

type bin_op = IntPlusOp | IntMinusOp | IntTimesOp | IntDivOp
           | FloatPlusOp | FloatMinusOp | FloatTimesOp | FloatDivOp 
           | ConcatOp | ConsOp | CommaOp | EqOp | NeqOp
           | GreaterOp | GreaterEqOp
           | ModOp | ExpoOp

(*
let gt is_html = if is_html then "&gt;" else ">"
let lt is_html = if is_html then "&lt;" else "<"
*)

let string_of_bin_op = function 
     IntPlusOp  -> " + "
   | IntMinusOp -> " - "
   | IntTimesOp -> " * "
   | IntDivOp -> " / "
   | FloatPlusOp -> " +. "
   | FloatMinusOp -> " -. "
   | FloatTimesOp -> " *. "
   | FloatDivOp -> " /. "
   | ConcatOp -> " ^ "
   | ConsOp -> " :: "
   | CommaOp -> " , "
   | EqOp  -> " = "
   | NeqOp -> " <> "
   | GreaterOp -> " > "
   | ExpoOp -> "**"
   | ModOp   -> "mod"

type mon_op = HdOp | TlOp | PrintOp | IntNegOp | FstOp | SndOp

let string_of_mon_op m =
    match m
    with HdOp  -> "hd"
       | TlOp  -> "tl"
       | PrintOp  -> "print_string"
       | IntNegOp   -> "~"
       | FstOp   -> "fst"
       | SndOp   -> "snd"

type exp =  (* Exceptions will be added in later MPs *)
  | VarExp of string                    (* variables *)
  | ConstExp of const                   (* constants *)
  | MonOpAppExp of mon_op * exp         (* % e1 for % is a builtin monadic operator *) 
  | BinOpAppExp of bin_op * exp * exp   (* e1 % e2 for % is a builtin binary operator *)
  | IfExp of exp * exp * exp            (* if e1 then e2 else e3 *)
  | AppExp of exp * exp                 (* e1 e2 *) 
  | FunExp of string * exp              (* fun x -> e1 *)
  | LetInExp of string * exp * exp      (* let x = e1 in e2 *)
  | LetRecInExp of string * string * exp * exp (* let rec f x = e1 in e2 *)
  | RaiseExp of exp                            (* raise e *)
  | TryWithExp of (exp * int option * exp * (int option * exp) list)
   (* try e with i -> e1 | j -> e1 | ... | k -> en *)
   (* And two new constructs *)
  | Val of value 
  | Eval of exp * memory

and value = 
    BasicVal of basic_value
  | PairVal of value * value
  | ListVal of value list
  | Closure of string * exp * memory
  | RecVarVal of string * string * exp * memory 

and memory = value env

let rec string_of_exp = function
   VarExp s -> s
 | ConstExp c ->  string_of_const c
 | IfExp(e1,e2,e3)->"if " ^ (string_of_exp e1) ^
                 " then " ^ (string_of_exp e2) ^
                 " else " ^ (string_of_exp e3)
 | MonOpAppExp (m,e) ->  (string_of_mon_op m) ^ " " ^ (paren_string_of_exp e) 
 | BinOpAppExp (b,e1,e2) -> 
   (match b with CommaOp -> ("(" ^ (paren_string_of_exp e1) ^ (string_of_bin_op b) ^
                              (paren_string_of_exp e2) ^ ")")
    | _ -> ((paren_string_of_exp e1) ^ (string_of_bin_op b)
            ^ (paren_string_of_exp e2)))
 | AppExp(e1,e2) -> (non_app_paren_string_of_exp e1) ^ " " ^ (paren_string_of_exp e2) 
 | FunExp (x,e) ->  ("fun " ^ x ^ "->" ^" " ^ (string_of_exp e))
 | LetInExp (x,e1,e2) -> ("let "^x^" = "^ (string_of_exp e1) ^ " in " ^ (string_of_exp e2))
 | LetRecInExp (f,x,e1,e2) -> 
    ("let rec "^f^" "^x^" = "^(string_of_exp e1) ^ " in " ^ (string_of_exp e2))
 | RaiseExp e -> "raise " ^ (string_of_exp e)
 | TryWithExp (e,intopt1,exp1,match_list) ->
    "try " ^ (paren_string_of_exp e) ^  " with " ^
     (string_of_exc_match (intopt1,exp1)) ^
     (List.fold_left (fun s m -> (s^" | " ^ (string_of_exc_match m))) "" match_list) 
 | Val v -> "Val " ^ string_of_value (*string_of_memory*) v 
 | Eval (exp, mem) -> "Eval ("^(string_of_exp exp)^", "^(string_of_memory mem)^")"


and paren_string_of_exp e =
    match e with VarExp _ | ConstExp _ -> string_of_exp e
    | _ -> "(" ^ string_of_exp e ^ ")"

and non_app_paren_string_of_exp e =
    match e with AppExp (_,_) -> string_of_exp e
    | _ -> paren_string_of_exp e
 

and string_of_exc_match (int_opt, e) =
    (match int_opt with None -> "_" | Some n -> string_of_int n) ^ " -> " ^
    (string_of_exp e)

and string_of_value (*string_of_memory*) v =
  match v
  with BasicVal bv    -> string_of_basic_value bv
  | PairVal (v1,v2)   ->  "("^
                         string_of_value (*string_of_memory*) v1^  ", "^
                         string_of_value (*string_of_memory*) v2^
                          ")"
  | ListVal l         ->  "["^
                         (let rec pl = function
                              []     ->  "]"
                            | v::vl  -> string_of_value (*string_of_memory*) v^
                                        (match vl with [] -> ""
                                          | _ -> "; " ^ pl vl)
                              in pl l)
  | Closure (x, e, m) ->
    "<"^x^" -> "^(string_of_exp e) ^ ", " ^ (string_of_memory m) ^ ">"
  | RecVarVal (f, x, e, m)  ->
    "<<"^x^" -> "^(string_of_exp e) ^ ", " ^ (string_of_memory m) ^ ">>"

and string_of_memory m =
    string_of_env (string_of_value (*string_of_memory*)) "->" "," m

(*value output*)
let rec short_string_of_value v =
  match v
  with BasicVal bv    -> string_of_basic_value bv
  | PairVal (v1,v2)   ->  "("^
                         short_string_of_value v1^  ", "^
                         short_string_of_value v2^
                          ")"
  | ListVal l         ->  "["^
                         (let rec pl = function
                              []     ->  "]"
                            | v::vl  -> short_string_of_value v^
                                        (match vl with [] -> "]"
                                          | _ -> "; " ^ pl vl)
                              in pl l)
  | Closure (x, e, m) -> "<closure>"
  | RecVarVal (f, x, e, m)  -> "<<recursive closure>>"

let rec canonicalize_exp exp =
  match exp
  with  VarExp s -> exp
 | ConstExp c ->  exp
 | MonOpAppExp (mop,e) ->  MonOpAppExp (mop, canonicalize_exp e)
 | BinOpAppExp (b,e1,e2) ->  BinOpAppExp (b, canonicalize_exp e1, canonicalize_exp e2)
 | IfExp(e1,e2,e3)-> IfExp(canonicalize_exp e1,canonicalize_exp e2,canonicalize_exp e3)
 | AppExp(e1,e2) -> AppExp(canonicalize_exp e1, canonicalize_exp e2) 
 | FunExp (x,e) ->  FunExp(x, canonicalize_exp e)
 | LetInExp (x,e1,e2) -> LetInExp (x, canonicalize_exp e1, canonicalize_exp e2)
 | LetRecInExp (f,x,e1,e2) -> LetRecInExp (f,x,canonicalize_exp e1, canonicalize_exp e2)
 | RaiseExp e -> RaiseExp (canonicalize_exp e)
 | TryWithExp (e,intopt1,exp1,match_list) -> failwith "We're not doing this right now"
 | Val v -> Val (canonicalize_value v )
 | Eval (e, mem) -> Eval(canonicalize_exp e,canonicalize_memory mem)

and canonicalize_memory m =
    match m with [] -> []
      | (x, v)::rem_mem ->
        ins_env (canonicalize_memory rem_mem) x (canonicalize_value v)

and canonicalize_value v =
  match v with BasicVal bv -> v
    | PairVal (v1, v2) -> PairVal (canonicalize_value v1, canonicalize_value v2)
    | ListVal vl -> ListVal (List.map canonicalize_value vl)
    | Closure (x, e, m) -> Closure (x, canonicalize_exp e, canonicalize_memory m)
    | RecVarVal (f, x, e, m) ->
      RecVarVal (f, x, canonicalize_exp e, canonicalize_memory m)

type dec =
     Anon of exp
   | Let of string * exp                 (* let x = exp *)
   | LetRec of string * string * exp     (* let rec f x = exp *)

let string_of_dec = function
 | Anon e -> ("let _ = "^ (string_of_exp e))
 | Let (s, e) ->  ("let "^ s ^" = " ^ (string_of_exp e))
 | LetRec (fname,argname,fn) -> 
    ("let rec " ^ fname ^ " " ^ argname ^ " = " ^ (string_of_exp fn))

let print_exp exp = print_string (string_of_exp exp) 
let print_dec dec = print_string (string_of_dec dec)
