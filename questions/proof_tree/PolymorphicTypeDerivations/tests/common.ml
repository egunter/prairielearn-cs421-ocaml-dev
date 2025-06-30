(* File: common.ml *)
(* Author: Elsa L. Gunter *)
(* Copyright 2017 *)
(* Share and Enjoy *)

open Genutils

type const =
     BoolConst of bool | IntConst of int | FloatConst of float
   | StringConst of string  | NilConst | UnitConst

let string_of_const = function
   BoolConst b     -> (if b then "true" else "false")
 | IntConst i      -> string_of_int i
 | FloatConst f     -> ((string_of_float f)^(if ceil f = floor f then ("0") else ("")))
 | StringConst s   -> ("\""^ (String.escaped s)^ "\"")
 | NilConst        -> "[]"
 | UnitConst       -> "()"

type bin_op = IntPlusOp | IntMinusOp | IntTimesOp | IntDivOp
           | FloatPlusOp | FloatMinusOp | FloatTimesOp | FloatDivOp 
           | ConcatOp | ConsOp | CommaOp | EqOp | GreaterOp 
           | ModOp | ExpoOp

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

type dec =
     Anon of exp
   | Let of string * exp                 (* let x = exp *)
   | LetRec of string * string * exp     (* let rec f x = exp *)

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
    | _ -> ((paren_string_of_exp e1) ^ " " ^ (string_of_bin_op b)
            ^ " " ^ (paren_string_of_exp e2)))
 | AppExp(e1,e2) -> (non_app_paren_string_of_exp e1) ^ " " ^ (paren_string_of_exp e2) 
 | FunExp (x,e) ->  ("fun " ^ x ^ " -> " ^ (string_of_exp e))
 | LetInExp (x,e1,e2) -> ("let "^x^" = "^ (string_of_exp e1) ^ " in " ^ (string_of_exp e2))
 | LetRecInExp (f,x,e1,e2) -> 
    ("let rec "^f^" "^x^" = "^(string_of_exp e1) ^ " in " ^ (string_of_exp e2))
 | RaiseExp e -> "raise " ^ (string_of_exp e)
 | TryWithExp (e,intopt1,exp1,match_list) ->
    "try " ^ (paren_string_of_exp e) ^  " with " ^
     (string_of_exc_match (intopt1,exp1)) ^
     (List.fold_left (fun s m -> (s^" | " ^ (string_of_exc_match m))) "" match_list) 

and paren_string_of_exp e =
    match e with VarExp _ | ConstExp _ -> string_of_exp e
    | _ -> "(" ^ string_of_exp e ^ ")"

and non_app_paren_string_of_exp e =
    match e with AppExp (_,_) -> string_of_exp e
    | _ -> paren_string_of_exp e
 

and string_of_exc_match (int_opt, e) =
    (match int_opt with None -> "_" | Some n -> string_of_int n) ^
    " -> " ^
    (string_of_exp e)
              
let string_of_dec = function
 | Anon e -> ("let _ = "^ (string_of_exp e))
 | Let (s, e) ->  ("let "^ s ^" = " ^ (string_of_exp e))
 | LetRec (fname,argname,fn) -> 
    ("let rec " ^ fname ^ " " ^ argname ^ " = " ^ (string_of_exp fn))

let print_exp exp = print_string (string_of_exp exp) 
let print_dec dec = print_string (string_of_dec dec)


(*type system*) (* Really ? *)

type typeVar = string

let rec expand n (list,len) =
    let q = n / 26 in
        if q = 0 then (n :: list, len + 1)
        else expand q (((n mod 26)::list), len + 1);;

let string_of_typeVar s = "'" ^ s;;


type monoTy = TyVar of typeVar | TyConst of (string * monoTy list)

let rec string_of_monoTy t =
  let rec string_of_tylist = function
     []     -> ""
   | t'::[] -> string_of_monoTy t'
   | t'::ts -> string_of_monoTy t'^ ","^ string_of_tylist ts
  in
  let string_of_subty s =
  match s with 
     TyConst ("*", _) | TyConst ("->", _) -> ("("^ string_of_monoTy s^ ")")
   | _ ->  string_of_monoTy s
  in 
    match t with
       TyVar n         -> (string_of_typeVar n)
     |TyConst (name, []) -> name
     |TyConst (name, [ty]) -> (string_of_subty ty^ " "^ name)
     |TyConst ("*", [ty1; ty2]) -> (string_of_subty ty1^ " * "^ string_of_subty ty2)
     |TyConst ("->", [ty1; ty2]) -> (string_of_subty ty1^ " -> "^ string_of_monoTy ty2)
     |TyConst (name, tys) -> ("("^ string_of_tylist tys^ ") "^ name)

let rec accummulate_freeVarsMonoTy fvs ty =
    match ty
    with TyVar n -> n::fvs
       | TyConst (c, tyl) -> List.fold_left accummulate_freeVarsMonoTy fvs tyl

let freeVarsMonoTy ty = delete_duplicates (accummulate_freeVarsMonoTy [] ty)


let bool_ty = TyConst("bool",[])
let int_ty = TyConst ("int", [])
let float_ty = TyConst ("float",[])
let string_ty = TyConst ("string",[])
let unit_ty = TyConst("unit", [])
let mk_pair_ty ty1 ty2 = TyConst("*",[ty1;ty2])
let mk_fun_ty ty1 ty2 = TyConst("->",[ty1;ty2])
let mk_list_ty ty = TyConst("list",[ty])

type polyTy = typeVar list * monoTy  (* the list is for quantified variables *)

let string_of_polyTy (bndVars, t) = match bndVars with [] -> string_of_monoTy t
    | _ ->  (List.fold_left
             (fun s v -> s ^ " " ^ string_of_typeVar v)
             "ALL"
             bndVars)
             ^ ". " ^ string_of_monoTy t

type type_env = polyTy env

let string_of_type_env gamma = string_of_env " : " string_of_polyTy gamma

let freeVarsPolyTy ((tvs, ty):polyTy) = delete_duplicates(
    List.filter (fun x -> not(List.mem x tvs)) (freeVarsMonoTy ty))

let polyTy_of_monoTy mty = (([],mty):polyTy)

let int_op_ty = polyTy_of_monoTy(mk_fun_ty int_ty (mk_fun_ty int_ty int_ty))
let float_op_ty =
    polyTy_of_monoTy(mk_fun_ty float_ty (mk_fun_ty float_ty float_ty))
let string_op_ty =
    polyTy_of_monoTy(mk_fun_ty string_ty (mk_fun_ty string_ty string_ty))

(* fixed signatures *)
let const_signature const = match const with
   BoolConst b -> (([], bool_ty):polyTy)
 | IntConst n -> ([], int_ty)
 | FloatConst f -> ([], float_ty)
 | StringConst s -> ([], string_ty)
 | NilConst -> (["a"],mk_list_ty (TyVar "a"))
 | UnitConst -> ([], unit_ty)

let binop_signature binop = match binop with
     IntPlusOp   -> int_op_ty
   | IntMinusOp   -> int_op_ty
   | IntTimesOp   -> int_op_ty
   | IntDivOp   -> int_op_ty
   | ModOp      -> int_op_ty
   | ExpoOp     -> float_op_ty
   | FloatPlusOp   -> float_op_ty
   | FloatMinusOp   -> float_op_ty
   | FloatTimesOp   -> float_op_ty
   | FloatDivOp   -> float_op_ty
   | ConcatOp -> string_op_ty
   | ConsOp -> 
       let alpha = TyVar "a"
       in (["a"], 
              mk_fun_ty alpha (mk_fun_ty (mk_list_ty alpha) (mk_list_ty alpha)))
   | CommaOp ->
       let alpha = TyVar "a" in
       let beta = TyVar "b" in
           (["a";"b"],
            mk_fun_ty alpha (mk_fun_ty beta (mk_pair_ty alpha beta)))
   | EqOp -> 
     let alpha = TyVar "a" in (["a"],mk_fun_ty alpha (mk_fun_ty alpha bool_ty))
   | GreaterOp ->
     let alpha = TyVar "a" in (["a"],mk_fun_ty alpha (mk_fun_ty alpha bool_ty))

let monop_signature monop = match monop with
    | HdOp -> let alpha = TyVar "a" in(["a"], mk_fun_ty (mk_list_ty alpha) alpha)
    | TlOp -> let alpha = TyVar "a" in
                  (["a"], mk_fun_ty (mk_list_ty alpha) (mk_list_ty alpha))
    | PrintOp -> ([], mk_fun_ty string_ty unit_ty)
    | IntNegOp -> ([], mk_fun_ty int_ty int_ty)
    | FstOp -> let t1,t2 = TyVar "a",TyVar "b"
             in (["a";"b"],mk_fun_ty (mk_pair_ty t1 t2) t1)
    | SndOp -> let t1,t2 = TyVar "a",TyVar "b"
             in (["a";"b"],mk_fun_ty (mk_pair_ty t1 t2) t2)

let freeVarsEnv l = delete_duplicates (
    List.fold_right (fun (_,pty) fvs -> freeVarsPolyTy pty @ fvs) l [])


type substitution = (typeVar * monoTy) list

let string_of_substitution s = 
  string_of_assoc_list
    "{" "}" " -> " ", " string_of_typeVar string_of_monoTy s

let subst_fun (s:substitution) n = (try List.assoc n s with _ -> TyVar n)

(* Problem 3 *)
let rec monoTy_lift_subst (s:substitution) ty =
  match ty with
    TyVar m -> subst_fun s m
  | TyConst(st, typelst) ->  TyConst(st, List.map (fun t -> monoTy_lift_subst s t) typelst);;

type memory = (string * value) list
and value =
    UnitVal                                       | BoolVal of bool
  | IntVal of int                                 | FloatVal of float
  | StringVal of string                           | PairVal of value * value
  | Closure of string * exp * memory              | ListVal of value list
  | RecVarVal of string * string * exp * memory   | Exn of int

(*value output*)
let rec string_of_value v =
   match v with
    UnitVal           -> "()"
  | IntVal n          -> if n < 0 then ("~"^ string_of_int (abs n)) else string_of_int n 
  | FloatVal r        -> string_of_float r
  | BoolVal true      -> "true"
  | BoolVal false     -> "false"
  | StringVal s       -> ("\"" ^  (String.escaped s) ^ "\"")
  | PairVal (v1,v2)   -> "("^
                         string_of_value v1^ ", "^
                         string_of_value v2^
                         ")"
  | ListVal l         -> string_of_list "[" "]" "; " string_of_value l
  | Closure (x, e, m) -> ("<some closure>")
  | RecVarVal (f, x, e, m)  -> ("<some recvar>")
  | Exn n -> ("(Exn "^ string_of_int n^ ")")

let compact_memory m = delete_duplicates m
  (*
  let rec comp m rev_comp_m =
      (match m with [] -> List.rev rev_comp_m
        | (x,y) :: m' ->
           if List.exists (fun (x',_) -> x = x') rev_comp_m
              then comp m' rev_comp_m
           else comp m' ((x,y)::rev_comp_m))
  in comp m []
  *)

(*memory output*)
let string_of_memory m =
  let cm = delete_duplicates m in
  string_of_assoc_list "" "\n" " = " "\n" (fun x -> "val "^x) string_of_value cm


(* Proof tree node stuff *) (*

type annotated_term =
  Annotated of exp * monoTy

type unprocessed_node =
  { str_label  : string;
    str_left   : string;
    str_middle : string;
    str_right  : string;
    str_sideCondition : string }

type unprocessed = (string * unprocessed_node) list

type label = Const | Var | MonOp | BinOp | If | App | Fun | Let | LetRec | NoLabel

let label_of_string = function
    "Const" -> Const
  | "Var" -> Var
  | "MonOp" -> MonOp
  | "BinOp" -> BinOp
  | "If" -> If
  | "App" -> App
  | "Fun" -> Fun
  | "Let" -> Let
  | "LetRec" -> LetRec
  | _ -> NoLabel

let string_of_label = function
    Const   -> "Const"
  | Var     -> "Var"
  | MonOp   -> "MonOp"
  | BinOp   -> "BinOp"
  | If      -> "If"
  | App     -> "App"
  | Fun     -> "Fun"
  | Let     -> "Let"
  | LetRec  -> "LetRec"
  | NoLabel -> ""

type node =
  { label  : label;
    left   : (polyTy env) parsed;
    middle : exp parsed;
    right  : monoTy parsed;
    sideCondition : (monoTy env) parsed }

(*
let string_of_sideCondition sc =
  (* {'a -> (a -> b), 'b -> a}*)
  let rec string_of_env_aux gamma =
    match gamma with
      []        -> ""
    | (x,y)::xs -> "'"^x^ " -> "^ string_of_monoTy y^
                   match xs with
                     [] -> ""
                   | _  -> ", "^ string_of_env_aux xs
  in
  "{"^ string_of_env_aux sc^ "}"
*)
let string_of_sideCondition sc = string_of_substitution sc

let string_of_node node =
  "{str_label = \""        ^ string_of_label node.label ^ "\"; " ^
  "str_left = \""          ^ string_of_parsed string_of_type_env node.left ^ "\"; " ^
  "str_middle = \""        ^ string_of_parsed string_of_exp node.middle ^ "\"; " ^
  "str_right = \""         ^ string_of_parsed string_of_monoTy node.right ^ "\"; " ^
  "str_sideCondition = \"" ^ string_of_parsed string_of_sideCondition node.sideCondition ^ "\"}"
*)
type judgment =
   ExpJudgment of type_env * exp * monoTy
 | DecJudgment of type_env * dec * type_env

let string_of_judgment judgment =
  match judgment with ExpJudgment(gamma, exp, monoTy) ->
        string_of_type_env gamma ^ " |- "^ string_of_exp exp ^
         " : " ^ string_of_monoTy monoTy
  | DecJudgment (gamma, dec, delta) ->
        string_of_type_env gamma ^ " |- "^ string_of_dec dec ^
         " : " ^ string_of_type_env delta

