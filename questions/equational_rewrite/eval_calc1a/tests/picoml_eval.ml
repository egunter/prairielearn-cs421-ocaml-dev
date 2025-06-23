(* File: picoml_eval.ml *)
(* Author: Elsa L. Gunter *)
(* Share and Enjoy *)

open Utils
open Gen_env
open Picoml_exp_and_val


(* Picoml Evaluation *)

let const_to_val c = 
  match c with
    TrueConst     -> BasicVal TrueVal
  | FalseConst    -> BasicVal FalseVal
  | IntConst i    -> BasicVal (IntVal i)
  | FloatConst f  -> BasicVal (FloatVal f)
  | StringConst s -> BasicVal (StringVal s)
  | NilConst      -> ListVal []
  | UnitConst     -> BasicVal UnitVal

let monOpApply op v =
  match op, v with
    HdOp, ListVal []        -> failwith "monOpApply: hd"
  | TlOp, ListVal []        -> failwith "monOpApply: tl"
  | HdOp, ListVal (v::vs)   -> v 
  | TlOp, ListVal (v::vs)   -> ListVal vs
  | FstOp, PairVal (v1, v2) -> v1
  | SndOp, PairVal (v1, v2) -> v2
  | PrintOp, BasicVal (StringVal s)       -> (print_string s; BasicVal UnitVal)
  | IntNegOp, BasicVal (IntVal i)         -> BasicVal (IntVal(-i))
  | _ -> failwith "monOpApply: bad input"

let binOpApply binop (v1,v2) =
  try (
  match binop, v1, v2 with
    ConcatOp, BasicVal (StringVal x), BasicVal (StringVal y) -> BasicVal (StringVal (x ^ y))
  | ConsOp,_, ListVal v2_lst -> ListVal (v1::v2_lst)
  | CommaOp,_,_  -> PairVal (v1, v2) 
  | EqOp,_,_  -> (match (v1 = v2) with true -> BasicVal TrueVal | false -> BasicVal FalseVal)
  | NeqOp,_,_  -> (match (v1 <> v2) with true -> BasicVal TrueVal | false -> BasicVal FalseVal)
  | GreaterOp,_,_  -> (match (v1 > v2) with true -> BasicVal TrueVal | false -> BasicVal FalseVal)
  | GreaterEqOp,_,_  -> (match (v1 >= v2) with true -> BasicVal TrueVal | false -> BasicVal FalseVal)
  | _, BasicVal (IntVal x), BasicVal (IntVal y) -> BasicVal (IntVal(
      match binop with
        IntPlusOp -> x + y
      | IntMinusOp -> x - y
      | IntTimesOp -> x * y
      | IntDivOp -> x / y
      | ModOp -> x mod y
      | _ -> failwith "binOpApply: bad input"
    ))
  | _, BasicVal (FloatVal x), BasicVal (FloatVal y) -> BasicVal (FloatVal(
      match binop with
        FloatPlusOp -> x +. y
      | FloatMinusOp -> x -. y
      | FloatTimesOp -> x *. y
      | FloatDivOp -> if y = 0.0 then raise Division_by_zero else x /. y
      | ExpoOp -> x ** y
      | _ -> failwith "binOpApply: bad input"
    ))
  | _ -> failwith "binOpApply: bad input"
  ) with Division_by_zero -> failwith "binOpApply: divide by zero"

(* Rules *)

let rec one_step_eval (exp, env) =
  match exp
  with VarExp x ->
    (match lookup_env env x
     with None -> (print_string
                     ("The variable "^x^" is not in the environment:\n"^
                         (string_of_memory env));
                   None)
       | Some v -> Some(Val v))
      
    | ConstExp c -> Some(Val (const_to_val c))
      
    | MonOpAppExp (monop, exp1) ->
      (match exp1
       with Val v ->
         (try Some(Val(monOpApply monop v))
          with Failure s -> (print_string (s^"\n"); None)
            | _ -> None)
         | Eval(e1,env1) ->
           (match one_step_eval (e1, env1)
            with None -> None
              | Some exp1' -> Some(Eval(MonOpAppExp (monop,exp1'), env)))
         | _ -> Some(Eval(MonOpAppExp (monop,Eval(exp1,env)), env)))

    | BinOpAppExp (binop,exp1, exp2) ->
      (match exp2
       with Val v2 ->
         (match exp1
          with Val v1 -> 
            (try Some(Val (binOpApply binop (v1, v2)))
             with Failure s -> (print_string (s^"\n"); None)
               | _ -> None)
            | Eval(e1, env1) ->
              (match one_step_eval (e1, env1)
               with None -> None
                 | Some exp1' -> Some(Eval(BinOpAppExp (binop, exp1', Val v2), env)))
            | _ -> Some(Eval(BinOpAppExp (binop, Eval(exp1,env), Val v2), env)))
         | Eval(e2, env2) ->
           (match one_step_eval (e2, env2)
            with None -> None
              | Some exp2' -> Some(Eval(BinOpAppExp (binop, exp1, exp2'), env)))
         | _ -> Some(Eval(BinOpAppExp (binop, exp1, Eval (exp2,env)), env)))
        
    | IfExp (bexp, texp, eexp) ->
      (match bexp
       with Val (BasicVal TrueVal) -> Some (Eval(texp, env))
         | Val (BasicVal FalseVal) -> Some (Eval(eexp, env))
         | Val nb ->
           (print_string ("The boolguard Val("^(string_of_value nb)^
                             ") in the if-then-else expression:\n"^
                             (string_of_exp exp)^
                             "\nis required to be a boolean but it is not.\n");
            None)
         | Eval (b, benv) -> 
           (match one_step_eval (b, benv)
            with None -> None
              | Some bexp' -> Some(Eval(IfExp(bexp',texp,eexp), env)))
         | _ -> Some(Eval(IfExp(Eval(bexp,env),texp,eexp), env)))

    | AppExp (fexp, aexp) ->
      (match aexp
       with Val v ->
         (match fexp
          with Val(Closure(x,bexp,mem)) -> Some(Eval(bexp, (ins_env mem x v)))
            | Val(RecVarVal(f,x,bexp,mem)) ->
              Some(Eval(exp, (ins_env (ins_env mem f (RecVarVal(f,x,bexp,mem))) x v)))
            | Eval (fe, fenv) ->
              (match one_step_eval (fe, fenv)
               with None -> None
                 | Some fexp' -> Some(Eval(AppExp(fexp', Val v), env)))
            | Val _ -> None
            | _ -> Some (Eval(AppExp(Eval(fexp, env), Val v), env)))
         | Eval(ae, aenv) ->
           (match one_step_eval (ae, aenv)
            with None -> None
              | Some aexp' -> Some (Eval(AppExp(fexp, aexp'), env)))
         | _ -> Some (Eval(AppExp(fexp, Eval(aexp, env)), env)))

    | FunExp(x,exp1) -> Some(Val(Closure(x, exp1, env)))

    | LetInExp (x, exp1, exp2) ->
      (match exp1
       with Val v1 ->  Some(Eval(exp2, (ins_env env x v1)))
         | Eval (e1, env1) -> 
           (match one_step_eval (e1, env1)
            with None -> None
              | Some exp1' -> Some(Eval(LetInExp(x, exp1', exp2), env)))
         | _ -> Some(Eval(LetInExp(x, Eval(exp1, env),  exp2), env)))

    | LetRecInExp (f, x, exp1, exp2) ->
      Some(Eval(exp2, (ins_env env f (RecVarVal(f,x,exp1,env)))))

    | RaiseExp _ -> None (* we won't handle this *)
    | TryWithExp _ -> None (* or this *)

    | Eval (exp1, env1) ->
      (print_string "\n\nThis should not happen!\n\n";
       match exp1
       with Val v -> Some(Val v)
(*
         | Eval(e1, ev1) ->
           (match one_step_eval (e1, ev1)
            with None -> None
              | Some exp1' -> Some (Eval (exp1', env1)))
*)
         | _ -> one_step_eval (exp1, env1))
    | Val _ -> None



(* I need a data type of memories and one of closures marking where and
   how things are wrong.  I need to be able to "pretty print"
   annotated memories and closures as html strings.  I need a way to
   give incremental points.
*)
(*

let rec canonicalize_memory mem =
  Gen_env.canonicalize_env canonicalize_value mem

and canonicalize_value value =
  match value
  with Closure (var, body, memory) -> Closure(var, body, canonicalize_memory memory)
    | RecVarVal (f, x, body, memory)
      -> RecVarVal (f, x, body, canonicalize_memory memory)
    | PairVal(v1, v2) -> PairVal (canonicalize_value v1, canonicalize_value v2)
    | ListVal l -> ListVal (List.map canonicalize_value l)
    | _ -> value

type marker = Good | Bad

type annotated_memory = 
     CorrectMem of (value env)
  | MissingEntry of (string * annotated_memory)
  | ExtraEntry of (string * value list * annotated_memory)
  | DuplicateEntries of (string * value * annotated_value list * annotated_memory)
  | WrongEntryValue of (string * value * annotated_value * annotated_memory)
  | CorrectEntryValue of (string * value  * annotated_memory)
  | ContainsError of (string * value * annotated_value env)
and annotated_value =
    CorrectValue of value
  | WrongBasicValue of basic_value
  | WrongValueSort of value
  | PairContainsError of annotated_value * annotated_value
  | ListContainsError of annotated_value list
  | WrongClosure of (string*marker) * (exp*marker) * annotated_memory
  | WrongRecClosure of (string*marker) * (string*marker) * (exp*marker) * annotated_memory


let rec mem_multifun mem =
  match (canonicalize_memory mem)
  with [] -> []
    | (key1,value1)::more_mem ->
      let mem_multifn = mem_multifun more_mem
      in
      match mem_multifn
      with [] -> [(key1,[value1])]
         | (key2,values) :: rem_memfun ->
           if key1 = key2 then (key1, (value1::values))::rem_memfun
           else (key1,[value1])::mem_multifn
*)
