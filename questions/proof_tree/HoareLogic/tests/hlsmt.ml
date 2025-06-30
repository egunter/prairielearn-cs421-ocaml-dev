open Aez
open Smt
module T = Term
module F = Formula
module Solver = Make (struct end)
open Common

(* helper functions *)
let insert a l = if List.exists (fun x -> if x = a then true else false) l then l else a::l;;

let rec insertAll l1 l2 = match l1 with [] -> l2 | x::xl -> insertAll xl (insert x l2);;

let rec collectVarsExp x = match x with IntConst a -> []
           | Ident a -> [a]
           | MonOpAppExp (a,b) -> collectVarsExp b
           | BinOpAppExp (a,b,c) -> insertAll (collectVarsExp b) (collectVarsExp c);;

let rec collectVars b = match b with BoolConst x -> []
       | AndExp (x,y) -> insertAll (collectVars x) (collectVars y)
       | OrExp (x, y) -> insertAll (collectVars x) (collectVars y)
       | NotExp x -> collectVars x
       | LessExp (x,y) -> insertAll (collectVarsExp x) (collectVarsExp y)
       | LessEqExp (x,y) -> insertAll (collectVarsExp x) (collectVarsExp y)
       | GreaterExp (x,y) -> insertAll (collectVarsExp x) (collectVarsExp y)
       | GreaterEqExp (x,y) -> insertAll (collectVarsExp x) (collectVarsExp y)
       | EqExp (x,y) -> insertAll (collectVarsExp x) (collectVarsExp y)
       | ImpExp (x,y) -> insertAll (collectVars x) (collectVars y);;

let rec lookup m x = match m with [] -> None
         | ((a,b)::xl) -> if a = x then Some b else lookup xl x;;

(* first make a table for all variables and declare all vars *)
let rec makeMapForVars l = match l with [] -> []
         | x::xl -> (x,Hstring.make x)::(makeMapForVars xl);;

let rec declareVars l = match l with [] -> ()
       | ((x,y)::xl) -> Symbol.declare y [] Type.type_int; declareVars xl;;

(* second, parse all formula to a form of alt-ergo zero *)
let rec transferMonOp b = match b with IntNegOp -> T.Minus
let rec transferBinOp b = match b with IntPlusOp -> T.Plus
       | IntMinusOp -> T.Minus | IntTimesOp -> T.Mult
       | IntDivOp -> T.Div | ModOp -> T.Modulo

let rec transferExp e m = match e with IntConst x -> T.make_int (Num.Int x)
         | Ident x -> (match lookup m x with None -> T.make_int (Num.Int 0)
                             | Some y -> T.make_app y [])
         | MonOpAppExp (x,y) -> T.make_arith (transferMonOp x) (T.make_int (Num.Int 0)) (transferExp y m)
         | BinOpAppExp (x,y,z) -> T.make_arith (transferBinOp x) (transferExp y m) (transferExp z m);;

let rec transfer b m = match b with BoolConst x -> if x then F.f_true else F.f_false
         | AndExp (x,y) -> F.make F.And [transfer x m; transfer y m]
         | OrExp (x,y) -> F.make F.Or [transfer x m; transfer y m]
         | NotExp x -> F.make F.Not [transfer x m]
         | ImpExp (x,y) -> F.make F.Imp [transfer x m; transfer y m]
         | LessExp (x,y) -> F.make_lit F.Lt [transferExp x m; transferExp y m]
         | GreaterExp (x,y) -> F.make_lit F.Lt [transferExp y m; transferExp x m]
         | LessEqExp (x,y) -> F.make_lit F.Le [transferExp x m; transferExp y m]
         | GreaterEqExp (x,y) -> F.make_lit F.Le [transferExp y m; transferExp x m]
         | EqExp (x,y) -> F.make_lit F.Eq [transferExp x m; transferExp y m];;

(* third, do the smt solving *)

let rec validCheck b = let l = collectVars b
          in let theMap = makeMapForVars l in 
            declareVars theMap; 
            (let transfered = transfer b theMap in 
                 try
                   Solver.clear ();
                   Solver.assume ~id:1 (F.make F.Not [transfered]);
                   Solver.check (); false
                 with Unsat _ -> true);;
