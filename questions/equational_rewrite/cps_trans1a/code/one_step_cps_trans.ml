(* File: one_step_cps_trans.ml *)
(* Author: Elsa L. Gunter *)
(* Share and Enjoy *)

open Picoml_exp_and_cps

let top_one_step_cps_trans exp k =
  match exp
(*[[x]]k = k x*)
  with VarExp x -> VarCPS (k, x)
(*[[c]]k = k x*)
    | ConstExp n -> ConstCPS (k, n)
(*[[~ e]]k = [[e]]_(fun r -> k (~ r)) *)
    | MonOpAppExp (m, e) ->
      let r = freshFor (freeVarsInContCPS k)
      in CPS_Trans (e, (FnContCPS (r, MonOpAppCPS (k, m, r))))
(*[[(e1 + e2)]]k = [[e2]]_ fun s -> [[e1]] _ fun r -> k (r + s)*)
    | BinOpAppExp (b, e1, e2) ->
      let v2 = freshFor (freeVarsInContCPS k @ freeVarsInExp e1)  in 
      let v1 = freshFor (v2 :: (freeVarsInContCPS k)) in 
      let e2CPS = CPS_Trans (e1, (FnContCPS (v1, BinOpAppCPS(k, b, v1, v2)))) in
      CPS_Trans (e2, (FnContCPS (v2, e2CPS)))
(*[[if e1 then e2 else e3]]k = [[e1]]_(fun r -> if r then [[e2]]k else [[e3]]k)*)
   | IfExp (e1,e2,e3) ->
      let r = freshFor (freeVarsInContCPS k @
                        freeVarsInExp e2 @ freeVarsInExp e3) in 
      let e2cps = CPS_Trans(e2, k) in
      let e3cps = CPS_Trans(e3, k) in 
      CPS_Trans(e1, (FnContCPS(r, IfCPS(r, e2cps, e3cps))))
(*[[e1 e2]]k = [[e2]]_fun v2 -> [[e1]]_fun v1 -> k (v1 v2)*)
   | AppExp (e1,e2) -> 
      let v2 = freshFor (freeVarsInContCPS k @ freeVarsInExp e1) in
      let v1 = freshFor (v2 :: freeVarsInContCPS k) in
      let e1cps = CPS_Trans(e1, (FnContCPS (v1, AppCPS(k, v1, v2)))) in
      CPS_Trans(e2, (FnContCPS (v2, e1cps)))
(*[[fun x -> e]]k = k(fnk x kx -> [[e]]kx) *)
   | FunExp (x,e) ->
      let ecps = CPS_Trans(e, (ContVarCPS Kvar)) in
      FunCPS (k, x, Kvar, ecps)
(*[[let x = e1 in e2]]k = [[e1]]_fun x -> [[e2]]k) *)
   | LetInExp (x,e1,e2) -> 
     let e2cps = CPS_Trans(e2, k) in
     let fx = FnContCPS (x, e2cps) in
     CPS_Trans(e1, fx)
(*[[let rec f x = e1 in e2]]k =
  (FN f -> [[e2]]_k))(FIX f. FUN x -> fn kx => [[e1]]kx) *)
   | LetRecInExp(f,x,e1,e2) -> 
     let e1cps = CPS_Trans(e1, (ContVarCPS Kvar)) in
     let e2cps = CPS_Trans(e2, k) in
     FixCPS(FnContCPS (f,e2cps),f,x,Kvar,e1cps)

let rec all_one_step_cps_trans exp_cps =
  match exp_cps
  with VarCPS (k, x) ->
    List.map (fun kt -> VarCPS(kt, x)) (all_one_step_cps_cont_trans k)
    | ConstCPS (k, c) -> 
      List.map (fun kt -> ConstCPS (kt,c)) (all_one_step_cps_cont_trans k)
    | MonOpAppCPS (k,m,s) ->
      List.map (fun kt -> MonOpAppCPS (kt,m,s)) (all_one_step_cps_cont_trans k)
    | BinOpAppCPS (k,b,r,s) -> 
      List.map (fun kt ->  BinOpAppCPS (kt,b,r,s)) (all_one_step_cps_cont_trans k)
    | IfCPS (r,e1,e2) ->
      (List.map (fun e1' -> IfCPS (r,e1',e2)) (all_one_step_cps_trans e1)) @
        (List.map (fun e2' -> IfCPS (r,e1,e2')) (all_one_step_cps_trans e2))
    | AppCPS (k,x1,x2) ->  
      List.map (fun kt -> AppCPS (kt,x1,x2)) (all_one_step_cps_cont_trans k)
    | FunCPS (k,x,Kvar,e) -> 
      (List.map (fun kt -> FunCPS (kt,x,Kvar,e)) (all_one_step_cps_cont_trans k)) @ 
      (List.map (fun e' -> FunCPS (k,x,Kvar,e')) (all_one_step_cps_trans e))
    | FixCPS (k,f,x,Kvar,e) ->  
      (List.map (fun kt -> FixCPS (kt,f,x,Kvar,e)) (all_one_step_cps_cont_trans k)) @ 
      (List.map (fun e' -> FixCPS (k,f,x,Kvar,e'))  (all_one_step_cps_trans e))
    | CPS_Trans (e, k) ->
      (top_one_step_cps_trans e k) ::
      List.map (fun kt -> CPS_Trans (e, kt)) (all_one_step_cps_cont_trans k)
and
   all_one_step_cps_cont_trans k =
   match k with External -> [] 
   | ContMetaVar -> []
   | ContVarCPS _ -> []
   | FnContCPS (k, e) -> List.map (fun e' -> FnContCPS (k, e')) (all_one_step_cps_trans e)
