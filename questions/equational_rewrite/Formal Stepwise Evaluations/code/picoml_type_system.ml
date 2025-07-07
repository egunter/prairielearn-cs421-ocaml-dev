(*
File: picoml_type_system.ml
Author: Elsa L. Gunter 
Share and Enjoy
*)

open Utils
open Gen_env
open Picoml_exp_and_val
open Picoml_types

(* fixed signatures *)
let const_signature const = match const with
   TrueConst | FalseConst -> (([], bool_ty):polyTy)
 | IntConst n -> ([], int_ty)
 | FloatConst f -> ([], float_ty)
 | StringConst s -> ([], string_ty)
 | NilConst -> ([0],mk_list_ty (TyVar 0))
 | UnitConst -> ([], unit_ty)

let basic_value_signature value =
  match value
  with UnitVal ->  ([], unit_ty)
    | TrueVal | FalseVal -> (([], bool_ty):polyTy)
    | IntVal _ ->  ([], int_ty)
    | FloatVal f -> ([], float_ty)
    | StringVal s -> ([], string_ty)

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
       let alpha = TyVar 0
       in ([0], 
              mk_fun_ty alpha (mk_fun_ty (mk_list_ty alpha) (mk_list_ty alpha)))
   | CommaOp ->
       let alpha = TyVar 0 in
       let beta = TyVar 1 in
           ([0;1],
            mk_fun_ty alpha (mk_fun_ty beta (mk_pair_ty alpha beta)))
   | EqOp -> ([],mk_fun_ty int_ty (mk_fun_ty int_ty bool_ty))
   | GreaterOp ->
     let alpha = TyVar 0 in ([0],mk_fun_ty alpha (mk_fun_ty alpha bool_ty))

let monop_signature monop = match monop with
    | HdOp -> let alpha = TyVar 0 in([0], mk_fun_ty (mk_list_ty alpha) alpha)
    | TlOp -> let alpha = TyVar 0 in
                  ([0], mk_fun_ty (mk_list_ty alpha) (mk_list_ty alpha))
    | PrintOp -> ([], mk_fun_ty string_ty unit_ty)
    | IntNegOp -> ([], mk_fun_ty int_ty int_ty)
    | FstOp -> let t1,t2 = TyVar 0,TyVar 1
             in ([0;1],mk_fun_ty (mk_pair_ty t1 t2) t1)
    | SndOp -> let t1,t2 = TyVar 0,TyVar 1
             in ([0;1],mk_fun_ty (mk_pair_ty t1 t2) t2)

type type_env = polyTy env

let string_of_type_env gamma = string_of_env string_of_polyTy ":" "," gamma


let freeVarsEnv l = delete_duplicates (
    List.fold_right (fun (_,pty) fvs -> freeVarsPolyTy pty @ fvs) l [])


(*judgment*) 
type judgment =
   ExpJudgment of type_env * exp * monoTy
 | DecJudgment of type_env * dec * type_env

let string_of_judgment judgment =
  match judgment with ExpJudgment(gamma, exp, monoTy) ->
        string_of_type_env gamma ^ " |= "^ string_of_exp exp ^
         " : " ^ string_of_monoTy monoTy
  | DecJudgment (gamma, dec, delta) ->
        string_of_type_env gamma ^ " |= "^ string_of_dec dec ^
         " : " ^ string_of_type_env delta

type proof = Proof of proof list * judgment

(*proof printing*)
let string_of_proof p =
  let depth_max = 10 in
  let rec string_of_struts = function
     []    -> ""
   | x::[] -> (if x then "|-" else "|-")  (* ??? *)
   | x::xs -> (if x then "  " else "| ")^ string_of_struts xs
  in let rec string_of_proof_aux (Proof(ant,conc)) depth lst =
    "\n"^ "  "^ string_of_struts lst^
    (if (depth > 0) then "-" else "")^
    let assum = ant in
      string_of_judgment conc ^
      if depth <= depth_max
         then string_of_assum depth lst assum
      else ""
  and string_of_assum depth lst assum =
    match assum with 
       []     -> ""
     | p'::ps -> string_of_proof_aux p' (depth + 1) (lst@[ps=[]])^
                 string_of_assum depth lst ps
  in
    string_of_proof_aux p 0 []^ "\n\n"

type substitution = (typeVar * monoTy) list

let subst_fun (s:substitution) n = (try List.assoc n s with _ -> TyVar n)

(*unification algorithm*)
(* Problem 1 *)
let rec contains n ty =
  match ty with
    TyVar m -> n=m
  | TyConst(st, typelst) ->
     List.fold_left (fun xl x -> if xl then xl else contains n x) false typelst;;

(* Problem 2 *)
let rec substitute ie ty = 
  let n,sub = ie 
  in match ty with
       TyVar m -> if n=m then sub else ty
     | TyConst(st, typelist) -> TyConst(st, List.map (fun t -> substitute ie t) typelist);;

let polyTySubstitute s (pty:polyTy) =
    match s with  (n,residue) ->
    (match pty with (bound_vars, ty) -> 
           if List.mem n bound_vars then pty
           else ((bound_vars, substitute s ty):polyTy))
    

(* Problem 3 *)
let rec monoTy_lift_subst (s:substitution) ty =
  match ty with
    TyVar m -> subst_fun s m
  | TyConst(st, typelst) ->  TyConst(st, List.map (fun t -> monoTy_lift_subst s t) typelst);;

let rec monoTy_rename_tyvars s mty =
    match mty with
      TyVar n -> (match lookup s n with Some m -> TyVar m | _ -> mty)
    | TyConst(c, tys) -> TyConst(c, List.map (monoTy_rename_tyvars s) tys)

let subst_compose (s2:substitution) (s1:substitution) : substitution =
    (List.filter (fun (tv,_) -> not(List.mem_assoc tv s1)) s2) @ 
    (List.map (fun (tv,residue) -> (tv, monoTy_lift_subst s2 residue)) s1)

let gen (env:type_env) ty =
    let env_fvs = freeVarsEnv env in
    ((List.filter (fun v -> not(List.mem v env_fvs)) (freeVarsMonoTy ty), ty):polyTy)

let freshInstance ((tvs, ty):polyTy) =
    let fresh_subst = List.fold_right (fun tv s -> ((tv,fresh())::s)) tvs [] in
    monoTy_lift_subst fresh_subst ty

let first_not_in n l =
    let rec first m n l =
        if n > 0 then
         if List.mem m l then first (m+1) n l else m :: (first (m+1) (n - 1) l)
        else []
    in first 0 n l

let alpha_conv ftvs (pty:polyTy) =
    match pty with (btvs, ty) ->
    (let fresh_bvars =
         first_not_in (List.length btvs) (ftvs @ (freeVarsPolyTy pty))
     in (fresh_bvars,
         monoTy_lift_subst (List.combine btvs (List.map (fun v -> TyVar v) fresh_bvars))
         ty))

let polyTy_lift_subst s pty =
	let rec fvsfun x r = match x with
		| TyVar n -> n :: r
		| TyConst (_, l) -> List.fold_right fvsfun l r
	in
	let fvs = List.fold_right fvsfun (snd(List.split s)) [] in
    let (nbvs, nty) = alpha_conv fvs pty in
    ((nbvs, monoTy_lift_subst s nty):polyTy)

let rec mk_bty_renaming n bty =
    match bty with [] -> ([],[])
    | (x::xs) -> (match mk_bty_renaming (n-1) xs
                   with (s,l) -> (((x,n) :: s), n :: l))

let polyTy_rename_tyvars s (bty, mty) =
    let (renaming,new_bty) = mk_bty_renaming (~-7) bty in
    (new_bty, monoTy_rename_tyvars s (monoTy_rename_tyvars renaming mty))

let env_rename_tyvars s (env: 'a env) =
    ((List.map
      (fun (x,polyTy) -> (x,polyTy_rename_tyvars s polyTy)) env): 'a env)

let env_lift_subst s (env:'a env) =
    ((List.map (fun (x,polyTy) -> (x,polyTy_lift_subst s polyTy)) env):'a env)


(* Problem 4 *)
let rec unify eqlst : substitution option =
  let rec addNewEqs lst1 lst2 acc =
    match lst1,lst2 with
      [],[] -> Some acc
    | t::tl, t'::tl' -> addNewEqs tl tl' ((t,t')::acc)
    | _ -> None
  in
  match eqlst with
    [] -> Some([])
    (* Delete *)
  | (s,t)::eqs when s=t -> unify eqs
    (* Eliminate *)
  | (TyVar(n),t)::eqs when not(contains n t)-> 
      let eqs' = List.map (fun (t1,t2) -> (substitute (n,t) t1 , substitute (n,t) t2)) eqs
      in (match unify eqs' with
           None -> None
         | Some(phi) -> Some((n, monoTy_lift_subst phi t):: phi))
    (* Orient *)
  | (TyConst(str, tl), TyVar(m))::eqs -> unify ((TyVar(m), TyConst(str, tl))::eqs)
    (* Decompose *)
  | (TyConst(str, tl), TyConst(str', tl'))::eqs when str=str' -> 
      (match (addNewEqs tl tl' eqs) with
        None -> None
      | Some l -> unify l)
    (* Other *)
  | _ -> None
;;


(*-----------------------------------------------*)

(*constraint list*)
type consList = (monoTy * monoTy) list


(*applying a substitution to a proof*)

(*applying a substitution to a proof*)
let rec proof_lift_subst f = function
    Proof(assum, ExpJudgment(gamma, exp, monoTy)) ->
    Proof(List.map (proof_lift_subst f) assum,
          ExpJudgment(env_lift_subst f gamma, exp, monoTy_lift_subst f monoTy))
 | Proof(assum, DecJudgment(gamma, dec, delta)) ->
    Proof(List.map (proof_lift_subst f) assum,
          DecJudgment(env_lift_subst f gamma, dec, env_lift_subst f delta))

let rec proof_rename_tyvars f = function
    Proof(assum, ExpJudgment(gamma, exp, monoTy)) ->
    Proof(List.map (proof_rename_tyvars f) assum,
          ExpJudgment(env_rename_tyvars f gamma, exp,
                      monoTy_rename_tyvars f monoTy))
 | Proof(assum, DecJudgment(gamma, dec, delta)) ->
    Proof(List.map (proof_rename_tyvars f) assum,
          DecJudgment(env_rename_tyvars f gamma, dec,
                      env_rename_tyvars f delta))

let get_ty = function
   None       -> raise(Failure "None")
 | Some(ty,p) -> ty

let get_proof = function
   None       -> raise(Failure "None")
 | Some(ty,p) -> p

let infer_exp gather_exp (gamma:type_env) (exp:exp) = 
  let ty = fresh() in
  let result = 
    match gather_exp gamma exp ty with
       None         -> None
     | Some(proof,sigma) -> match ty with
          | TyVar n -> Some (subst_fun sigma n, proof_lift_subst sigma proof)
          | _       -> None
  in let _ = reset() in
  result;;

let infer_dec gather_dec (gamma:type_env) (dec:dec) =
  let result = 
    match gather_dec gamma dec with
       None -> None
     | Some(proof,sigma) -> Some (proof_lift_subst sigma proof)
  in let _ = reset() in
  result;;

let string_of_constraints c =
  let rec aux c =
     match c with 
     | [] -> ""
     | [(s,t)] ->  (string_of_monoTy s^ " -->; "^ string_of_monoTy t)
     | (s,t)::c' -> (string_of_monoTy s^ " -->; "^ string_of_monoTy t^
		     "; "^ aux c')
  in ("["^ aux c^ "]\n")

 
let string_of_substitution s =
  let rec aux s =
     match s with 
     | [] -> ""
     | [(i,t)] -> ((string_of_typeVar i)  ^ " -->; " ^ string_of_monoTy t)
     | (i,t)::s' -> (((string_of_typeVar i)  ^ " -->; ")^
                     string_of_monoTy t^ "; "^ aux s')
  in ("["^ aux s^ "]\n")


let niceInfer_exp gather_exp (gamma:type_env) exp = 
  let ty = fresh()
  in
  let result = 
    match gather_exp gamma exp ty with
     None ->
      (print_string("Failure: No type for expression: "^
       string_of_exp exp^ "\n"^
       "in the environment: "^
       string_of_env string_of_polyTy ":" "," gamma^ "\n");
       raise (Failure ""))
   | Some (p,s) ->
   (string_of_proof p^
	(*
   "Constraints: "^
   string_of_constraints c ^
   "Unifying..."^
   match unify c with
     None -> ("Failure: No solution for these constraints!\n"^
              raise (Failure ""))
   | Some s ->
	*)
   ("Unifying substitution: "^
    string_of_substitution s^
    "Substituting...\n"^
    let new_p = proof_lift_subst s p in
    string_of_proof new_p)) in
  let _ = reset() in
  result;;

let niceInfer_dec
    (gather_dec:(type_env -> dec -> (proof * type_env * substitution) option))
    (gamma:type_env) dec = 
  let result = 
    match gather_dec gamma dec with
     None ->
      (print_string("Failure: No type for declaraion: "^
       string_of_dec dec^ "\n"^
       "in the environment: "^
       string_of_env string_of_polyTy ":" "," gamma^ "\n");
       raise (Failure ""))
   | Some (p,d,s) ->
   (string_of_proof p^
   ("Unifying substitution: "^
    string_of_substitution s^
    "Substituting...\n"^
    let new_p = proof_lift_subst s p in
    string_of_proof new_p)) in
  let _ = reset() in
  result;;

(* Collect all the TyVar indices in a proof *)

let rec collectTypeVars ty lst =
  match ty with
    TyVar m -> m::lst
  | TyConst(st, typelst) -> List.fold_left (fun xl x -> collectTypeVars x xl) lst typelst

let rec collectFreeTypeVars bty ty lst =
  match ty with
    TyVar m -> if List.mem m bty then lst else m::lst
  | TyConst(st, typelst) ->
    List.fold_left (fun xl x -> collectFreeTypeVars bty x xl) lst typelst

let collectPolyTyVars (bty,mty) lst = collectFreeTypeVars bty mty lst

let collectEnvVars (gamma:type_env) lst =
    List.fold_left (fun tys (_,pty)-> collectPolyTyVars pty tys) lst gamma

let collectJdgVars jdg lst =
    match jdg with ExpJudgment(gamma, exp, monoTy) ->
        collectEnvVars gamma (collectTypeVars monoTy lst)
    | DecJudgment(gamma, dec, delta) ->
        collectEnvVars gamma (collectEnvVars delta lst)

let rec collectProofVars prf lst =
  match prf with Proof (assum, jdg)
   -> collectAssumVars assum (collectJdgVars jdg lst)
and collectAssumVars assum lst =
  match assum with 
    []     -> lst
  | p::ps -> collectAssumVars ps (collectProofVars p lst)

let canonicalize_proof prf_opt =
    match prf_opt with None -> None
    | Some(ty, prf) ->
  let (varlst,_) =
    List.fold_right (fun x (xl,idx) -> ((x,idx)::xl), idx+1) 
      (delete_duplicates (collectProofVars prf (collectTypeVars ty []))) 
      ([],1)
  in Some(monoTy_rename_tyvars varlst ty, proof_rename_tyvars varlst prf)

let canon = canonicalize_proof

let canon_dec prf_opt =
    match prf_opt with None -> None
    | Some prf ->
  let (varlst,_) =
    List.fold_right (fun x (xl,idx) -> ((x, idx)::xl), idx+1) 
      (delete_duplicates (collectProofVars prf []))
      ([],1)
  in Some(proof_rename_tyvars varlst prf)

(* ML3's inferencer *)

let rec gather_exp_ty_substitution gamma exp tau =
    let judgment = ExpJudgment(gamma, exp, tau) in
(*
    let _ = print_string ("Trying to type "^ string_of_judgment judgment^"\n") in
*)
    let result =
    match exp
    with ConstExp c ->
         let tau' = const_signature c in
         (match unify [(tau, freshInstance tau')]
          with None       -> None
             | Some sigma -> Some(Proof([],judgment), sigma))
    | VarExp x -> 
      (match lookup_env gamma x with None -> None
       | Some gamma_x ->
         (match unify [(tau, freshInstance gamma_x)]
          with None       -> None
             | Some sigma -> Some(Proof([],judgment), sigma)))
    | BinOpAppExp (binop, e1,e2) ->
      let tau' = binop_signature binop in
      let tau1 = fresh() in
      let tau2 = fresh() in
      (match gather_exp_ty_substitution gamma e1 tau1
       with None -> None
       | Some(pf1, sigma1) ->
         (match gather_exp_ty_substitution (env_lift_subst sigma1 gamma) e2 tau2
          with None -> None
          | Some (pf2, sigma2) ->
            let sigma21 = subst_compose sigma2 sigma1 in
            (match unify[(monoTy_lift_subst sigma21
                          (mk_fun_ty tau1 (mk_fun_ty tau2 tau)),
                         freshInstance tau')]
             with None -> None
             | Some sigma3 -> 
               Some(Proof([pf1;pf2], judgment),subst_compose sigma3 sigma21))))
    | MonOpAppExp (monop, e1) ->
      let tau' = monop_signature monop in
      let tau1 = fresh() in
      (match gather_exp_ty_substitution gamma e1 tau1
       with None -> None
       | Some(pf, sigma) ->
         (match unify[(monoTy_lift_subst sigma (mk_fun_ty tau1 tau),
                       freshInstance tau')]
          with None -> None
          | Some subst ->
            Some(Proof([pf], judgment),
                 subst_compose subst sigma)))
    | IfExp(e1,e2,e3) ->
      (match gather_exp_ty_substitution gamma e1 bool_ty
       with None -> None
       | Some(pf1, sigma1) ->
         (match gather_exp_ty_substitution
                (env_lift_subst sigma1 gamma) e2 (monoTy_lift_subst sigma1 tau)
          with None -> None
          | Some (pf2, sigma2) ->
            let sigma21 = subst_compose sigma2 sigma1 in
            (match gather_exp_ty_substitution
                   (env_lift_subst sigma21 gamma) e3
                   (monoTy_lift_subst sigma21 tau)
             with  None -> None
             | Some(pf3, sigma3) ->
               Some(Proof([pf1;pf2;pf3], judgment), subst_compose sigma3 sigma21))))
    | FunExp(x,e) ->
      let tau1 = fresh() in
      let tau2 = fresh() in
      (match gather_exp_ty_substitution
             (ins_env gamma x (polyTy_of_monoTy tau1)) e tau2
       with None -> None
       | Some (pf, sigma) ->
         (match unify [(monoTy_lift_subst sigma tau,
                        monoTy_lift_subst sigma (mk_fun_ty tau1 tau2))]
          with None -> None
          | Some sigma1 ->
            Some(Proof([pf],judgment), subst_compose sigma1 sigma)))
    | AppExp(e1,e2) ->
      let tau1 = fresh() in
      (match gather_exp_ty_substitution gamma e1 (mk_fun_ty tau1 tau)
       with None -> None
       | Some(pf1, sigma1) ->
         (match gather_exp_ty_substitution (env_lift_subst sigma1 gamma) e2
                                           (monoTy_lift_subst sigma1 tau1)
          with None -> None
          | Some (pf2, sigma2) ->
            Some(Proof([pf1;pf2], judgment), subst_compose sigma2 sigma1)))
    | RaiseExp e ->
      (match gather_exp_ty_substitution gamma e int_ty
       with None -> None
       | Some(pf, sigma) -> Some(Proof([pf],judgment), sigma))
    | LetInExp(x,e1,e2)  -> 
       let tau1 = fresh() in
       (match gather_exp_ty_substitution gamma e1 tau1
	with None -> None
	   | Some(pf1, sigma1) -> 
	      let delta_env = make_env x (gen (env_lift_subst sigma1 gamma) 
					      (monoTy_lift_subst sigma1 tau1)) in
	      (match gather_exp_ty_substitution 
		       (sum_env delta_env (env_lift_subst sigma1 gamma)) e2
                         (monoTy_lift_subst sigma1 tau)
	       with None -> None
		  | Some (pf2,sigma2) ->
		     let sigma21 = subst_compose sigma2 sigma1 in
		     Some(Proof([pf1;pf2], judgment), sigma21)))
    | LetRecInExp(f,x,e1,e2) ->
       let tau1  = fresh() in
       let tau2 = fresh() in
       let tau1_to_tau2 = mk_fun_ty tau1 tau2 in
       (match gather_exp_ty_substitution
		(ins_env (ins_env gamma f (polyTy_of_monoTy tau1_to_tau2))
			  x (polyTy_of_monoTy tau1))
		e1 tau2
	with None -> None
	   | Some(pf1, sigma1) -> 
              let sigma1_gamma = env_lift_subst sigma1 gamma in
	      let sigma1_tau1_to_tau2 = monoTy_lift_subst sigma1 tau1_to_tau2 in
	      (match gather_exp_ty_substitution
                     (ins_env sigma1_gamma f (gen sigma1_gamma sigma1_tau1_to_tau2))
		     e2 (monoTy_lift_subst sigma1 tau)
	       with None -> None
		  | Some(pf2,sigma2) ->
		     let sigma21 = subst_compose sigma2 sigma1 in
		     Some(Proof([pf1;pf2], judgment),sigma21)))
    | TryWithExp (e,intopt1,e1, match_list) ->
      (match (gather_exp_ty_substitution gamma e tau)
       with None -> None
       | Some (pf, sigma) ->
         (match
           List.fold_left
           (fun part_result -> fun (intopti, ei) ->
            (match part_result with None -> None
             | Some (rev_pflist, comp_sigmas) ->
               (match gather_exp_ty_substitution
                      (env_lift_subst comp_sigmas gamma) ei
                      (monoTy_lift_subst comp_sigmas tau)
                with None -> None
                | Some (pfi, sigmai) ->
                  Some (pfi :: rev_pflist, subst_compose sigmai comp_sigmas))))
           (Some([pf], sigma))
           ((intopt1,e1):: match_list)
           with None -> None
           | Some (rev_pflist, comp_subst) ->
             Some(Proof(List.rev rev_pflist, judgment), comp_subst)))
    | Eval (e, env) -> gather_exp_ty_substitution gamma e tau
    (* Should I walk the value memory as well? *)
    | Val v -> gather_val_ty_substitution v tau

in (
(*
    (match result
     with None ->
      print_string ("Failed to type "^string_of_judgment judgment^"\n")
     | Some (_, subst) -> print_string ("Succeeded in typing "^
                               string_of_judgment judgment^"\n"^
"  with substitution "^ string_of_substitution subst ^"\n"));
*)
  result)
and gather_val_ty_substitution v tau =
  let judgment = ExpJudgment([], (Val v) , tau) in
     (match v
      with BasicVal bv -> gather_basic_val_ty_substitution bv tau
        | PairVal (v1, v2) ->
          let (tau1, tau2) = (fresh(),fresh()) in
          (match gather_val_ty_substitution v1 tau1
           with None -> None
             | Some(pf1, sigma1) ->
               (match gather_val_ty_substitution v2 tau2
                with None -> None
                  | Some (pf2, sigma2) ->
                    let sigma12 = subst_compose sigma2 sigma1
                    in
                    (match unify [(monoTy_lift_subst sigma12 tau,
                        monoTy_lift_subst sigma12 (mk_pair_ty tau1 tau2))]
                     with None -> None
                       | Some sigmapair ->
                         Some(Proof([pf1;pf2], judgment), subst_compose sigmapair sigma12))))
        | ListVal vl -> gather_val_list_ty_substitution vl tau
        | Closure (x, e, mem) ->
          (match infer_type_env_from_value_env mem with None -> None
            | Some new_gamma ->
              gather_exp_ty_substitution new_gamma (FunExp(x,e)) tau)
        (* We need to use the "type environment" of the memory in the
           closure for the variables in the body *)
        | RecVarVal (f, x, e, mem) ->
          (match infer_type_env_from_value_env mem with None -> None
            | Some new_gamma ->
              gather_exp_ty_substitution new_gamma (LetRecInExp(f,x,e, VarExp f)) tau))
            
and gather_basic_val_ty_substitution bv tau = (* Is gamma ever used?  I would think not. *)
  gather_exp_ty_substitution [] (ConstExp(const_of_basic_value bv)) tau

and gather_val_list_ty_substitution vl tau =
  let judgment = ExpJudgment([], Val (ListVal vl), tau) in
  match vl with [] -> gather_exp_ty_substitution [] (ConstExp NilConst) tau
    | v :: nvl ->
      (let tauv = fresh() in
       (match unify [(mk_list_ty tauv, tau)]
        with None -> None
          | Some sigmav1 ->
            (match gather_val_ty_substitution v (monoTy_lift_subst sigmav1 tauv)
             with None -> None
               | Some(pfv, sigmav2) ->
                 let sigmav = subst_compose sigmav2 sigmav1 in
                 (match gather_val_list_ty_substitution nvl (monoTy_lift_subst sigmav tau)
                  with None -> None
                    | Some (pfvl, sigmavl) ->
                      Some(Proof ([pfv;pfvl], judgment), subst_compose sigmav sigmavl)))))
            

and infer_type_env_from_value_env mem =
  List.fold_right
    (fun (id, v) -> fun r ->
      (match r with None -> None
        | Some ty_env -> 
          let tau = fresh() in
          (match gather_val_ty_substitution v tau
           with None -> None
             | Some (_, sigma) -> Some ((id, ([], monoTy_lift_subst sigma tau)):: ty_env))))
    mem
    (Some [])
    
              

let rec gather_dec_ty_substitution gamma dec =
    match dec with 
    | Anon e ->
      let tau = fresh() in
      (match gather_exp_ty_substitution gamma e tau
	with None -> None
	   | Some(pf, sigma) ->
             Some(Proof([pf],DecJudgment (gamma, dec, [])), sigma))
    | Let(x,e) -> 
       let tau = fresh() in
       (match gather_exp_ty_substitution gamma e tau
	with None -> None
	   | Some(pf, sigma) -> 
	      let delta_env = make_env x (gen (env_lift_subst sigma gamma) 
					      (monoTy_lift_subst sigma tau)) in
             Some(Proof([pf],DecJudgment (gamma, dec, delta_env)),sigma))
    | LetRec(f,x,e) ->
       let tau1  = fresh() in
       let tau2 = fresh() in
       let tau1_to_tau2 = mk_fun_ty tau1 tau2 in
       (match gather_exp_ty_substitution
		(ins_env (ins_env gamma f (polyTy_of_monoTy tau1_to_tau2))
			  x (polyTy_of_monoTy tau1))
		e tau2
	with None -> None
	   | Some(pf, sigma) -> 
              let sigma_gamma = env_lift_subst sigma gamma in
	      let sigma_tau1_to_tau2 = monoTy_lift_subst sigma tau1_to_tau2 in
              let delta_env =
                 (ins_env sigma_gamma f (gen sigma_gamma sigma_tau1_to_tau2))
              in 
	      Some(Proof([pf],DecJudgment (gamma, dec, delta_env)),sigma))

