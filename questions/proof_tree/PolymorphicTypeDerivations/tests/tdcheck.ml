(* File: hoareLogicCheck.ml *)
(* Author: John Lee & Elsa Gunter *)
(* Copyright 2017 *)


open Genutils
open Gencheck
open Common
open Solution
open Treedata
open Student

open Interface

  (*
    custom term data structures
  *)

type type_check_obj =
  Gamma of type_env
  | SideCond of monoTy env

let string_of_type_check_obj tyckobj =
  match tyckobj
  with Gamma ty_env -> string_of_type_env ty_env
    | SideCond subst -> string_of_substitution subst  

let gamma_equiv g1 g2 = match (g1, g2) with
  | (TermObj (Gamma env1, EnvSrc env1'), TermObj (Gamma env2, EnvSrc env2')) -> equiv_env env1 env2
  | _ -> false

let gen (env:type_env) ty =
    let env_fvs = freeVarsEnv env in
    ((List.filter (fun v -> not(List.mem v env_fvs)) (freeVarsMonoTy ty), ty):polyTy)

  (*
    #################################
     PICOML TO TERM CONVERSION
    #################################
  *)

let const_to_term c =
  let src = ExpSrc (ConstExp c) in 
  match c with
      BoolConst b ->
        let s = if b then "true" else "false" in
        TermElem("bool", [TermElem(s, [], ConcrSynSrc s)], src)
    | IntConst i ->
      let s = string_of_int i in
      TermElem("int", [TermElem(s, [], ConcrSynSrc s)], src)
    | FloatConst f -> 
      let s = string_of_float f in
      TermElem("float", [TermElem(s, [], ConcrSynSrc s)], src)
    | StringConst s -> 
      TermElem("string", [TermElem(s, [], ConcrSynSrc s)], src)
    | NilConst ->  TermElem("nil", [], src)
    | UnitConst -> TermElem("unit", [], src)

let monop_to_term m =
  let s = string_of_mon_op m in TermElem(s, [], ConcrSynSrc s)
let binop_to_term b = 
  let s = string_of_bin_op b in TermElem(s, [], ConcrSynSrc s)

let rec picoml_to_term exp =
  let src = ExpSrc exp in
  match exp with
  VarExp x -> TermElem("var", [TermElem(x, [], ConcrSynSrc x)], src)
  | ConstExp c -> TermElem("const", [const_to_term c], src)
  | MonOpAppExp(m, e) -> TermElem("monop", [monop_to_term m; picoml_to_term e], src)
  | BinOpAppExp(b, e1, e2) -> TermElem("binop", [binop_to_term b; picoml_to_term e1; picoml_to_term e2], src)
  | IfExp(e1, e2, e3) -> TermElem("if", [picoml_to_term e1; picoml_to_term e2; picoml_to_term e3], src)
  | AppExp(e1, e2) -> TermElem("app", [picoml_to_term e1; picoml_to_term e2], src)
  | FunExp(x, e) -> TermElem("fun", [TermElem(x, [], ConcrSynSrc x); picoml_to_term e], src)
  | LetInExp(x, e1, e2) -> TermElem("let", [TermElem(x, [], ConcrSynSrc x); picoml_to_term e1; picoml_to_term e2], src)
  | LetRecInExp(f, x, e1, e2) -> TermElem("letrec", [TermElem(f, [], ConcrSynSrc f); TermElem(x, [], ConcrSynSrc x); picoml_to_term e1; picoml_to_term e2], src)
  | _ -> raise (Failure "UNIMPLEMENTED: tdcheck.ml - Pattern matching over Try / Raise unsupported.")

let rec monoTy_to_term ty =
  let src = TySrc ty in
  match ty with
  TyConst(x, tList) -> TermElem("c_" ^ x, List.map monoTy_to_term tList,src)
  | TyVar(x) -> TermElem("var_", [TermElem(x, [], ConcrSynSrc x)], src)

let get_parsed_error p = match p with
  | ParseEmpty -> [ParseEmpty]
  | SyntaxError s -> [SyntaxError s]
  | _ -> []

let typing_stmt_to_parsed_term (TypingStmt(gamma, e, ty)) =
  match (gamma, e, ty) with
    | (ParseOk gamma, ParseOk e, ParseOk ty) ->
        let src = JdgmtSrc (gamma, e, ty) in
        JudgeParse (TermElem("judgment", [TermObj (Gamma gamma, EnvSrc gamma); picoml_to_term e; monoTy_to_term ty], src))
    | _ -> JudgeErr ((get_parsed_error gamma) @ (get_parsed_error e) @ (get_parsed_error ty))

let rec term_to_type tau1 = match tau1 with
  | TermObj _ -> raise (Failure "BUG: tdcheck - object found where type was expected")
  | TermElem(x, l, src) -> (match x with
    | "c_bool" -> TyConst("bool", [])
    | "c_int" -> TyConst("int", [])
    | "c_float" -> TyConst("float", [])
    | "c_string" -> TyConst("string", [])
    | "c_unit" -> TyConst("unit", [])
    | "c_list" -> TyConst("list", List.map term_to_type l)
    | "c_*" -> TyConst("*", List.map term_to_type l)
    | "c_->" -> TyConst("->", List.map term_to_type l)
    | "var_" -> (match l with
      | TermElem(x', _, _) :: _ -> TyVar x'
      | _ -> raise (Failure "BUG: tdcheck - no name for type variable could be found")
    )
    | _ -> raise (Failure ("BUG: tdcheck - unknown term `" ^ x ^ "`found where type was expected"))
  )

let sc_to_parsed_term sc_str = match process_sc sc_str with
  ParseOk sc_val -> JudgeParse (TermObj (SideCond sc_val, SubstSrc sc_val))
  | ParseEmpty -> JudgeParse (TermObj (SideCond [], SubstSrc []))
  | _ -> JudgeErr [SyntaxError sc_str]

  (*
    ##################################
     TYPE DERIVATION RULE DEFINITIONS
    ##################################
  *)

type td_rule = (type_check_obj, source, string) syntax_rule

let constRule: td_rule = SynRule("Const", "const",
    PatElem("judgment", [PatVar "gamma"; pat_cons "const" ["c"]; PatVar "tau"]), [], []
  )

let rec ty_of_monop m = (monop_signature (match m with
  | "hd" -> HdOp
  | "tl" -> TlOp
  | "print_string" -> PrintOp
  | "~" -> IntNegOp
  | "fst" -> FstOp
  | "snd" -> SndOp
  | _ -> raise (Failure "BUG: tdcheck - monop without type")))

let rec ty_of_binop m = (binop_signature (match m with
  | " + " -> IntPlusOp
  | " - " -> IntMinusOp
  | " * " -> IntTimesOp
  | " / " -> IntDivOp
  | " +. " -> FloatPlusOp
  | " -. " -> FloatMinusOp
  | " *. " -> FloatTimesOp
  | " /. " -> FloatDivOp
  | " ^ " -> ConcatOp
  | " :: " -> ConsOp
  | " , " -> CommaOp
  | " = " -> EqOp
  | " > " -> GreaterOp
  | "**" -> ExpoOp
  | "mod" -> ModOp
  | _ -> raise (Failure "BUG: tdcheck - binop without type")))

let check_polymorphic_instance (bnd_vars, body_ty) subst monoTy =
  let norm_subst = delete_duplicates subst in
  let subst_vars = (List.map fst norm_subst) in
  if subst_vars = delete_duplicates subst_vars
  then if
      (List.for_all (fun bv -> List.mem bv subst_vars) bnd_vars) &&
        (List.for_all (fun sv -> List.mem sv bnd_vars) subst_vars)
    then if monoTy_lift_subst norm_subst body_ty = monoTy
      then None
      else Some ("Polymorhic instance check: Specializing "
                 ^(string_of_polyTy (bnd_vars, body_ty))
                 ^" by the subtitution "^(string_of_substitution subst)
                 ^"\n does not yield "^(string_of_monoTy monoTy))
    else Some ("Polymorhic instance check: Domain of substitution "
               ^(string_of_substitution subst)^"\n and binder of polymorphic type "
               ^(string_of_polyTy (bnd_vars, body_ty))^" do not match."
    )
  else Some ("Polymorhic instance check: Ill-formed substituttion: "
             ^(string_of_substitution subst))

let var_inst_sc env =
  match (lookup_env env "gamma", lookup_env env "sc") with
  | (Some (TermObj (Gamma g, g_src)), Some (TermObj (SideCond sc, sc_src))) ->
    (match (lookup_env env "x", lookup_env env "tau") with
      | (Some (TermElem(x, _, _)), Some tau) ->
        (match lookup_env g x with
    | Some (bnd_vars, x_ty) ->
            check_polymorphic_instance (bnd_vars, x_ty) sc (term_to_type tau)
    | _ -> Some (x ^ " is not in the environment "^(string_of_type_env g))
        )
      | _ -> Some ("BUG? \"x\" or \"tau\" is missing from the results a matching the rule.")
    )
  | (Some _, Some _) -> Some ("BUG! Not getting the right kind of objects.") 
  | (Some _, None) -> Some ("Side condition missing!")
  | (None, _) -> Some ("BUG? Environment missing!")

let varRule: td_rule = SynRule("Var", "var",
    PatElem("judgment", [PatVar "gamma"; pat_cons "var" ["x"]; PatVar "tau"]), [],
    [SCPred("var_inst_sc", var_inst_sc)]
  )

let monop_inst_sc env =
  if (match (lookup_env env "sc", lookup_env env "m") with
    | (Some (TermObj (SideCond sc, _)), Some (TermElem(m, _, _))) ->
      (match (lookup_env env "tau1", lookup_env env "tau2") with
        | (Some tau1, Some tau2) ->
          let (_, x_ty) = ty_of_monop m
          in (monoTy_lift_subst sc x_ty) = (mk_fun_ty (term_to_type tau1) (term_to_type tau2))
        | _ -> false
      )
    | _ -> false)
  then None
  else Some "fail"

let monopRule: td_rule = SynRule("MonOp", "monop",
    PatElem("judgment", [PatVar "gamma"; pat_cons "monop" ["m"; "e"]; PatVar "tau2"]), [
      PatElem("judgment", [PatVar "gamma"; PatVar "e"; PatVar "tau1"])
    ], [SCPred("monop_inst_sc", monop_inst_sc)]
  )

let binop_inst_sc env = match (lookup_env env "sc", lookup_env env "b") with
  (Some (TermObj (SideCond sc, _)), Some (TermElem(b, _, _))) ->
    (match (lookup_env env "tau1", lookup_env env "tau2", lookup_env env "tau3") with
      | (Some tau1, Some tau2, Some tau3) ->
        let (_, x_ty) = ty_of_binop b
        in let ret_ty = (mk_fun_ty (term_to_type tau2) (term_to_type tau3))
        in 
          if (monoTy_lift_subst sc x_ty) = (mk_fun_ty (term_to_type tau1) ret_ty) then None
            else Some "fail_eq"
      | _ -> Some "fail_type"
    )
  | _ -> Some "fail_struct"

let binopRule: td_rule = SynRule("BinOp", "binop",
    PatElem("judgment", [PatVar "gamma"; pat_cons "binop" ["b"; "e1"; "e2"]; PatVar "tau3"]), [
      PatElem("judgment", [PatVar "gamma"; PatVar "e1"; PatVar "tau1"]);
      PatElem("judgment", [PatVar "gamma"; PatVar "e2"; PatVar "tau2"])
    ], [SCPred("binop_inst_sc", binop_inst_sc)]
  )

let ifRule: td_rule = SynRule("If", "if",
    PatElem("judgment", [PatVar "gamma"; pat_cons "if" ["e1"; "e2"; "e3"]; PatVar "tau"]), [
      PatElem("judgment", [PatVar "gamma"; PatVar "e1"; pat_cons "c_bool" []]);
      PatElem("judgment", [PatVar "gamma"; PatVar "e2"; PatVar "tau"]);
      PatElem("judgment", [PatVar "gamma"; PatVar "e3"; PatVar "tau"])
    ], []
  )

let appRule: td_rule = SynRule("App", "app",
    PatElem("judgment", [PatVar "gamma"; pat_cons "app" ["e1"; "e2"]; PatVar "tau2"]), [
      PatElem("judgment", [PatVar "gamma"; PatVar "e1"; pat_cons "c_->" ["tau1"; "tau2"]]);
      PatElem("judgment", [PatVar "gamma"; PatVar "e2"; PatVar "tau1"])
    ], []
  )

  (* gamma2 = [x : tau1] + gamma *)

let fun_rule_sc env =
  if (match (lookup_env env "gamma2", lookup_env env "gamma") with
    | (Some (TermObj (Gamma g2, _)), Some (TermObj (Gamma g, _))) ->
      (match (lookup_env env "x", lookup_env env "tau1") with
  | (Some (TermElem(x, _, _)), Some tau1) ->
    equiv_env g2 (add_env g x ([], term_to_type tau1))
  | _ -> false
      )
    | _ -> false)
  then None
  else Some "fail"
  (*Some "Environment given to sub proof of function body expression (\"\"e\"\") is incorrect."*)

let funRule: td_rule = SynRule("Fun", "fun",
    PatElem("judgment", [PatVar "gamma"; pat_cons "fun" ["x"; "e"]; pat_cons "c_->" ["tau1"; "tau2"]]), [
      PatElem("judgment", [PatVar "gamma2"; PatVar "e"; PatVar "tau2"])
    ], [SCPred("fun_e", fun_rule_sc)]
  )

  (* gamma2 = [x : GEN(gamma, tau1)] + gamma*)

let let_rule_sc env =
  if (match (lookup_env env "gamma2", lookup_env env "gamma") with
    | (Some (TermObj (Gamma g2, _)), Some (TermObj (Gamma g, _))) ->
      (match (lookup_env env "x", lookup_env env "tau1") with
  | (Some (TermElem(x, _, _)), Some tau1) ->
    equiv_env g2 (add_env g x (gen g (term_to_type tau1)))
  | _ -> false
      )
    | _ -> false)
  then None
  else Some "fail"

let letRule: td_rule = SynRule("Let", "let",
    PatElem("judgment", [PatVar "gamma"; pat_cons "let" ["x"; "e1"; "e2"]; PatVar "tau2"]), [
      PatElem("judgment", [PatVar "gamma"; PatVar "e1"; PatVar "tau1"]);
      PatElem("judgment", [PatVar "gamma2"; PatVar "e2"; PatVar "tau2"])
    ], [SCPred("let_e2", let_rule_sc)]
  )

  (*
    gamma2 = [x : tau1] + [f : tau1 -> tau2] + gamma
    gamma3 = [f : GEN(gamma, tau1 -> tau2)] + gamma
  *)

let letrec_rule_g2_sc env =
  if (match (lookup_env env "gamma2", lookup_env env "gamma") with
    | (Some (TermObj (Gamma g2, _)), Some (TermObj (Gamma g, _))) ->
      (match (lookup_env env "x", lookup_env env "f") with
  | (Some (TermElem(x, _, _)), Some (TermElem(f, _, _))) ->
          (match (lookup_env env "tau1", lookup_env env "tau2") with
      | (Some tau1, Some tau2) ->
        let t1 = term_to_type tau1
        in let g' = add_env g f ([], mk_fun_ty t1 (term_to_type tau2))
     in let g2' = add_env g' x ([], t1) 
        in equiv_env g2 g2'
      | _ -> false
    )
  | _ -> false
      )
    | _ -> false)
  then None
  else Some "fail"
    
let letrec_rule_g3_sc env =
  if (match (lookup_env env "gamma3", lookup_env env "gamma") with
    | (Some (TermObj (Gamma g3, _)), Some (TermObj (Gamma g, _))) ->
      (match (lookup_env env "f", lookup_env env "tau1", lookup_env env "tau2") with
  | (Some (TermElem(f, _, _)), Some tau1, Some tau2) ->
    let f_ty = mk_fun_ty (term_to_type tau1) (term_to_type tau2)
    in equiv_env g3 (add_env g f (gen g f_ty))
  | _ -> false
      )
    | _ -> false)
  then None
  else Some "fail"

let letRecRule: td_rule =
  SynRule("LetRec", "letrec",
    PatElem("judgment", [PatVar "gamma"; pat_cons "letrec" ["x"; "e1"; "e2"]; PatVar "tau2"]),
          [PatElem("judgment", [PatVar "gamma2"; PatVar "e1"; PatVar "tau1"]);
     PatElem("judgment", [PatVar "gamma3"; PatVar "e2"; PatVar "tau2"])],
    [SCPred("letrec_e1", letrec_rule_g2_sc); SCPred("letrec_e2", letrec_rule_g3_sc)]
  )
  (*
    ##############################
     COMPLETED PROBLEM DEFINITION
    ##############################
  *)

let string_of_failed_sc sc_name e_name = match (sc_name, e_name) with
    | ("var_inst_sc", e_name) -> e_name
    | ("monop_inst_sc", _) -> "Side condition (or absence there of) for unary operator is incorrect."
    | ("binop_inst_sc", "fail_struct") -> "Not all elements necessary for type-checking binary operator are present."
    | ("binop_inst_sc", "fail_type") -> "Not all types necessary for type-checking binary operator are present."
    | ("binop_inst_sc", "fail_eq") -> "Side condition (or absence there of) for binary operator is incorrect."
    | ("fun_e", _) -> "Environment given to sub proof of function body is incorrect."
    | ("let_e2", _) -> "Environment given to sub proof of the let's second sub-expression (\"\"e2\"\") is incorrect."
    | ("letrec_e1", _) -> "Environment given to sub proof of let rec's first sub-expression (\"\"e1\"\") is incorrect."
    | ("letrec_e2", _) -> "Environment given to sub proof of let rec's second sub-expression (\"\"e2\"\") is incorrect."
    | _ ->
      raise (Failure ("BUG: tdcheck - Unknown side condition error. String \"" ^ sc_name ^ "\" is not a recognized sc string."))

  (* NOTE: rules with the same label must use the same number of points *)

let rec term_eq _ t1 t2 = match (t1, t2) with
  | (TermElem(c1, cl1, _), TermElem(c2, cl2, _)) ->
    c1 = c2 && term_list_eq cl1 cl2
  | _ -> gamma_equiv t1 t2
and term_list_eq cl1 cl2 = match (cl1, cl2) with
  | ([], []) -> true
  | (c1 :: ct1, c2 :: ct2) -> (term_eq "" c1 c2) && term_list_eq ct1 ct2 
  | _ -> false

let typeCheck_pdef : (typing_stmt, type_check_obj, source, string) problem_def = {
  process = process_node;
  judgment_to_term = typing_stmt_to_parsed_term;
  sc_val = sc_to_parsed_term;
  rule_list = [
    (1, constRule); (1, varRule); (7, binopRule); (4, monopRule);
    (9, ifRule); (6, appRule); (4, funRule); (7, letRule); (8, letRecRule)
  ];
  guess_index = 1;
  gen_penalty = 3;
  eq_poly_fun = term_eq;
  string_of_judgment = string_of_typing_stmt;
  string_of_source = string_of_source;
  string_of_cons = (fun x -> match x with
    | "c_bool" -> "bool type"
    | "c_int" -> "int type"
    | "c_float" -> "float type"
    | "c_string" -> "string type"
    | "c_unit" -> "unit type"
    | "c_list" -> "list type"
    | "c_*" -> "pair type"
    | "c_->" -> "fun type"
    | "var_" -> "type variable"
    | _ -> x
  );
  string_of_failed_sc = string_of_failed_sc
}

let _ = Interface.grade typeCheck_pdef Student.tree








