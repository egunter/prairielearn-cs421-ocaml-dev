(* File: picoml_eval.ml *)

(* expressions for PicoML *)
type const = TrueConst | FalseConst | IntConst of int | FloatConst of float
           | StringConst of string | NilConst | UnitConst

let string_of_const c =
    match c 
    with IntConst n    -> if n < 0 then "~"^string_of_int(abs n) else string_of_int n
       | TrueConst     -> "true"
       | FalseConst    -> "false"
       | FloatConst f  -> string_of_float f
       | StringConst s -> "\"" ^ s ^ "\""
       | NilConst      -> "[]"
       | UnitConst     -> "()"

type bin_op = IntPlusOp | IntMinusOp | IntTimesOp | IntDivOp
           | FloatPlusOp | FloatMinusOp | FloatTimesOp | FloatDivOp 
           | ConcatOp | ConsOp | CommaOp | EqOp | GreaterOp 
           | ModOp | ExpoOp

let gt is_html = if is_html then "&gt;" else ">"
let lt is_html = if is_html then "&lt;" else "<"

let string_of_bin_op is_html = function 
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
   | GreaterOp -> " "^(gt is_html)^" "
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

let rec string_of_exp is_html = function
   VarExp s -> s
 | ConstExp c ->  string_of_const c
 | IfExp(e1,e2,e3)->"if " ^ (string_of_exp is_html e1) ^
                 " then " ^ (string_of_exp is_html e2) ^
                 " else " ^ (string_of_exp is_html e3)
 | MonOpAppExp (m,e) ->  (string_of_mon_op m) ^ " " ^ (paren_string_of_exp is_html e) 
 | BinOpAppExp (b,e1,e2) -> 
   (match b with CommaOp -> ("(" ^ (paren_string_of_exp is_html e1) ^ (string_of_bin_op is_html b) ^
                              (paren_string_of_exp is_html e2) ^ ")")
    | _ -> ((paren_string_of_exp is_html e1) ^ " " ^ (string_of_bin_op is_html b)
            ^ " " ^ (paren_string_of_exp is_html e2)))
 | AppExp(e1,e2) -> (non_app_paren_string_of_exp is_html e1) ^ " " ^ (paren_string_of_exp is_html e2) 
 | FunExp (x,e) ->  ("fun " ^ x ^ " -" ^ (gt is_html) ^" " ^ (string_of_exp is_html e))
 | LetInExp (x,e1,e2) -> ("let "^x^" = "^ (string_of_exp is_html e1) ^ " in " ^ (string_of_exp is_html e2))
 | LetRecInExp (f,x,e1,e2) -> 
    ("let rec "^f^" "^x^" = "^(string_of_exp is_html e1) ^ " in " ^ (string_of_exp is_html e2))
 | RaiseExp e -> "raise " ^ (string_of_exp is_html e)
 | TryWithExp (e,intopt1,exp1,match_list) ->
    "try " ^ (paren_string_of_exp is_html e) ^  " with " ^
     (string_of_exc_match is_html (intopt1,exp1)) ^
     (List.fold_left (fun s m -> (s^" | " ^ (string_of_exc_match is_html m))) "" match_list) 

and paren_string_of_exp is_html e =
    match e with VarExp _ | ConstExp _ -> string_of_exp is_html e
    | _ -> "(" ^ string_of_exp is_html e ^ ")"

and non_app_paren_string_of_exp is_html e =
    match e with AppExp (_,_) -> string_of_exp is_html e
    | _ -> paren_string_of_exp is_html e
 

and string_of_exc_match is_html (int_opt, e) =
    (match int_opt with None -> "_" | Some n -> string_of_int n) ^
    " -"^(gt is_html)^" " ^
    (string_of_exp is_html e)
							
let string_of_dec is_html = function
 | Anon e -> ("let _ = "^ (string_of_exp is_html e))
 | Let (s, e) ->  ("let "^ s ^" = " ^ (string_of_exp is_html e))
 | LetRec (fname,argname,fn) -> 
    ("let rec " ^ fname ^ " " ^ argname ^ " = " ^ (string_of_exp is_html fn))

let print_exp is_html exp = print_string (string_of_exp is_html exp) 
let print_dec is_html dec = print_string (string_of_dec is_html dec)

(* Util functions *)
let rec drop y = function
   []    -> []
 | x::xs -> if x=y then drop y xs else x::drop y xs

let rec delete_duplicates = function
   []    -> []
 | x::xs -> x::delete_duplicates (drop x xs)

(*type system*)

type typeVar = int

let rec expand n list =
    let q = n / 26 in
        if q = 0 then (n :: list)
        else expand q ((n mod 26)::list);;

let string_of_typeVar n = 
   let (num_list) =
       match (expand n [])
       with [] -> [] (* can't actually happen *)
          | [s] -> [s]
          | (x::xs) -> ((x - 1) :: xs)
   in
  List.fold_left
    (fun s x -> (s ^ (String.make 1 (Char.chr(x + 97))))) "'" num_list


type monoTy = TyVar of typeVar | TyConst of (string * monoTy list)

let rec string_of_monoTy t =
  let rec string_of_tylist = function
     []     -> ""
   | t'::[] -> string_of_monoTy t'
   | t'::ts -> string_of_monoTy t'^ ","^ string_of_tylist ts
  in
  let string_of_subty s =
  match s with 
     TyConst ("*", _) | TyConst ("-&gt;", _) -> ("("^ string_of_monoTy s^ ")")
   | _ ->  string_of_monoTy s
  in 
    match t with
       TyVar n         -> (string_of_typeVar n)
     |TyConst (name, []) -> name
     |TyConst (name, [ty]) -> (string_of_subty ty^ " "^ name)
     |TyConst ("*", [ty1; ty2]) -> (string_of_subty ty1^ " * "^ string_of_monoTy ty2)
     |TyConst ("-&gt;", [ty1; ty2]) -> (string_of_subty ty1^ " -&gt; "^ string_of_monoTy ty2)
     |TyConst (name, tys) -> ("("^ string_of_tylist tys^ ") "^ name)

let rec accummulate_freeVarsMonoTy fvs ty =
    match ty
    with TyVar n -> n::fvs
       | TyConst (c, tyl) -> List.fold_left accummulate_freeVarsMonoTy fvs tyl

let freeVarsMonoTy ty = delete_duplicates (accummulate_freeVarsMonoTy [] ty)

(*fresh type variable*)
let (fresh, reset) =
   let nxt = ref 0 in
   let f () = (nxt := !nxt + 1; TyVar(!nxt)) in
   let r () = nxt := 0 in
    (f, r)

let bool_ty = TyConst("bool",[])
let int_ty = TyConst ("int", [])
let float_ty = TyConst ("float",[])
let string_ty = TyConst ("string",[])
let unit_ty = TyConst("unit", [])
let mk_pair_ty ty1 ty2 = TyConst("*",[ty1;ty2])
let mk_fun_ty ty1 ty2 = TyConst("-&gt;",[ty1;ty2])
let mk_list_ty ty = TyConst("list",[ty])

type polyTy = typeVar list * monoTy  (* the list is for quantified variables *)

let string_of_polyTy (bndVars, t) = match bndVars with [] -> string_of_monoTy t
    | _ ->  (List.fold_left
             (fun s v -> s ^ " " ^ string_of_typeVar v)
             "Forall"
             bndVars)
             ^ ". " ^ string_of_monoTy t

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
   TrueConst | FalseConst -> (([], bool_ty):polyTy)
 | IntConst n -> ([], int_ty)
 | FloatConst f -> ([], float_ty)
 | StringConst s -> ([], string_ty)
 | NilConst -> ([0],mk_list_ty (TyVar 0))
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

(* environments *)
type 'a env = (string * 'a) list

let freeVarsEnv l = delete_duplicates (
    List.fold_right (fun (_,pty) fvs -> freeVarsPolyTy pty @ fvs) l [])

let string_of_seq string_of_elt lbrack sep rbrack seq =
  let rec string_of_seq_aux seq =
    match seq
    with [] -> ""
      | (x::xs) -> sep^" "^(string_of_elt x)^(string_of_seq_aux xs)
  in match seq with [] -> lbrack ^ rbrack
    | x :: xs -> lbrack^(string_of_elt x)^(string_of_seq_aux xs)^rbrack

let string_of_env string_of_entry mapsto_sym sep gamma = 
  string_of_seq
    (fun (key,value) -> (key ^ " "^ mapsto_sym ^ " " ^ string_of_entry value))
    "{" sep "}"
    gamma

(*
  let rec string_of_env_aux gamma =
    match gamma with
       []        -> ""
     | (x,y)::xs -> x ^ " "^ mapsto_sym ^ " " ^ string_of_entry y^
                    match xs with [] -> ""
                      | _  -> sep ^ " " ^ string_of_env_aux xs
  in
    "{"^ string_of_env_aux gamma^ "}"
*)

let string_of_type_env gamma = string_of_env string_of_polyTy ":" "," gamma

(*environment operations*)
let rec lookup mapping x =
  match mapping with
     []        -> None
   | (y,z)::ys -> if x = y then Some z else lookup ys x

type type_env = polyTy env

let make_env x y = ([(x,y)]:'a env)
let lookup_env (gamma:'a env) x = lookup gamma x
let ins_env (gamma:'a env) x y = 
    ((x,y)::(List.filter (fun (u,v) -> not(x = u)) gamma))
let sum_env (delta:'a env) (gamma:'a env) = 
    List.fold_right (fun (x,y) env -> ins_env env x y) delta gamma
(*
let sum_env (delta:'a env) (gamma:'a env) = ((delta@gamma):'a env)
let ins_env (gamma:'a env) x y = sum_env (make_env x y) gamma
*)

(* This should not be needed if the above are used
let compact_env m =
  let rec comp rev_m comp_m =
      (match rev_m with [] -> comp_m
        | (x,y) :: m' ->
           if List.exists (fun (x',_) -> x = x') rev_comp_m
              then comp m' rev_comp_m
           else comp m' ((x,y)::rev_comp_m))
  in comp (List.rev m) []
*)

(*judgment*) 
type judgment =
   ExpJudgment of type_env * exp * monoTy
 | DecJudgment of type_env * dec * type_env

let string_of_judgment judgment =
  match judgment with ExpJudgment(gamma, exp, monoTy) ->
        string_of_type_env gamma ^ " |= "^ string_of_exp false exp ^
         " : " ^ string_of_monoTy monoTy
  | DecJudgment (gamma, dec, delta) ->
        string_of_type_env gamma ^ " |= "^ string_of_dec false dec ^
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
     | [(s,t)] ->  (string_of_monoTy s^ " --&gt; "^ string_of_monoTy t)
     | (s,t)::c' -> (string_of_monoTy s^ " --&gt; "^ string_of_monoTy t^
		     "; "^ aux c')
  in ("["^ aux c^ "]\n")

 
let string_of_substitution s =
  let rec aux s =
     match s with 
     | [] -> ""
     | [(i,t)] -> ((string_of_typeVar i)  ^ " --&gt; " ^ string_of_monoTy t)
     | (i,t)::s' -> (((string_of_typeVar i)  ^ " --&gt; ")^
                     string_of_monoTy t^ "; "^ aux s')
  in ("["^ aux s^ "]\n")


let niceInfer_exp gather_exp (gamma:type_env) exp = 
  let ty = fresh()
  in
  let result = 
    match gather_exp gamma exp ty with
     None ->
      (print_string("Failure: No type for expression: "^
       string_of_exp false exp^ "\n"^
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
       string_of_dec false dec^ "\n"^
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


(*********************************************)
(*                  values                   *)

type basic_value =
    UnitVal
  | TrueVal | FalseVal
  | IntVal of int
  | FloatVal of float
  | StringVal of string
  | Exn of int
                      
type memory = (string * value) list
and value = 
    BasicVal of basic_value
  | PairVal of value * value
  | ListVal of value list
  | Closure of string * exp * memory
  | RecVarVal of string * string * exp * memory 

(*
let make_mem x y = ([(x,y)]:memory)
let rec lookup_mem (gamma:memory) x =
  match gamma with
     []        -> raise (Failure ("identifier "^x^" unbound"))
   | (y,z)::ys -> if x = y then z else lookup_mem ys x
let sum_mem (delta:memory) (gamma:memory) = ((delta@gamma):memory)
let ins_mem (gamma:memory) x y = sum_mem (make_mem x y) gamma
*)

let string_of_basic_value v =
   match v with
    UnitVal           -> "()"
  | IntVal n          -> if n < 0 then ("~"^ string_of_int (abs n))
                         else string_of_int n 
  | FloatVal r        -> string_of_float r
  | TrueVal	      ->  "true"
  | FalseVal	      ->  "false"
  | StringVal s       ->  "\"" ^ String.escaped s ^ "\""
  | Exn n -> ( "(Exn "^ string_of_int n^  ")")

(*value output*)
let rec short_string_of_value is_html v =
  match v
  with BasicVal bv    -> string_of_basic_value bv
  | PairVal (v1,v2)   ->  "("^
                         short_string_of_value is_html v1^  ", "^
                         short_string_of_value is_html v2^
                          ")"
  | ListVal l         ->  "["^
                         (let rec pl = function
                              []     ->  "]"
                            | v::vl  -> short_string_of_value is_html v^
                                        (match vl with [] -> ""
                                          | _ -> "; " ^ pl vl)
                              in pl l)
  | Closure (x, e, m) -> (lt is_html)^"closure"^(gt is_html)
  | RecVarVal (f, x, e, m)  ->
    if is_html then "&lt;&lt; recursive closure &gt;&gt;"
    else "<<recursive closure>>"

let rec string_of_value is_html string_of_memory v =
  match v
  with BasicVal bv    -> string_of_basic_value bv
  | PairVal (v1,v2)   ->  "("^
                         string_of_value is_html string_of_memory v1^  ", "^
                         string_of_value is_html string_of_memory v2^
                          ")"
  | ListVal l         ->  "["^
                         (let rec pl = function
                              []     ->  "]"
                            | v::vl  -> string_of_value is_html string_of_memory v^
                                        (match vl with [] -> ""
                                          | _ -> "; " ^ pl vl)
                              in pl l)
  | Closure (x, e, m) -> 
     ((lt is_html) ^ x ^ " -"^(gt is_html)^" " ^ string_of_exp is_html e ^ ", " ^ string_of_memory is_html m ^ (gt is_html))
  | RecVarVal (f, x, e, m)  ->
     ((if is_html then "&lt;&lt; " else "<<") ^ f ^ " " ^ x ^ " -"^(gt is_html)^" " ^ string_of_exp is_html e ^ ", "
      ^ string_of_memory is_html m ^  (if is_html then " &gt;&gt; " else ">> "))

(*memory output*)
let rec string_of_memory is_html m =
    string_of_env (string_of_value is_html string_of_memory) ("-"^(gt is_html)) "," m

(* Pervasive memory *)

(* NOTE: below not changed in 2015 *)
(*
let pervasive_memory =
  let bi s = (s, BuiltInOpVal s) in
  let perv1 =
   List.map bi
   ["+"; "-"; "*"; "/"; "<"; ">"; "<="; ">="; "mod"; "div";
    "+."; "-."; "*."; "/."; "**"; "::"; "head"; "tail"; "="; "^";
     "fst"; "snd"]
  in
  let and_val = Closure ("p",
   IfThenElse (App (Id "fst", Id "p"), App (Id "snd", Id "p"), Bool false),
   [("fst", BuiltInOpVal "fst"); ("snd", BuiltInOpVal "snd")]) in
  let or_val = Closure ("p",
   IfThenElse (App (Id "fst", Id "p"), Bool true, App (Id "snd", Id "p")),
   [("fst", BuiltInOpVal "fst"); ("snd", BuiltInOpVal "snd")]) in
  let not_val = Closure ("b", IfThenElse (Id "b", Bool false, Bool true), [])
  in
("and", and_val) ::
("or", or_val) ::
("not", not_val) ::
("nil", ListVal []) ::
perv1;;
*)

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
    HdOp, ListVal []        -> BasicVal (Exn 0)
  | TlOp, ListVal []        -> BasicVal (Exn 0)
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
  | GreaterOp,_,_  -> (match (v1 > v2) with true -> BasicVal TrueVal | false -> BasicVal FalseVal)
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
  ) with Division_by_zero -> BasicVal (Exn 0)

let rec eval_exp (exp, m) = 
  match exp with 
    ConstExp t -> const_to_val t
  | VarExp x   -> (
    match lookup_env m x with
      Some (RecVarVal(f, y, e, m'))
        -> Closure(y, e, ins_env m' f (RecVarVal(f, y, e, m')))
    | Some v -> v
    | None -> raise (Failure ("identifier "^x^" unbound"))
    )
  | BinOpAppExp(binop, e1, e2) -> (
    match eval_exp (e1, m) with
      BasicVal (Exn i) -> BasicVal (Exn i)
    | v1    ->
      match eval_exp (e2, m) with
        BasicVal (Exn i) -> BasicVal (Exn i)
      | v2    -> binOpApply binop (v1, v2) 
    ) 
  | MonOpAppExp(monop, e1) -> (
    match eval_exp (e1, m) with
      BasicVal (Exn i) -> BasicVal (Exn i)
    | v1    -> monOpApply monop v1
    )
  | AppExp(e1, e2) -> (
    match eval_exp (e1, m) with
      BasicVal (Exn i) -> BasicVal (Exn i)
    | v1    -> 
      match eval_exp (e2, m) with
        BasicVal (Exn i) -> BasicVal (Exn i)
      | v2 ->
        match v1, v2 with
        | Closure (x, e', m'), v' -> eval_exp (e', ins_env m' x v') 
        | _ -> failwith "eval_exp: case not handled"
    )
  | LetInExp(x, e1, e2) -> (
    match eval_exp (e1, m) with
      BasicVal (Exn i) -> BasicVal (Exn i)
    | v     -> eval_exp (e2, ins_env m x v)
    )
  | LetRecInExp(f, x, e1, e2) -> (eval_exp (e2, ins_env m f (RecVarVal(f, x, e1, m))))
  | FunExp(x, e) -> Closure (x, e, m)
  | IfExp(e1, e2, e3) -> (
    match eval_exp (e1, m) with
      BasicVal (Exn i)         -> BasicVal (Exn i)
    | BasicVal TrueVal       -> eval_exp(e2, m)
    | BasicVal FalseVal      -> eval_exp(e3, m)
    | _ -> failwith "eval_exp: bad arguments for IfExp"
  )
  | RaiseExp e -> (
    match eval_exp (e, m) with
      BasicVal (Exn i)    -> BasicVal (Exn i)
    | BasicVal (IntVal i) -> BasicVal (Exn i)
    | _        -> failwith "eval_exp: raise called on non-int"
    )
  | TryWithExp(e1, intopt1, exp1, match_list) -> (
    match eval_exp (e1, m) with
      BasicVal (Exn i) -> (
      try let e = snd(List.find (function (Some j,_) -> j=i | (None,_) -> true)
                     ((intopt1, exp1)::match_list))
          in eval_exp (e, m)
      with Not_found -> BasicVal (Exn i) 
      )
    | v     -> v
  )
(*  | _ -> failwith "eval_exp: case not handled" *)

let eval_dec (dec, m) =
  match dec with
    Anon e -> ((None, eval_exp (e, m)), m)
  | Let(x, e) -> (
    match eval_exp (e, m) with
      BasicVal (Exn i) -> ((None, BasicVal (Exn i)), m)
    | v     -> ((Some x, v), ins_env m x v)
    )
  | LetRec(f, x, e) -> ((Some f, RecVarVal(f, x, e, m)), ins_env m f (RecVarVal(f, x, e, m)))
(*  | _ -> failwith "eval_dec: case not handled"*)


(* I need a set of functions to canonicalize memories and closures.  I
   need a data type of memories and one of closures marking where and
   how things are wrong.  I need to be able to "pretty print"
   annotated memories and closures as html strings.  I need a way to
   give incremental points.
*)


let rec canonicalize_memory mem = 
  List.fold_right
    (fun (var,value) -> fun canon_mem ->
      List.merge
        (fun (v1,_) -> fun (v2,_) -> String.compare v1 v2)
        [(var, canonicalize_value value)]
        canon_mem)
    mem
    []
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

(*
let rec string_of_annotated_memory_aux ann_mem =
  match ann_mem
  with CorrectMem mem ->
        ("<good>"^(string_of_memory mem)^"</good>\n")
    | MissingEntry (v,an_mem) ->
      ("<missingentry varneeded="^v^">\n"^
          (string_of_annotated_memory an_mem) ^"</missingentry>\n")
    | ExtraEntry (var, values, an_mem) ->
      ("<extraentry extravar="^var^" values="^
          (string_of_seq string_of_value is_html "(" " |" ")" values)^">\n"
          (string_of_annotated_memory an_mem) ^"</extraentry>\n")
    | DuplicateEntries (var, good_value, an_values,an_mem) ->
      ("<duplicateentries expvalue="^(string_of_value good_value)^
          " givenvalues="^
          (string_of_seq string_of_annotated_value "(" " |" ")" an_values)^
          ">\n"^
          (string_of_annotated_memory an_mem) ^"</duplicateentries>\n")
    | WrongEntryValue (var, good_value, an_value, an_mem) ->
      ("<wrongentryvalue var="^var^" expvalue="^(string_of_value good_value)^
          " givenvalue="^(string_of_annotated_value an_value)^
          ">\n"^
          (string_of_annotated_memory an_mem) ^"</wrongentryvalue>\n")
    | CorrectEntryValue (var, good_value, an_mem) ->
      ("<correctentryvalue var="^var^" value="^(string_of_value good_value)^
          ">\n"^
          (string_of_annotated_memory an_mem) ^"</correctentryvalue>\n")
and string_of_annotated_memory ann_mem = string_of_annotated_memory_aux ann_mem
and string_of_annotated_value an_mem =
  match an_mem
  with CorrectValue value -> 
        ("<good>"^(string_of_value value)^"</good>\n")
  | WrongBasicValue basic_value ->
    ("<wrongbasicvalue>"^(string_of_basic_value basic_value)^
     "</wrongbasicvalue>\n")
  | WrongValueSort value ->
    ("<wrongvaluesort>"^(string_of_value value)^"</wrongvaluesort>\n")
  | PairContainsError (an_value1, an_value2) ->
    ("<paircontainserror>("^(string_of_annotated_value an_value1)^", "^
        (string_of_annotated_value an_value2)^")</paircontainserror>\n")
  | ListContainsError an_values ->
    "<listcontainserror>"^(string_of_seq string
  | WrongClosure of (string*marker) * (exp*marker) * annotated_memory
  | WrongRecClosure of (string*marker) * (string*marker) * (exp*marker) * annotated_memory
*)
        


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

(*
let rec check_first_memory_against_second fst_mem snd_mem =
  let fst_mem_multifn = mem_multifun fst_mem in
  let canon_snd_mem = canonicalize_memory snd_mem in
  match (fst_mem_multifn, canon_snd_mem)
  with ([],[]) -> CorrectMem([])
    | ((key1, values1), 
*)
