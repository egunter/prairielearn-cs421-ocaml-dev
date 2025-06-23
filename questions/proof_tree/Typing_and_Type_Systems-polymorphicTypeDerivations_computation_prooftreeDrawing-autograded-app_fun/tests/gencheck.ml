(* File: genutils.ml *)
(* Author: John Lee and Elsa L Gunter *)
(* Copyright 2017 *)


open Genutils

(* syntax_pat: used to define general syntax rules, where elem is a
   constructor, and var is used to perform pattern matching *)
type syntax_pat =
	PatElem of string * syntax_pat list
	| PatVar of string

	(* tree_term: the actual terms we are matching over *)
type ('a, 'source) tree_term =
	TermElem of string * (('a,'source) tree_term) list * 'source
  | TermObj of 'a * 'source

let source_of_tree_term tr =
  match tr
  with TermElem (_,_,src) -> src
    | TermObj (_, src) -> src

let string_of_tree_src string_of_src tr = string_of_src (source_of_tree_term tr)

type ('a, 'source, 'sc_error) sc_pred =
    SCPred of string * ((('a, 'source) tree_term) env -> 'sc_error option)

let string_of_sc_pred (SCPred (s,p)) = s

	(* syntax_rule: essentially defines a single rule from a given proof system *)
type ('a, 'source, 'sc_error)  syntax_rule =
    SynRule of
        string * string * syntax_pat * syntax_pat list *
        ('a, 'source, 'sc_error)  sc_pred list

type ('a, 'sc_error, 'source) term_match_error =
	TBadConsName of string * string * 'source
	| TMismatch of string * ('a, 'source) tree_term * ('a, 'source) tree_term
	| TMisplacedObj of ('a * 'source) * string
	| TWrongArity
	| TFailedSC of string * 'sc_error
	| TBadLabel of string * string
	| TParseError of (unit parsed) list 
	| TSubParseError of string
	| TScParseError of string

	(* syntactic sugar for constructors where we want to match all of their children *)

let source_opt_of_term_match_error source_of_sc_err tme =
  match tme
  with TBadConsName (_,_,src) -> Some src
    | TMismatch (_, t, p) -> Some (source_of_tree_term t)
    | TMisplacedObj ((_, src), _) -> Some src
    | TWrongArity -> None
    | TFailedSC (sc_label, sc_err) -> source_of_sc_err sc_err
    | TBadLabel _ -> None
    | TParseError _ -> None
    | TSubParseError s -> None
    | TScParseError _ -> None

let string_of_term_match_error string_of_source string_of_cons string_of_failed_sc e =
match e with
    TBadConsName(x, y, src) ->
      "Use of *" ^ (string_of_cons x) ^ "* at `" ^ (string_of_source src)
      ^ "` when rule expected *" ^ (string_of_cons y) ^ "*"
  | TMismatch(x, t, p) ->
    "Inconsistency of `" ^ x ^ "`, "^(string_of_source(source_of_tree_term t))
    ^" and "^(string_of_source(source_of_tree_term p))
  | TMisplacedObj((_,src), x) ->
    "Failure to match `" ^ x ^ "` at all uses in "^(string_of_source src)
  | TWrongArity -> "Wrong number of sub proofs"
	(* right now, this part is type derivation specific *)
  | TFailedSC (sc_label, err) ->
  	string_of_failed_sc sc_label err
    (*"Failed side condition "^sc_label^": "^(string_of_failed_sc err)*)
  | TBadLabel(l_bad, _) ->
  	if l_bad = "" then "No rule label was selected from drop-down menu."
    else "Rule `" ^ l_bad ^ "` label selected from drop-down menu is incorrect."
  | TParseError p ->
    string_of_list "" "" "; \n" (string_of_parsed (fun () -> "No parse error")) p
  | TSubParseError s -> ("Error in parse of a subterm: "^s)
  | TScParseError s -> ("Error in parse of side condition: "^s)

let pat_cons s sl = PatElem(s, List.map (fun s' -> PatVar s') sl)

(* peripheral functions *)

(* 
   a series of functions, the main one of which is 'match_term'. their purpose is to collect a list of
   all errors matching a term to a pattern. (an empty list representing a correct matching.)
   
   this is performed in two steps, the 'match_term_collect' step which checks for proper constructor
   usage, as well as a 'match_unify' step, which checks that all instances of terms matched to the
   same binding are all equivalent (ie 'tau1' is the same in all uses).
   
   'eq_poly_fun' is a set of equality functions being passed in to account for cases where the
   "type" affects the notion of equality. (for instance, in order to check equality for 'gamma'
   matchings, we'd like to convert it from a generic term into an actual environment, since
   environment equality ought to be commutative).
*)

let rec match_term_collect t p env = match (t, p) with
	| (_, PatVar x) -> (match lookup_env env x with
		| None -> ([], add_env env x [t])
		| Some other -> ([], add_env env x (t :: other))
	)
	| (TermElem(x, pars, source), PatElem(x', args)) ->
		if x <> x' then ([TBadConsName(x, x',source)], env)
		else match_term_zip (zip_list_list pars args) env
	| (TermObj(v,source), PatElem(x, _)) -> ([TMisplacedObj((v,source), x)], env)
and match_term_zip l env = match l with
	| [] -> ([], env)
	| (t, p) :: t_t ->
		let (err_l1, env2) = match_term_collect t p env
		in let (err_l2, env3) = match_term_zip t_t env2
		in (err_l1 @ err_l2, env3)

let rec zip_term_pat tl pl = match (tl, pl) with
	| ([], []) -> ([], [])
	| (t :: t_t, p :: p_t) ->
		let (err, res) = zip_term_pat t_t p_t
		in (err, (t, p) :: res)
	| _ -> ([TWrongArity], [])


let rec match_unify eq_poly_fun match_env = match match_env with
	| [] -> []
	| (x, mv :: ml) :: m_t ->
		(match_unify_list (eq_poly_fun x) x mv ml) @ (match_unify eq_poly_fun m_t)
	| (x, []) :: m_t -> raise (Failure "BUG: gencheck.ml - binding exists but doesn't match to any terms #1")
and match_unify_list eq_fun x mv ml = match ml with
	| [] -> []
	| mv' :: mt ->
		if eq_fun mv mv' then match_unify_list eq_fun x mv mt
		else (TMismatch(x, mv, mv')) :: (match_unify_list eq_fun x mv mt)

let rec collapse_env match_env = match match_env with
	| [] -> []
	| (x, []) :: _ -> raise (Failure "BUG: gencheck.ml - binding exists but doesn't match to any terms #2")
	| (x, mv :: _) :: m_t -> (x, mv) :: (collapse_env m_t)

let rec check_sc_list env sc_l = match sc_l with
	| [] -> []
	| SCPred(x, f) :: sc_t ->
          (match f env
           with None -> []
             | Some err -> (TFailedSC (x, err)) :: (check_sc_list env sc_t))
               (*
            ((SCPred(x, f))(*:('a, 'b) sc_pred*)) :: sc_t ->
          (if f env then []
           else [((TFailedSC x)(*:('a, 'b) term_match_error*))])
                @ (check_sc_list env sc_t)
               *)

(*
let match_term eq_poly_fun t p sc_l =
	let (err_env1, match_env) = match_term_collect t p []
	in let err_env2 = match_unify eq_poly_fun match_env
	in let err_env3 = if err_env2 = [] then check_sc_list (collapse_env match_env) sc_l else []
	in err_env1 @ err_env2 @ err_env3
*)

let match_rule (SynRule(l_g, x, p, pl, sc_l)) init_env eq_poly_fun (lbl, t, tl) =
	let (err_env1, term_pat_list) = zip_term_pat (t :: tl) (p :: pl)
	in let (err_env2, match_env) = match_term_zip term_pat_list init_env
	in let err_env3 = match_unify eq_poly_fun match_env
	in let err_env4 = if err_env2 = [] then check_sc_list (collapse_env match_env) sc_l else []
	in let err_env5 = if lbl = l_g then [] else [TBadLabel(lbl, l_g)]
	in err_env1 @ err_env2 @ err_env3 @ err_env4 @ err_env5

