
open Genutils
open Gencheck
open Solution

  (*
    #############################################
      PROBLEM SPECIFICATION + RELEVANT DATATYPES
    #############################################
  *)

  (* target datatype for turning parsed student input into terms that the checker can work over *)

type ('obj, 'source) term_parse_error =
	JudgeParse of ('obj, 'source) tree_term
	| JudgeErr of unit parsed list

type ('obj, 'source) term_parse_elist =
  JParseList of ('obj, 'source) tree_term list
  | JErrList

  (* attempts to turn a list of JudgeParse elements into a JParseList. *)

let rec term_list_collect (sl: ('obj, 'source) term_parse_error list): ('obj, 'source) term_parse_elist = match sl with
  | [] -> JParseList []
  | (JudgeParse jt) :: st -> (match term_list_collect st with
    | JParseList l -> JParseList (jt :: l)
    | JErrList -> JErrList
  )
  | (JudgeErr _) :: st -> (match term_list_collect st with
    | JParseList _ -> JErrList
    | JErrList -> JErrList
  )

  (*
    target datatype for defining the problem, if you define a record containing all of these fields,
    you should be able to come up with a new proof tree problem

    'judgment -> the datatype returned by the parser (including cases for parse errors)
    'obj -> datatype for values that cannot be represented as terms (environments, etc)
    'source -> used in tree_term to allow for debug information to be printed out

    judgment_to_term: transforms the parser result into a tree term, tracking errors if they show up
    rule_list: the list of rules for term checking
    eq_poly_fun: decides when two objects in a term are equivalent. if a term has more than one kind of object,
      the equality function is selected using a string
    string_of_judgment: allows a judgment to be printed
    string_of_cons: designates english language names for the term constructors
  *)

type ('judgment, 'obj, 'source, 'sc_error) problem_def =
	{
    process: unprocessed_node -> 'judgment; 
		judgment_to_term: 'judgment -> ('obj, 'source) term_parse_error;
    sc_val: string -> ('obj, 'source) term_parse_error;
		rule_list: (int * ('obj, 'source, 'sc_error) syntax_rule) list;
		eq_poly_fun: string -> ('obj, 'source) tree_term -> ('obj, 'source) tree_term -> bool;
    string_of_judgment: 'judgment -> string;
    string_of_cons: string -> string;
    string_of_source: 'source -> string;
    string_of_failed_sc: string -> string
	}

  (*
    ######################################################
      PROCESSING OF STUDENT DATA INTO A TREE OF JUDGMENTS
    ######################################################
  *)

	(* raw node id, label, judgment, side condition *)
type 'judgment raw_tree = (string * (string * 'judgment * string)) list

type 'judgment proof_tree = ProofTree of 'judgment proof_tree list * string * 'judgment * string

(*
  reconstruction of the proof tree from a list of each proof node
*)

(* checks if s1 is contained in s2 ("ab" "abc") *)

let string_prefix s1 s2 =
  if String.length s1 > String.length s2 then false
  else String.sub s2 0 (String.length s1) = s1

let rec parent_check sx (s, po) =
  if string_prefix sx s && s <> sx then match po with
    | None -> (s, Some(sx))
    | Some p ->
      if string_prefix p sx then (s, Some(sx))
      else (s, po)
  else (s, po)

  (* raw node list ==> map of parents *)

let processed_tree_to_parent_env (plist: 'judgment raw_tree): (string option) env =
  let rec pttpe_rec plist p_env = match plist with
      [] -> p_env
    | (s, _) :: p_t -> pttpe_rec p_t (List.map (parent_check s) p_env)
  in pttpe_rec plist (List.map (fun (s, _) -> (s, None)) plist)

let rec find_node_with_parent p_env n = match p_env with
  | [] -> None
  | (s, v) :: p_t -> if v = n then Some (s, p_t) else
      (match find_node_with_parent p_t n with
	  None -> None
	| Some (s', p_t') -> Some (s', (s, v) :: p_t')
      )

    (* map of parents ==> string tree *)

type string_tree = StringTree of string * string_tree list

let parent_env_to_string_tree (p_env: (string option) env): string_tree =
  let rec petst_rec p_env parent child_list = match find_node_with_parent p_env parent with
    | None -> (child_list, p_env)
    | Some (child, p_env2) ->
      let (c_child_list, p_env3) = petst_rec p_env2 (Some child) []
      in petst_rec p_env3 parent (StringTree(child, c_child_list) :: child_list)
  in let (root_list, p_rem) = petst_rec p_env None []
     in match root_list with
       | [] -> raise (Failure "BUG: interface - No root of tree provided for processing. (proof tree step)")
       | pt :: [] -> (match p_rem with
	   | [] -> pt
	   | _ -> raise (Failure "BUG: interface - Other unrelated nodes exist.")
       )
       | _ -> raise (Failure "BUG: interface - Incomplete tree, root has siblings.")

    (* raw node list + string tree ==> proof tree *)

let string_tree_comp (StringTree(s1, _)) (StringTree(s2, _)) = compare s1 s2

let rec string_tree_to_proof_tree (plist: 'judgment raw_tree) (StringTree(s, sl)): 'judgment proof_tree = match lookup_env plist s with
	None -> raise (Failure "BUG: interface - String tree includes elements without data.")
	| Some (lbl, judgment, sc_str) ->
		let sorted_list = List.sort string_tree_comp sl
		in let child_list = List.map (string_tree_to_proof_tree plist) sorted_list
		in ProofTree(child_list, lbl, judgment, sc_str)

	(* raw node list ==> proof tree *)

let list_to_tree (plist: 'judgment raw_tree): 'judgment proof_tree =
	let p_env = processed_tree_to_parent_env plist
	in let st = parent_env_to_string_tree p_env
	in string_tree_to_proof_tree plist st


	(* execution of proof tree reconstruction + grading *)

let recon_proof_tree (tree: 'judgment raw_tree): 'judgment proof_tree = list_to_tree tree (*(processed_tree tree)*)

  (*
    #############################
     THE MAIN PROOF TREE CHECKER
    #############################
  *)

  (* selects all relevant rules from the rule list *)
let select_rule_list pdef t = match t with
  TermElem("judgment", (TermElem(x, _, _)) :: _, _) ->
    let rec td_find_rule rule_list = match rule_list with
      | [] -> []
      | (pts, SynRule(l_g, n, p, pl, sc)) :: rule_t ->
        if x = n || n = "any" then (l_g, SynRule(l_g, n, p, pl, sc)) :: td_find_rule rule_t else td_find_rule rule_t
    in td_find_rule pdef.rule_list
  | _ -> raise (Failure "BUG: interface - type derivation check being performed on non-judgment")

  (* old: selects a rule from the rule list *)
  (*
let select_rule pdef t = match t with
	TermElem("judgment", (TermElem(x, _, _)) :: _, _) ->
		let rec td_find_rule rule_list = match rule_list with
			| [] -> raise (Failure "BUG: interface - type derivation check being performed on syntax without a rule")
			| (pts, SynRule(l_g, n, p, pl, sc)) :: rule_t ->
				if x = n then (l_g, SynRule(l_g, n, p, pl, sc)) else td_find_rule rule_t
		in td_find_rule pdef.rule_list
	| _ -> raise (Failure "BUG: interface - type derivation check being performed on non-judgment")
  *)

	(* node error consists of term error list, rule name (if known), and the judgment + its subterms *)
type ('judgment, 'obj, 'source, 'sc_error) node_error =
  | NodeErr of
      (('obj, 'sc_error, 'source) term_match_error) list *
        string option * ('judgment list * 'judgment)


let rec check_rule pdef (lbl, j_term, sub_term_list, sc_val) (rule_name, c_rule) =
  let c_err = match_rule c_rule [("sc", [sc_val])] pdef.eq_poly_fun (lbl, j_term, sub_term_list)
  in (rule_name, c_err)

let rec get_shortest_list table = match table with
  | [] -> raise (Failure "BUG: interface - proof tree check applied to term for which no rule is applicable")
  | x :: [] -> x
  | (rule_name, l) :: t ->
    let (rule_name', l') = get_shortest_list t
    in if List.length l' <= List.length l then (rule_name', l') else (rule_name, l)

let check_rule_list pdef (lbl, j_term, sub_term_list, sc_val) rule_list =
  get_shortest_list (List.map (check_rule pdef (lbl, j_term, sub_term_list, sc_val)) rule_list)

  (* collects errors from a proof tree *)
let rec check_tree (pdef: ('judgment, 'obj, 'source, 'sc_error) problem_def)
	(ProofTree(pl, lbl, judgment, sc_str): 'judgment proof_tree): ('judgment, 'obj, 'source, 'sc_error) node_error list =
  let sub_plist = List.map (fun (ProofTree(_, _, j, _)) -> j) pl
  in let sub_jlist = List.map pdef.judgment_to_term sub_plist
  in let (jt, sub_tlist, psc) = (pdef.judgment_to_term judgment, term_list_collect sub_jlist, pdef.sc_val sc_str)
  in match (jt, sub_tlist, psc) with
    | (JudgeParse j_term, JParseList sub_term_list, JudgeParse sc_val) ->
      let rule_list = select_rule_list pdef j_term
      in let (rule_name, c_err) = check_rule_list pdef (lbl, j_term, sub_term_list, sc_val) rule_list
      in [NodeErr(c_err, Some rule_name, (sub_plist, judgment))] @ (check_tree_list pdef pl)
      (*let (rule_name, c_rule) = select_rule pdef j_term
      in let c_err = match_rule c_rule [] pdef.eq_poly_fun (lbl, j_term, sub_term_list)
      in [NodeErr(c_err, Some rule_name, (sub_plist, judgment))] @ (check_tree_list pdef pl)*)
    | (JudgeErr j_err, _, _) -> [NodeErr([TParseError j_err], None, (sub_plist, judgment))]
    | (_, _, JudgeErr sc_err) ->
      let j_err = [TScParseError sc_str]
      in [NodeErr(j_err, None, (sub_plist, judgment))]
    | (JudgeParse j_term, JErrList, _) ->
      let j_err = [TSubParseError (string_of_tree_src pdef.string_of_source j_term)]
      in [NodeErr(j_err, None, (sub_plist, judgment))]
and check_tree_list pdef pl (*rule_list*) = match pl with
	[] -> []
	| p :: p_t -> (check_tree pdef p) @ (check_tree_list pdef p_t)
;;

  (* combines with the proof tree reconstruction step *)
let test_result (pdef: ('judgment, 'obj, 'source, 'sc_error) problem_def)
	(tree: unprocessed): ('judgment, 'obj, 'source, 'sc_error) node_error list =
    let proc_tree = List.map (fun (key, node) -> (key, (node.str_label, pdef.process node, node.str_sideCondition))) tree
    in check_tree pdef (recon_proof_tree proc_tree);;

  (*
    #############################
     GRADER OUTPUT FUNCTIONALITY
    #############################
  *)

  (* functions related to grading the labels *)

let get_points_from_label pdef lbl =
  let rec gpfl_rec rule_list = match rule_list with
    | [] -> 0
    | (pts, SynRule(l, _, _, _, _)) :: rule_t ->
      if l = lbl then pts else gpfl_rec rule_t
  in gpfl_rec pdef.rule_list

let rec exp_points_from_label_list pdef label_list = match label_list with
  | [] -> 0
  | lbl :: label_t -> (get_points_from_label pdef lbl) + (exp_points_from_label_list pdef label_t)

let rec take_from_label_list label_list l = match label_list with
  | [] -> None
  | lbl :: label_t ->
    if l = lbl then Some label_t
    else (match take_from_label_list label_t l with
      | None -> None
      | Some lt' -> Some (lbl :: lt')
    )

  (* points calculation related functions *)

type possible_points =
  PRuleValue of string * int
  | PRuleOverused of string
  | PNoParse

let points_of_node_error pdef (NodeErr(c_err, rule_sel, _)) rem_lbl_list =
  let err_count = List.length c_err
  in match rule_sel with
    | None -> (PNoParse, 0, rem_lbl_list)
    | Some rule_name -> (match take_from_label_list rem_lbl_list rule_name with
      | None -> (PRuleOverused rule_name, 0, rem_lbl_list)
      | Some rll' -> (PRuleValue(rule_name, get_points_from_label pdef rule_name), err_count, rll')
    )


let string_of_error pdef e =
  string_of_term_match_error pdef.string_of_source pdef.string_of_cons pdef.string_of_failed_sc e;;

let string_of_error_list pdef c_err pts_possible pts_lost =
  let (status_start, pts_worth) = match pts_possible with
    | PNoParse -> ("  No points given because rule could not be determined.", 0) 
    | PRuleOverused x -> ("  No points given because max credit for use of rule `" ^ x ^ "` has already been given.", 0)
    | PRuleValue(x, n) -> ("  Use of rule `" ^ x ^ "` is worth " ^ (string_of_int n) ^ " points.", n)
  in let capped_pts_lost = pts_worth - (max 0 (pts_worth - pts_lost))
  in if pts_worth > 0 && c_err = [] then ("  No errors!\n" ^ status_start, 0)
    else (string_of_list (status_start ^ "\n") ("\nPoints lost: "^(string_of_int capped_pts_lost)^"\n")
               "\n" (string_of_error pdef) c_err, capped_pts_lost)

let string_of_node_error pdef pts_possible pts_lost (NodeErr(c_err, _, (antecedents, ts))) =
  let sc_string = "" (* match sc with
      ParseEmpty -> ""
      | ParseOk [] -> ""
      | _ -> ("SC: "^(string_of_parsed string_of_substitution sc)^"\n") *)
  in let (err_str, capped_pts_lost) = string_of_error_list pdef c_err pts_possible pts_lost
  in (("Inference: \n"
    ^ (snd (List.fold_left
              (fun (n, str) -> fun tyst ->
                (n+1, (str^(string_of_int n)^". "^(pdef.string_of_judgment tyst) ^ "\n")))
              (1,"")
              antecedents))
    ^ sc_string
    ^ "---------------------------------------------------------------------------\n"
    ^ "  " ^ (pdef.string_of_judgment ts) ^ "\n"
    ^ "Status: \n" ^ err_str ^ "\n"), capped_pts_lost)    

let rec points_of_unused_rules pdef rem_lbl_list = match rem_lbl_list with
  | [] -> 0
  | rem_lbl :: rem_lbl_t -> (get_points_from_label pdef rem_lbl) + (points_of_unused_rules pdef rem_lbl_t)

let string_of_unused_rules pdef rem_lbl_list =
  if rem_lbl_list = [] then ""
  else string_of_list "Other: \n" "\n" "" (fun rem_lbl ->
    "Correct solution has use of rule `" ^ rem_lbl ^ "`" ^ " not present here worth " ^
      (string_of_int (get_points_from_label pdef rem_lbl)) ^ " points.\n"
  ) rem_lbl_list

let string_of_test_res pdef label_list error_list =
let max_points = exp_points_from_label_list pdef label_list
  (* returns (string to print, points lost, remaining labels to use up) *)
in let rec str_of_test_res rem_lbl_list error_list = match error_list with
  [] -> ("", 0, rem_lbl_list)
  | node_err (*(NodeErr(c_err, pts, (antecedents, ts, sc)))*) :: error_t ->
    let (pts_possible, pts_lost, rll1) = points_of_node_error pdef node_err rem_lbl_list
    in let (node_err_str, capped_pts_lost) = string_of_node_error pdef pts_possible pts_lost node_err
    in let (rec_err_str, rec_pts_lost, rll2) = str_of_test_res rll1 error_t
    in ((node_err_str ^ "\n\n" ^ rec_err_str), capped_pts_lost + rec_pts_lost, rll2)
in let (err_report, pts_lost, rem_lbl_list) = str_of_test_res label_list error_list
in let pts_lost_rem = points_of_unused_rules pdef rem_lbl_list
in let err_end = string_of_unused_rules pdef rem_lbl_list
in let score = max_points - pts_lost - pts_lost_rem
in err_report ^ err_end ^ "Total: [" ^ (string_of_int score) ^ " / " ^ (string_of_int max_points) ^ "]\n";;

let grade (pdef: ('judgment, 'obj, 'source, 'sc_error) problem_def)
  (tree: unprocessed): unit =
  print_string (string_of_test_res pdef Solution.sol_label_list (test_result pdef tree));;


(*let x_grade pdef = grade *)