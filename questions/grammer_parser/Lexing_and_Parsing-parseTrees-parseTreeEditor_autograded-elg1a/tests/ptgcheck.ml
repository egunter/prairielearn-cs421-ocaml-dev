
open Genset
open Genmap
open Ptgload
open Solution

	(* correct non-terminal productions, mistakes, fringe check, top level check*)
type score_result = ScoreRes of int * int * bool * bool


	(* functions for analyzing a tree and its correctness *)
	
let get_label (Tree(l, _)) = l

let valid_prod gram head child_list = match lookup_map gram head with
	| None -> Some (head, child_list)
	| Some valid_list ->
		if in_set (list_set valid_list (=)) child_list then None
		else Some (head, child_list)

	(* returns correct non-terminal productions and a set of mistakes made *)
let rec analyze_tree gram tree ml =
	let rec analyze_tree_list tl ml = match tl with
		| [] -> (0, ml)
		| c :: t ->
			let (p1, ml1) = analyze_tree gram c ml
			in let (p2, ml2) = analyze_tree_list t ml1
			in (p1 + p2, ml2)
	in match tree with
		| Tree(l, []) -> (0, ml)
		| Tree(l, child_list) -> let (p', ml') = analyze_tree_list child_list ml
			in (match valid_prod gram l (List.map get_label child_list) with
				| None -> (p' + 1, ml')
				| Some m -> (p', add_set ml' m)
			)

let rec get_fringe tree =
	let rec get_fringe_list tree_list = match tree_list with
		| [] -> ""
		| tr :: t -> (get_fringe tr) ^ (get_fringe_list t)
	in match tree with
		| Tree(l, []) -> l
		| Tree(l, trl) -> get_fringe_list trl

	(* code concerned with actually calculating a student's grade *)

let mistake_equality m1 m2 = m1 = m2

let perfect_score (Problem(gram, fringe, head, sol_no_parse)) (stu_no_parse, tree) =
  let Tree(l, _) = tree in
  let (p', ml) = analyze_tree gram tree (empty_set mistake_equality)
  in
  let stu_fringe = get_fringe tree
  in if size_set ml > 0 then None
    else if fringe = stu_fringe && head = l then Some p'
    else (print_string (" ~~ " ^ stu_fringe ^ "\n"); None)

