(* File: rrg.ml *)
(* Author: Elsa L Gunter *)
(* Copyright 2016 *)
(* Share and Enjoy *)

(* This file attempts to develop an equivalence test on right regular
   expressions. *)
let compare = ();;

(* Generic list as as set funcitons *)
let bool_compare b1 b2 =
  match (b1,b2)
  with (false, false) -> 0
    | (false, true) -> -1
    | (true, false) -> 1
    | (true, true) -> 0

let rec mem compare x l =
  match l with [] -> false | (y::ys) -> (compare x y = 0) || mem compare x ys

let pair_compare fst_compare snd_compare (a,b) (x,y) =
  match fst_compare a x
  with 0 -> snd_compare b y
    | c -> c

let rec list_compare compare l1 l2 =
  match (l1,l2)
  with ([],[]) -> 0
    | ([], _) -> -1
    | (_, []) -> 1
    | ((x::xs), (y::ys)) ->
      pair_compare compare (list_compare compare) (x,xs) (y,ys)

let rec insert_uniq compare x lst =
  match lst with [] -> [x]
    | y::ys ->
      let n = compare x y 
      in
      if n < 0 then x::lst
      else if n = 0 then x::ys (*prefer the inserted one*)
      else y::(insert_uniq compare x ys)

let partial_insert_uniq compare xopt lst =
  match xopt with Some x -> insert_uniq compare x lst
    | None -> lst

let sort_uniq compare lst = List.fold_right (insert_uniq compare) lst []

let set_minus compare lst1 lst2 =
  List.filter (fun x -> not(mem compare x lst2)) lst1

let set_diff compare lst1 lst2 =
  (set_minus compare lst1 lst2, set_minus compare lst2 lst1)

let set_union compare lst1 lst2 =
  List.fold_right (insert_uniq compare) lst1 lst2

let string_of_list lb sep rb string_of_elt lst =
  let rec mid_str_of_list l =
      match l with [] -> ""
        | [x] -> string_of_elt x
        | x :: xs -> string_of_elt x ^ sep ^ mid_str_of_list xs
  in lb^(mid_str_of_list lst)^rb

let assoc_compare compare (key1,val1) (key2,val2) = compare key1 key2

let update_assoc compare = insert_uniq (assoc_compare compare)

let rec sorted_to_assoc_list key_compare sorted_list =
  match sorted_list
  with [] -> []
    | (key,value)::more ->
        (match sorted_to_assoc_list key_compare more
         with [] -> [(key,[value])]
           | (((key',values)::rest) as result) ->
             if (key_compare key key' = 0)
             then (key,value :: values)::rest
             else (key,[value]):: result)

(*------------------------------------------------------------------*)

type terminal = T of string

let terminal_compare (T s1) (T s2) = String.compare s1 s2

let string_of_terminal (T s) = s

let string_of_path path =
  string_of_list "" " " "" string_of_terminal path

type alphabet = terminal list

type nonterminal = NT of string | DONE (*| DEAD*)

let nonterminal_compare nt1 nt2 =
  match (nt1,nt2)
  with (DONE,DONE) -> 0
    | (DONE, NT s) -> -1
    | (NT s, DONE) -> 1
    | (NT s1, NT s2) -> String.compare s1 s2

let string_of_nonterminal nt =
  match nt with NT s -> s
    | DONE -> "DONE_RRG"

(*
  To support viewing right regual grammars as NDFA, we view
  nonterminals as states in the NDFA, and add a "nameless" state
  DONE, which is a nameless accepting state.  Each production with a
  terminal is viewed as a labeled edge.  If the destination state is
  missing, then it is the nameless state DONE.  If the production has
  no terminal, then this is treated as a declaration that the
  left-hand nonterminal is an accepting state.
*)

type production =
    More of terminal * nonterminal | One of terminal | Epsilon

let production_compare p1 p2 =
  match (p1,p2)
  with (Epsilon, Epsilon) -> 0
    | (Epsilon, _) -> -1
    | (One tm, Epsilon) -> 1
    | (One tm1, One tm2) -> terminal_compare tm1 tm2
    | (One _ , _ ) -> -1
    | (More (tm1, nt1), More (tm2, nt2)) ->
      pair_compare terminal_compare nonterminal_compare (tm1, nt1) (tm2, nt2)
    | (More (_,_), _) -> 1

let string_of_production p =
  match p with More (tm,nt) ->
    ((string_of_terminal tm) ^" "^ (string_of_nonterminal nt))
    | One tm -> (string_of_terminal tm)
    | Epsilon -> "E"

type ext_production =
    ExtMore of terminal list * nonterminal | ExtOne of terminal list

let ext_production_compare p1 p2 =
  match (p1,p2)
  with (ExtOne ts1, ExtOne ts2) -> list_compare terminal_compare ts1 ts2
    | (ExtOne _, ExtMore _) -> -1
    | (ExtMore _, ExtOne _) -> 1
    | (ExtMore (ts1, nt1), ExtMore (ts2, nt2)) ->
       pair_compare (list_compare terminal_compare) nonterminal_compare
         (ts1, nt1) (ts2, nt2)

let string_of_ext_production p =
  match p with ExtMore (tms,nt) ->
    ((string_of_list "" " " " " string_of_terminal tms) ^
        (string_of_nonterminal nt))
    | ExtOne tms -> (string_of_list "" " " "" string_of_terminal tms)

let is_ext_production_production ext_prod =
  match ext_prod
  with ExtMore ([tm],nt) -> true | ExtMore _ -> false
    | ExtOne [] -> true | ExtOne [tm] -> true | ExtOne _ -> false

let ext_production_to_production ext_prod =
  match ext_prod
  with ExtMore ([tm],nt) -> More(tm,nt)
    | ExtMore _ ->
      raise (Failure "is_ext_production_production should have ruled this out")
    | ExtOne [] -> Epsilon | ExtOne [tm] -> One tm
    | ExtOne _ -> 
      raise (Failure "is_ext_production_production should have ruled this out")

type rrg_rule = (nonterminal * production)

let rule_compare = pair_compare nonterminal_compare production_compare

let string_of_rrg_rule (nt, p) =
  (string_of_nonterminal nt)^" ::= "^(string_of_production p)

type ext_rrg_rule = (nonterminal * ext_production)

let ext_rule_compare = pair_compare nonterminal_compare ext_production_compare

(* Time to convert an extended grammar into a right regular grammar *)
(* For a sequence of more than one terminal, introduce fresh nonterminals *)
(* For a empty sequence of terminals with a terminal, we must copy all the
   rules for the second into the first, deleting immeditate cycles, and recurse *)

type zero_or_one_prod =
    ZeroMore of nonterminal
  | OneMore of terminal * nonterminal
  | JustOne  of terminal
  | Zero

let zero_or_one_prod_compare zo1 zo2 =
  match (zo1, zo2)
  with ZeroMore nt1, ZeroMore nt2 -> nonterminal_compare nt1 nt2
    | ZeroMore _, _ -> -1
    | _, ZeroMore _ -> 1
    | Zero,Zero -> 0
    | Zero, _ -> -1
    | _, Zero -> 1
    | JustOne t1, JustOne t2 -> terminal_compare t1 t2
    | JustOne _, _ -> -1
    | _, JustOne _ -> 1
    | (OneMore (tm1, nt1), OneMore (tm2, nt2)) ->
      pair_compare terminal_compare nonterminal_compare (tm1, nt1) (tm2, nt2)

let rec ext_rule_to_zero_or_one_rule count ext_rule =
  match ext_rule
  with (nt0,ExtMore (tms, nt)) ->
    (match tms
     with [] -> ([(nt0,ZeroMore nt)], count)
       | [tm] -> ([(nt0, OneMore (tm,nt))], count)
       | (tm::more_tms) ->
         let nt1 = NT("%"^(string_of_int count)) in
         let (rules, new_count) =
           ext_rule_to_zero_or_one_rule (count +1) (nt1,ExtMore (more_tms, nt))
         in ((insert_uniq
                (pair_compare nonterminal_compare zero_or_one_prod_compare)
                (nt0,OneMore (tm, nt1))
                rules),
             new_count))
    | (nt0,ExtOne tms) ->
      (match tms
       with [] -> ([(nt0, Zero)], count)
         | [tm] -> ([(nt0, JustOne tm)], count)
         | (tm::more_tms) ->
           let nt1 = NT("%"^(string_of_int count)) in
           let (rules, new_count) =
             ext_rule_to_zero_or_one_rule (count +1) (nt1,ExtOne more_tms)
           in ((insert_uniq
                  (pair_compare nonterminal_compare zero_or_one_prod_compare)
                  (nt0,OneMore (tm, nt1))
                  rules),
               new_count))

let ext_rules_to_zero_or_one_rules ext_rules =
  let rec ext_rules_to_zo_rules count ext_rules =
    match ext_rules
    with [] -> []
      | ext_rule :: more_ext_rules ->
        (match ext_rule_to_zero_or_one_rule count ext_rule
         with (zo_rules, new_count) ->
           List.merge
             (pair_compare nonterminal_compare zero_or_one_prod_compare)
             zo_rules
             (ext_rules_to_zo_rules new_count more_ext_rules))
  in
  sorted_to_assoc_list nonterminal_compare (ext_rules_to_zo_rules 0 ext_rules)

(* Assumes ZeroMore nt0 not in nt0_ext_prods *)
let remove_indirection_from_productions nt0 nt0_ext_prods ext_prods =
  if List.exists (fun ep -> ep = ZeroMore nt0) ext_prods
  then set_union zero_or_one_prod_compare nt0_ext_prods
    (List.filter (fun ep -> not(ep = ZeroMore nt0)) ext_prods)
  else ext_prods

let remove_indirection_from_ext_rule nt0 nt0_ext_prods (nt1,nt1_ext_prods) =
  (nt1, remove_indirection_from_productions nt0 nt0_ext_prods nt1_ext_prods)

let remove_indirection_from_ext_rules (nt0,nt0_ext_prods) ext_rules =
  List.map (remove_indirection_from_ext_rule nt0 nt0_ext_prods) ext_rules

let remove_all_indirection_from_ext_prods rrg_rules ext_prods =
  List.filter (fun ep -> match ep with ZeroMore _ -> false | _ -> true)
    (List.fold_left
       (fun ext_prods (nt0, nt0_ext_prods) ->
         remove_indirection_from_productions nt0 nt0_ext_prods ext_prods)
       ext_prods
       rrg_rules)

let rec elim_ZeroMore rules =
  match rules
  with [] -> []
    | (nt0,ext_prods)::ext_rules ->
      let nt0_ext_prods =
        List.filter (fun ep -> not(ep = ZeroMore nt0)) ext_prods
      in
      let no_nt0_rules =
        remove_indirection_from_ext_rules (nt0,nt0_ext_prods) ext_rules
      in
      let rrg_rules =
        elim_ZeroMore no_nt0_rules
      in
      (nt0,(remove_all_indirection_from_ext_prods rrg_rules ext_prods))
      :: rrg_rules

(* Do I need this for anything? *)
let zero_or_one_prod_is_rr zo_prod =
    match zo_prod
    with ZeroMore nt -> false
      | _ -> true

let zero_or_one_prod_to_production zo_prod =
  match zo_prod
  with Zero -> Epsilon
    | JustOne tm -> One tm
    | OneMore (tm,nt) -> More (tm,nt)
    | ZeroMore _ ->
      raise (Failure ("elim_ZeroMore should have removed this case"))

let rec ext_rules_to_rr_rules ext_rules =
  let full_zu_rules = ext_rules_to_zero_or_one_rules ext_rules in
  let rr_zero_or_one_rules = elim_ZeroMore full_zu_rules in
  List.flatten
    (List.map (fun (nt,zo_prods) ->
      (List.map (fun zo_prod -> (nt, zero_or_one_prod_to_production zo_prod))
         zo_prods))
       rr_zero_or_one_rules)
  
type right_regular_grammar = rrg_rule list * nonterminal

let grammar_compare =
  pair_compare (list_compare rule_compare) nonterminal_compare

let string_of_grammar (rules, nt) =
  (string_of_list "" "\n" "\n" string_of_rrg_rule rules) ^
    ("Start Symbol = "^(string_of_nonterminal nt)^"\n")

type ext_right_regular_grammar = ext_rrg_rule list * nonterminal

let ext_rr_grammar_to_rr_grammar (ext_rules, nt) =
  ((ext_rules_to_rr_rules ext_rules, nt):right_regular_grammar)

let head_of_rule (nt, production) = nt

let terminal_of_production p =
  match p with  More (tm,_) -> Some tm
    | One tm -> Some tm
    | Epsilon -> None

let production_destination p =
  match p with More (_,nt) -> Some nt
    | One _ -> Some DONE
    | Epsilon -> None

let terminal_of_rule (nt,production) = terminal_of_production production

let next_nonterminal (nt,production) = production_destination production

let get_alphabet (rules:rrg_rule list) =
  ((List.fold_left
      (fun abc r ->
        partial_insert_uniq terminal_compare (terminal_of_rule r) abc)
      []
      rules):alphabet)

let is_accepting rules nonterminal =
  (nonterminal = DONE) || (mem rule_compare (nonterminal,Epsilon) rules)

(* Build a DFA *)

(* Assumed to always be sorted without duplicates *)
type node_name = nonterminal list

let node_name_compare = list_compare nonterminal_compare

let dead_name:node_name = []

(* Put the accepting state info into the node_state *)
type node_state = node_name * bool

let node_state_compare = pair_compare node_name_compare bool_compare

let dead_state = (dead_name,"false")

type outedge = (terminal * node_state)
let outedge_compare = pair_compare terminal_compare node_state_compare

type outedges = outedge list
let outedges_compare = list_compare outedge_compare

type node = node_state * outedges

(* full node_compare should not be used within a single graph, as the
   node_name compare should be enough.  node_compare shoudl only be
   part of comparing different graphs (which we don't actually ever
   do).  *)

let mk_dead_node (alphabet:alphabet) =
  (dead_state, List.map (fun terminal -> (terminal, dead_state)) (alphabet:alphabet))

type graph = node list
type dfa = graph * node_state
    
(*
   Start with a rrg_rule list, a start symbol, and an empty node list
   For each rule starting with the current nonterminal, for each
   terminal, gather the list of nonterminals to the right of a terminal
   to make a new node for that terminal to go to.  If there are any
   rrg_rules with the terminal but no nonterminal, then the node is
   marked as accepting, and otherwise not.
*)

let gather_destinations rules terminal startnonterminal (prev_dests, prev_acc) =
  match startnonterminal
  with DONE -> ((*insert_uniq compare DEAD *) prev_dests, prev_acc)
(*    | DEAD -> (insert_uniq compare DEAD prev_dests, prev_acc) *)
    | _ ->
      List.fold_left
        (fun ((dests:node_name),(is_acc:bool)) (r:rrg_rule) ->
          if head_of_rule r = startnonterminal &&
             terminal_of_rule r = Some terminal
          then
            (match next_nonterminal r
             with Some (dest:nonterminal) -> 
               (((insert_uniq nonterminal_compare dest dests):node_name),
                ((is_acc || (is_accepting rules dest)):bool))
               | (None:nonterminal option) -> (dests,is_acc))
          else (dests,is_acc))
        ((prev_dests:node_name), (prev_acc:bool))
        (rules:rrg_rule list)

let node_name_destination rules (nodename:node_name) terminal =
  ((terminal,
   List.fold_right (fun nt node -> gather_destinations rules terminal nt node)
     nodename
     ([], false)):outedge)

let make_node_edges rules (nodename:node_name) (alphabet:alphabet) =
  ((List.map (node_name_destination rules nodename) alphabet):outedges)

let generate_dfa_graph (alphabet:alphabet) (rules:rrg_rule list) (current_graph:graph)
    (node_states_to_visit: node_state list) =
(*  let alphabet = get_alphabet rules in*)
  let rec mk_graph (current_graph: graph)
      (node_states_to_visit: node_state list) =
      match node_states_to_visit
      with [] -> current_graph
        | ((nts,_)as node_state)::more_nodenames ->
          if List.exists (fun ((nts',_),_) -> nts' = nts) current_graph
          then mk_graph current_graph more_nodenames
          else
            let node_edges = make_node_edges rules nts alphabet in
            let nodes_to_do =
              List.fold_right
                (fun (tm, nd) ->
                  insert_uniq (pair_compare node_name_compare bool_compare) nd)
                node_edges
                more_nodenames
            in
            let new_graph =
              update_assoc node_state_compare
                (node_state,node_edges) current_graph
            in
            mk_graph new_graph nodes_to_do
  in mk_graph current_graph node_states_to_visit

let dfa_of_rrg (alphabet:alphabet)
    ((rules, start_symbol):right_regular_grammar) =
  let start_node = ([start_symbol],(is_accepting rules start_symbol)) in 
  (((generate_dfa_graph alphabet rules [] [start_node]), start_node):dfa)


let build_and_check_product quit_at_first_error
    ((graph1, start_state1):dfa) ((graph2, start_state2):dfa) =

  (* assume they have been built over the same alphabet *)
  (* Every edge should already be attached at its source to a node
     already in the product graph
  *)

  let rec prod product_graph not_in_1_errors_found  not_in_2_errors_found
      in_both (*current_path*) nodes_to_visit_with_access =
    match nodes_to_visit_with_access
    with [] -> (product_graph, not_in_1_errors_found, not_in_2_errors_found, in_both)
      | (prod_state,path) :: more_to_visit ->
        if List.exists (fun (ps,edges) -> ps = prod_state) product_graph
        then prod product_graph not_in_1_errors_found
          not_in_2_errors_found in_both (*current_path*) more_to_visit
        else
          (match prod_state
           with (state1, state2) ->
             let edges1 = List.assoc state1 graph1 in
             let edges2 = List.assoc state2 graph2 in
             (*
             let (matched_outgoing_edges, unmatched_edges2) =
               List.fold_left
                 (fun (tm1, ns1) -> fun (prod_edges, rem_edges2) ->
                   match (List.partition (fun (tm2,_) -> (tm1 = tm2)) rem_edges2)
                   with (is_tm1_lst, not_tm1_lst) ->
                     ((match List.map (fun (_,ns2) -> (tm1,(ns1, ns2))) is_tm1_lst
                       with [] -> [(tm1, (ns1, ([DEAD],[])))]
                         | s -> s)
                      @ prod_edges,
                      not_tm1_lst))
                 ([],edges2)
                 edges1
             in
             let extra_outgoing_edges =
               List.map (fun (tm1,ns2) -> (tm2, (([DEAD],[]),ns2))) rem_edges2
             in
             *)
             let outgoing_edges =
               try
               List.map2
               (fun (tm1,ns1) (tm2,ns2) ->
                 if tm1 = tm2
                 then (tm1, (ns1, ns2))
                 else raise (Failure
                               ("Edges fail to line up, "^
                                   (string_of_terminal tm1)^", "^
                                   (string_of_terminal tm2))))
                 edges1 edges2
               with _ -> raise (Failure "DFA1 and DFA2 have nodes with differing numbers of egdes.")
             in
(*
             let outgoing_edges =
               set_unoin (assoc_compare compare)
                 extra_outgoing_edges (sort_uniq compare matched_outgoing_edges)
             in
*)
             let new_product_graph =
               update_assoc
                 (pair_compare node_state_compare node_state_compare)
                 (prod_state, outgoing_edges)
                 product_graph
             in
             let (new_nodes_to_visit, (new_left_errs, new_right_errs, new_in_both)) =
               List.fold_left
                 (fun (to_visit, (left_errs, right_errs, both)) (tm,prod_state) ->
                   let new_path = tm::path in
                   (match prod_state
                    with ((nn1,acc1),(nn2,acc2)) ->
                      (update_assoc
                         (pair_compare node_state_compare node_state_compare)
                         (prod_state,new_path)
                         to_visit,
                       if acc1 = acc2
                       then (left_errs, right_errs,
                             if acc1 then new_path::both else both)
                       else
                         if acc1 then (left_errs, new_path::right_errs, both)
                         else (new_path::left_errs, right_errs, both)
                      )
                   )
                 )
                 (more_to_visit,
                  (not_in_1_errors_found, not_in_2_errors_found, in_both))
                 outgoing_edges
             in
             if (not (quit_at_first_error)) ||
               ((new_left_errs, new_right_errs)=([],[]))
             then
               prod new_product_graph new_left_errs new_right_errs
                 new_in_both new_nodes_to_visit
             else
               (new_product_graph, new_left_errs, new_right_errs, new_in_both)
          )
  in prod [] [] [] [] [((start_state1,start_state2),[])]
