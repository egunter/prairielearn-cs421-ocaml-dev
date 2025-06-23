open Ptgload

let tree_description_from_file file =
  let chan = open_in file in
  let lex = Lexing.from_channel chan in
  let (no_parse, tree_description) = Ptgparse.main Ptglex.token lex in
  let _ = close_in chan in
  (no_parse,
   List.sort
    (fun (nn1, (on1, l1, (x1,y1))) -> (fun (nn2, (on2, l2, (x2,y2))) ->
      compare y1 y2))
      tree_description)

(* It seems that we are guaranteed that outnodes are always below the
   source node.  But we aren't going to chance it. *)

let rec take_out_first p l =
  match l with [] -> raise (Failure "No satisfying result found")
    | (x::xs) -> 
      if p x then (x, xs)
      else (match take_out_first p xs with (y, ys) -> (y, x::ys))

let rec make_trees nodes =
  match nodes with [] -> []
    | ((nodename,(outnode_names,label,(x,y)))::more_nodes) ->
      let subtrees = make_trees more_nodes
      in
      let (outtrees, unused_trees) = 
      List.fold_right
        (fun outnode_name -> fun (outtrees, rem_trees) ->
          let (nexttree, remtrees) =
            take_out_first
              (fun ((ndname,_),tree) -> ndname = outnode_name) rem_trees
          in (nexttree::outtrees, remtrees))
        outnode_names
        ([],subtrees)
      in
      let sorted_outtrees =
        List.sort
          (fun ((n1, (x1,y1)), t1) -> fun ((n2, (x2,y2)), t2) ->
            compare ((x1 -. x)/. (y1 -. y)) ((x2 -. x)/. (y2 -. y)))
          outtrees
      in (((nodename, (x,y)), Tree (label, (List.map snd sorted_outtrees)))::unused_trees)

(*
let student_tree_description = tree_description_from_file Solution.problem_file_name
let student_trees = make_trees (snd student_tree_description)
*)
