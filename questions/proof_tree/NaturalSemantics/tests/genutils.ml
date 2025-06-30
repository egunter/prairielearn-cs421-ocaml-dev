(* File: genutils.ml *)
(* Author: Elsa L. Gunter and John Lee *)
(* Copyright 2017 *)
(* Share and Enjoy *)

let string_of_list left_brace right_brace separator string_of_entry list =
  left_brace
  ^ (String.concat separator (List.map string_of_entry list))
  ^ right_brace

  
let string_of_assoc_list
    left_brace right_brace
    arrow separator
    string_of_key string_of_value
    assoc_list=
  let string_of_entry (key,value) =
    (string_of_key key) ^ arrow ^ (string_of_value value)
  in string_of_list left_brace right_brace separator string_of_entry assoc_list

let rec lookup mapping x =
  match mapping with
     []        -> None
   | (y,z)::ys -> if x = y then Some z else lookup ys x

(* environments *)
type 'a env = (string * 'a) list

let string_of_env arrow string_of_value env =
string_of_assoc_list "{" "}" arrow ", " (fun s -> s) string_of_value env

(*
let string_of_type_env gamma = string_of_env string_of_polyTy ": " gamma
*)

let make_env x y = ([(x,y)]:'a env)
let lookup_env (gamma:'a env) x = lookup gamma x
let sum_env (delta:'a env) (gamma:'a env) = ((delta@gamma):'a env)
let ins_env (gamma:'a env) x y = sum_env (make_env x y) gamma

let rec add_env gamma x y = match gamma with
	| [] -> (x, y) :: []
	| (x', y') :: t ->
		if x = x' then (x, y) :: t
		else (x', y') :: (add_env t x y) 

let rec select_env gamma x = match gamma with
	| [] -> None
	| (x', v') :: t ->
		if x = x' then Some (v', t)
		else (match select_env t x with
			| None -> None
			| Some (v, t) -> Some (v, (x', v') :: t)
		)

let rec zip_list_list l1 l2 = match (l1, l2) with
	([], []) -> []
	| (a :: t1, b :: t2) -> (a, b) :: (zip_list_list t1 t2)
	| _ -> raise (Failure "BUG: genutils.ml - term syntax matching performed on term with malformed arity.")

let rec equiv_env env1 env2 = match env1 with
	| [] -> env2 = []
	| (x, v) :: e1_t -> (match select_env env2 x with
		| None -> false
		| Some (v', e2_t) -> if v = v' then equiv_env e1_t e2_t else false
	)

(* Util functions *)
let rec drop y = function
   []    -> []
 | x::xs -> if x=y then drop y xs else x::drop y xs

let rec delete_duplicates = function
   []    -> []
 | x::xs -> x::delete_duplicates (drop x xs)

(* Things for general parsing *)
type 'a parsed =
    ParseOk of 'a
  | ParseEmpty
  | SyntaxError of string

let string_of_parsed to_str obj =
  match obj with
    ParseEmpty -> "<<no parse: empty field>>"
  | SyntaxError s -> "Syntax Error: " ^ s
  | ParseOk thing -> to_str thing


type unprocessed_node =
  { str_label  : string;
    str_left   : string;
    str_middle : string;
    str_right  : string;
    str_sideCondition : string }

type unprocessed = (string * unprocessed_node) list

