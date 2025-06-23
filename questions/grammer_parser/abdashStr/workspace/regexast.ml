
open List;;

(* general data structures *)

type 'a set = Set of 'a list * ('a -> 'a -> bool) * int

let empty_set eq = Set([], eq, 0)

let one_set n eq = Set([n], eq, 1)

let fold_set f z s = let Set(l, _, _) = s in
	fold_left f z l

let map_set f s = let Set(l, eq, x) = s in
	Set((map f l), eq, x)

let in_set s n = let Set(l, eq, _) = s in
	let rec in_set_rec l = match l with
		| [] -> false
		| m :: t -> if eq n m then true else in_set_rec t 
	in in_set_rec l

let size_set s = let Set(_, _, x) = s in x

let add_set s n = let Set(l, eq, x) = s in
	if in_set s n then Set(l, eq, x) else Set(n :: l, eq, x + 1)

let rec union_set s1 s2 =
	if (size_set s2) < (size_set s1) then union_set s2 s1
	else fold_set (fun s' n -> add_set s' n) s2 s1

let rec eq_set s1 s2 =
	if (size_set s2) <> (size_set s1) then false
	else fold_set (fun t n -> t && (in_set s2 n)) true s1

let comp_set s pred = let Set(_, eq, _) = s in
	let (s', x') = fold_set (fun (s', x) n -> if pred n then (n :: s', x + 1) else (s', x)) ([], 0) s
	in Set(s', eq, x')

let compx_set s trans = let Set(_, eq, _) = s in
	let (s', x') = fold_set (fun (s', x) n -> match trans n with
		| None -> (s', x)
		| Some n' -> (n' :: s', x + 1)) ([], 0) s
	in Set(s', eq, x')

(* type for proof of equivalence properties about regexes (using 'a as evidence for the latter (although we may not actually have a proof), and strings accepted by one regex as a proof for non-equivalence)*)

type ('a, 'b) proof =
	| Equiv of 'a
	| NE1 of 'b
	| NE2 of 'b

(* returns nothing is two sets are equal, otherwise returns element present in one set but not the other *)

let rec ineq_proof_set s1 s2 =
		(* assumes s1 is larger than s2*)
	let ineq_char s1 s2 = fold_set (fun t n -> match t with
		| None -> if in_set s2 n then None else Some n
		| Some _ -> t
	) None s1 in 
		if (size_set s1) < (size_set s2) then match ineq_char s2 s1 with
			| None -> Equiv ()
			| Some c -> NE2 c
		else match ineq_char s1 s2 with
			| None -> Equiv ()
			| Some c -> NE1 c

(* regular expressions *)

type regex =
	| Sym of char
	| Concat of regex list
	| Union of regex set
	| Star of regex

let rec eq_regex r1 r2 = match (r1, r2) with
	| (Sym c1, Sym c2) -> c1 = c2
	| (Concat [], Concat []) -> true
	| (Concat (n1 :: t1), Concat (n2 :: t2)) -> (eq_regex (Concat t1) (Concat t2)) && (eq_regex n1 n2)
	| (Union s1, Union s2) -> eq_set s1 s2
	| (Star r1, Star r2) -> eq_regex r1 r2
	| _ -> false

(* maintenance of irreducibility *)

let star_regex r = match r with
	| Star r' -> r
	| Concat [] -> Concat []
	| _ -> Star r

(* maintenance of associativity *)

let concat_regex r1 r2 = match (r1, r2) with 
	| (Concat l1, Concat l2) -> Concat (l1 @ l2)
	| (Concat l', _) -> Concat (l' @ [r2])
	| (_, Concat l') -> Concat (r1 :: l')
	| _ -> Concat [r1; r2]

(* maintenance of commutativity *)

let union_regex r1 r2 = Union (match (r1, r2) with
	| (Union s1, Union s2) -> union_set s1 s2
	| (Union s', _) -> add_set s' r2
	| (_, Union s') -> add_set s' r1
	| _ -> add_set (one_set r1 eq_regex) r2)

(* linear regular expressions *)

type lin_elem =
	| NEps
	| NCon of char * regex

type lin_regex = lin_elem set

let eq_lin_elem e1 e2 = match (e1, e2) with
	| (NEps, NEps) -> true
	| (NCon(c1, r1), NCon(c2, r2)) -> (c1 = c2) && (eq_regex r1 r2)
	| _ -> false

let empty_lin = empty_set eq_lin_elem

let one_lin e = one_set e eq_lin_elem

(* interactive commands *)

type check =
	| Print of regex
	| Eq of regex * regex
	| Exit
