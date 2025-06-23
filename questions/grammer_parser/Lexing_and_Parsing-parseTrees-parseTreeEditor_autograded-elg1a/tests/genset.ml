
open List

type 'a set = Set of 'a list * ('a -> 'a -> bool) * int

let empty_set eq = Set([], eq, 0)
let one_set n eq = Set([n], eq, 1)
let list_set l eq = Set(l, eq, length l)

let size_set s = let Set(_, _, x) = s in x


let take_set s = match s with
	| Set([], _, _) -> None
	| Set(n :: t, eq, x) -> Some (n, Set(t, eq, x - 1))

let in_set_pred s p = let Set(l, _, _) = s in
	let rec in_set_pred_rec l = match l with
		| [] -> false
		| n :: t -> if p n then true else in_set_pred_rec t
	in in_set_pred_rec l

let subtract_set_pred s p = let Set(l, eq, x) = s in
	let rec subtract_set_pred_rec l = match l with
		| [] -> None
		| n :: t -> if p n then Some (n, t) else (match subtract_set_pred_rec t with
			| None -> None
			| Some (r, t') -> Some (r, n :: t')
		)
	in match subtract_set_pred_rec l with
		| None -> None
		| Some (r, l') -> Some (r, Set(l', eq, x - 1))

let cut_set s p = let Set(l, eq, x) = s in
	let rec cut_set_rec l = match l with
		| [] -> ([], [], 0)
		| n :: t -> let (l1, l2, x) = cut_set_rec t in
			if p n then (n :: l1, l2, x + 1) else (l1, n :: l2, x)
	in let (l1, l2, x1) = cut_set_rec l in
		(Set(l1, eq, x1), Set(l2, eq, x - x1))


let in_set s n = let Set(l, eq, _) = s in
	let rec in_set_rec l = match l with
		| [] -> false
		| m :: t -> if eq n m then true else in_set_rec t 
	in in_set_rec l

let subtract_set s n = let Set(l, eq, x) = s in
	let rec subtract_set_rec l = match l with
		| [] -> None
		| m :: t -> if eq n m then Some t else (match subtract_set_rec t with
			| None -> None
			| Some t' -> Some (m :: t')
		)
	in match subtract_set_rec l with
		| None -> None
		| Some l' -> Some (Set(l', eq, x - 1))


let add_set s n = let Set(l, eq, x) = s in
	if in_set s n then Set(l, eq, x) else Set(n :: l, eq, x + 1)

let rec union_set s1 s2 = let Set(l, _, _) = s1 in
	if (size_set s2) < (size_set s1) then union_set s2 s1
	else fold_left (fun s' n -> add_set s' n) s2 l

let rec eq_set s1 s2 = let Set(l, _, _) = s1 in
	if (size_set s2) <> (size_set s1) then false
	else fold_left (fun t n -> t && (in_set s2 n)) true l


