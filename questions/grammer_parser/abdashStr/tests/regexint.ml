
open Regexast
open List

	(* The regular expression equivalence function developed here is an adaptation of -
		http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.140.6656&rep=rep1&type=pdf *)

(* string functions *)

let rec string_of_list r f sep = match r with
	| [] -> ""
	| n :: t -> fold_left (fun s n' -> s ^ sep ^ (f n')) (f n) t 

let string_of_set s f sep = let Set(l, _, _) = s in string_of_list (rev l) f sep

let rec string_of_regex r = match r with
	Sym i -> String.make 1 i
	| Concat [] -> "E"
	| Concat rl -> string_of_list rl string_of_regex ""
	| Union (Set(rl, _, _))  -> "(" ^ (string_of_list (rev rl) string_of_regex " | ") ^ ")"
	| Star (Sym i) -> (String.make 1 i) ^ "*"
	| Star (Union s) -> (string_of_regex (Union s)) ^ "*"
	| Star r' -> "(" ^ (string_of_regex r') ^ ")*"

let rec string_of_lin_regex r = string_of_set r string_of_lin_elem " || "
and string_of_lin_elem e = match e with
	| NEps -> "E"
	| NCon(c, n) -> (String.make 1 c) ^ " " ^ (string_of_regex n)

let string_of_check c = match c with
	Equiv _ -> "equivalent"
	| NE1 s -> "non-equivalent (regex 1 accepts: " ^ (if s = "" then "E" else s) ^ ")"
	| NE2 s -> "non-equivalent (regex 2 accepts: " ^ (if s = "" then "E" else s) ^ ")"

let str_list_of_deriv_regex_pair (a, b, p) = [
	"  " ^ p;
	"    " ^ (string_of_regex a);
	"    " ^ (string_of_regex b)]

let rec string_of_str_list sl indent = match sl with
	| [] -> ""
	| s :: t -> indent ^ s ^ "\n" ^ (string_of_str_list t indent)

let string_of_set_regex s indent = let Set(l, _, _) = s in
	let rec deriv_list dl = match dl with
		| [] -> ["}"]
		|	drp :: t -> (str_list_of_deriv_regex_pair drp) @ (deriv_list t)
	in string_of_str_list ("{" :: (deriv_list l)) indent

(* constant functions *)

let rec may_empty r = match r with
	| Sym i -> false
	| Concat l -> fold_left (fun a r' -> a && (may_empty r')) true l
	| Union s -> fold_set (fun a r' -> a || (may_empty r')) false s
	| Star r' -> true

(* linearization functions *)

	(* linearization is the process of turning a regular expression into a union of regular expressions, where
		each regular expression in the union is a concatenation that begins with a single character (ie turning
		the regular expression into essentially a transition function, where given a character, selects a new
		regular expression).

		the paper, to simplify the algorithm, ignores whether epsilon is producible from the
		regular expression, determining this fact later on; it also puts actually getting the first character of
		each concatenation from the union into a different step.

		our version of the algorithm does all these things in one step, essentially covering `pre-linearization`,
		`linearization`, and makes it easier to complete the derivatives function. it also subsumes the need for
		the `const` function, since the linearized regular expression will also
	*)

let rec lin1 r = (*print_string ((string_of_regex r) ^ "\n"); *)match r with
	| Sym i -> one_lin (NCon(i, Concat []))
	| Union (Set(s, _, _)) -> lin_union1 s
	| Concat [] -> one_lin NEps
	| Concat [a] -> lin1 a
	| Concat ((Sym i) :: t) -> one_lin (NCon(i, Concat t))
		(* a concat beginning with a union can have the tail distributed across each union member
			(we make the assumption that the result of the map will maintain uniqueness) *)
	| Concat ((Union (Set(s, eq, x))) :: t) ->
		lin1 (Union (Set(map (fun r' -> match r' with
			| Concat s' -> Concat (s' @ t)
			| _ -> Concat (r' :: t)
		) s, eq, x)))
		(* this is a property which should be enforced by construction, but is difficult to statically check. *)
	| Concat ((Concat _) :: t) -> raise (Failure "Bug: Illegal directly nested REGEX concats")
		(* it is worth noting that there is a kind of fixed point equation that needs to be solved here to
			prevent our algorithm from performing an infinite loop. it's obvious that lin(a*b) = lin(aa*b) + lin(b),
			however if lin(a) includes epsilon, then it will trigger recursion over lin(a*b). for this reason, we
			calculate lin(a)a*b + lin(b) instead. *)
	| Concat ((Star a) :: t) ->
		let a_set = compx_set (lin1 a) (fun lr -> match lr with
			| NEps -> None
			| NCon(c, ra) -> Some(NCon(c, concat_regex ra r))
		) in union_set a_set (lin1 (Concat t))
	| Star (Star a) -> raise (Failure "Bug: Illegal directly nested REGEX kleene stars")
	| Star a ->
		let a_set = compx_set (lin1 a) (fun lr -> match lr with
			| NEps -> None
			| NCon(c, ra) -> Some(NCon(c, concat_regex ra r))
		) in union_set a_set (one_lin NEps)
		(* for epsilon to be unproducible in a union, it must not be producible in any union member *)
and lin_union1 rl = match rl with
	| [] -> raise (Failure "Bug: Nullary REGEX (empty union) during linearization ")
	| [a] -> lin1 a
	| a :: t -> union_set (lin1 a) (lin_union1 t)

let lin r = lin1 r

(* determiniser function *)

(* * finds some c to perform split with *)

let rec some_var s =
	let rec some_var_rec l = match l with
		| [] -> None
		| NEps :: t -> some_var_rec t
		| (NCon(c, r)) :: t -> Some c
	in let Set(l, eq, x) = s in some_var_rec l

(* * returns the union of regexes associated with c, and the remaining union w/ those regexes removed *)

let rec split_set s c = let Set(l, eq, _) = s in
	let rec split_set_rec l' = match l' with
		| [] -> (([], 0), empty_set eq_regex)
		| NEps :: t -> let ((t', x'), st) = split_set_rec t in ((NEps :: t', x' + 1), st)
		| (NCon(c', r)) :: t -> let ((t', x'), st) = split_set_rec t in
			if c = c' then (match r with
				| Union s -> ((t', x'), union_set s st)
				| _ -> ((t', x'), add_set st r)
			) else (((NCon(c', r)) :: t', x' + 1), st)
	in let ((l', x'), sc) = split_set_rec l in (sc, Set(l', eq, x'))

(* * if there is some c that can be used to split, use it, otherwise add the regexes back in*)

	(* determinization is the process of pulling out the single characters produced by linearization and
		if more than two regular expressions in the union are guaranteed to start with the same single
		character, they are merged into a single regular expression. ie ar + as = a(r + s), where a is a
		character but r and s are general regular expressions. *)

let det r =
	let rec det_rec (clist, cx) r' = match some_var r' with
		| None -> let Set(l, eq, x) = r' in Set(l @ clist, eq, x + cx)
		| Some c -> let (rc, rt) = split_set r' c in
			det_rec ((NCon(c, Union rc)) :: clist, cx + 1) rt
	in det_rec ([], 0) r

(* instance of string accepted by regex *)

let rec inst r = match r with
	| Sym i -> String.make 1 i
	| Concat l -> fold_left (^) "" (map inst l)
	| Union (Set([], _, _)) -> raise (Failure "Bug: Nullary REGEX (empty union) while proving regex inequality (at step where regexes are checked for acceptance of the empty string)")
	| Union (Set(a :: _, _, _)) -> inst a
	| Star a -> ""

let inst_lin r = match r with
	| Set([], _, _) -> raise (Failure "Bug: Nullary REGEX (empty union) while attempting to prove regex inequality (at step where regexes are checked for matching heads)")
	| Set(NEps :: _, _, _) -> ""
	| Set((NCon(c, r)) :: _, _, _) -> (String.make 1 c) ^ (inst r)

(* derivative functions *)

let eq_equiv (a1, b1) (a2, b2) = (eq_regex a1 a2) && (eq_regex b1 b2)
let eq_equiv_prefix (a1, b1, _) (a2, b2, _) = (eq_regex a1 a2) && (eq_regex b1 b2)

let rec head s = fold_set (fun sc n -> match n with
		| NEps -> sc
		| NCon(c, _) -> add_set sc c
	) (empty_set (=)) s

let string_of_char c = String.make 1 c

let rec deriv_c r c =
	let rec deriv_c_rec l = match l with
		| [] -> raise (Failure "Bug: Head character has no matching tail during derivation. ")
		| NEps :: t -> deriv_c_rec t
		| NCon(c', r) :: t -> if c = c' then r else deriv_c_rec t
	in let Set(l, _, _) = r in deriv_c_rec l

(* returns either the set of tails, or a head that exists for one regex but not the other *)

let rec deriv_ineqp r1 r2 p = let sc = (head r1) in
	match ineq_proof_set sc (head r2) with
		| Equiv () -> Equiv (fold_set (fun d c ->
			add_set d (deriv_c r1 c, deriv_c r2 c, p ^ (String.make 1 c))
		) (empty_set eq_equiv_prefix) sc)
		| NE1 c -> NE1 (p ^ (String.make 1 c) ^ (inst (deriv_c r1 c)))
		| NE2 c -> NE2 (p ^ (String.make 1 c) ^ (inst (deriv_c r2 c)))

(* equivalence algorithm *)

let rec equiv_set_ineqp st h = match st with
	| Set([], _, _) -> Equiv ()
	| Set((a, b, p) :: s, eq, x) ->
		(*(print_string ("\n" ^ (string_of_set_regex st "") ^ "\n"));*) (
		if (may_empty a) <> (may_empty b) then if may_empty a then NE1 p else NE2 p
		else let (a', b') = (det (lin a), det (lin b)) in
			(*(print_string ("# " ^ (string_of_lin_regex a') ^ "\n"));
			(print_string ("# " ^ (string_of_lin_regex b') ^ "\n"));*)
			let h' = add_set h (a, b, "") in (match deriv_ineqp a' b' p with
				| NE1 s -> NE1 s
				| NE2 s -> NE2 s
				| Equiv s' -> let s'' = comp_set s' (fun n -> not (in_set h' n)) in
					equiv_set_ineqp (union_set (Set(s, eq, x - 1)) s'') h'
			)
		)

let equiv r1 r2 = equiv_set_ineqp (one_set (r1, r2, "") eq_equiv_prefix) (empty_set eq_equiv_prefix)

(* evaluation *)

let eval s = match s with
	Print r -> print_string ("\n" ^ (string_of_lin_regex (lin r)) ^ "\n\n")
	(*Print r -> print_string ("\n" ^ (string_of_regex r) ^ "\n\n")*)
	| Eq(r1, r2) ->
		print_string ("\n" ^ (string_of_regex r1) ^ " = " ^ (string_of_regex r2) ^ "\n");
		print_string ("  " ^ (string_of_check (equiv r1 r2)) ^ "\n\n")
	| Exit -> exit 0

(*
let _ =
	let rec loop () =
		(print_string "> "; flush stdout);
		try let lex = Lexing.from_string (input_line stdin)
			in let stmt = Regexparse.stmt Regexlex.token lex
			in (eval stmt; loop ())
		with Parsing.Parse_error ->
			(print_string "\ndoes not parse\n"; loop ())
	in loop ()
*)

