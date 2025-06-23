
open Genmap
open Ptgload

let current = "elg1a"
let problem_file_name = (current^".json")
let max_score = 3

let grammar_term = list_map [
  ("T", [["p"; "E"]; ["T"; "n"]; [" 0"]; ["1"]]);
  ("E", [["S"; "%"; "T"]; ["S"]]);
  ("S", [["0"]; ["1"]])]

  (*
let grammar_star = list_map [
		("star", [["id"]; ["plus"; "*"]]);
		("plus", [["id"; "+"; "concat"]; ["concat"]]);
		("concat", [["concat"; "star"]; ["id"]]);
		("id", [["a"]; ["b"]])
	]

let grammar_hash = list_map [
		("exp", [["!"; "conj"]; ["conj"]]);
		("conj", [["prop"; "&"; "id"]; ["id"]]);
		("prop", [["exp"; "#"]; ["conj"]]);
		("id", [["f"]; ["t"]])
	]
  *)
  
let grammar_bank = list_map [
  ("elg1a", Problem(grammar_term, "1nn", "T", false));
  ("elg1b", Problem(grammar_term, "p0", "T", false));
  ("elg2a", Problem(grammar_term, "p0%p0", "T", false));
  ("elg2d", Problem(grammar_term, "p0%1nn", "T", false));

  ("elg2c", Problem(grammar_term, "p1n%p1n", "T", true));
  ("elg2b", Problem(grammar_term, "pp1%p1n", "T", true));
  ("elg2e", Problem(grammar_term, "p%1nnn",  "T", true))
(*;
	("theory-comp-parseTrees-simp1", Problem(grammar_star, "ab+b**", "star", false));
	("theory-comp-parseTrees-simp2", Problem(grammar_star, "b+ab**", "star", false));
	("theory-comp-parseTrees-simp3", Problem(grammar_star, "aa*b*", "star", false));
	("theory-comp-parseTrees-simp4", Problem(grammar_hash, "!f&t#&f", "exp", false));
	("theory-comp-parseTrees-simp5", Problem(grammar_hash, "!f#&t&t", "exp", false))
                                                                *)
]

