{

  open Unifygenparse
  open UnifySteps

exception EndInput

}

let numeric = ['0' - '9']
let lower_case = ['a' - 'z']
let upper_case = ['A' - 'Z' ]
let alpha = ['a' - 'z' 'A' - 'Z' ]
let id_char = numeric | alpha | "_"

let open_comment = "(*"
let close_comment = "*)"
let whitespace = [' ' '\t' '\n']

rule token = parse
  | [' ' '\t'] { token lexbuf }  (* skip over whitespace *)
  | ['\n'] { token lexbuf }  (* skip over whitespace *)
  | eof             { EOF }
  | "->"    { ARROW  }
  | "="     { EQUALS  }
  | "("     { LPAREN  }
  | ")"     { RPAREN  }
  | ","     { COMMA  }
  | "{"     { LBRACE }
  | "}"     { RBRACE }

  | "by"   { BY }
  | "on"   { ON }
  | "o"    { COMPOSE }
  | "Unify" { UNIFY }
  | "Delete" { DELETE }
  | "Decompose" { DECOMPOSE }
  | "Orient" { ORIENT }
  | "Eliminate" { ELIMINATE }
  | "Fail"  { FAIL }
  | "'" lower_case (alpha*) as s  { VAR s }
  | upper_case (alpha*) as s  { CONST s }
  | open_comment       { comment 1 lexbuf }

  | close_comment      { raise (Failure "unmatched closed comment") }
and comment count = parse
   open_comment        { comment (1 + count) lexbuf }
 | close_comment       { match count with 0 -> raise (Failure "Solution error")
                         | 1 -> token lexbuf
                         | n -> comment (n - 1) lexbuf
 }
 | eof             { raise (Failure "unmatched open comment") }
 | _                   { comment count lexbuf }
(* do not modify this function: *)
{ let lextest s = token (Lexing.from_string s)

  let get_all_tokens s =
      let b = Lexing.from_string (s^"\n") in
      let rec g () =
      match token b with EOF -> []
      | t -> t :: g () in
      g ()

let try_get_all_tokens s =
    try (Some (get_all_tokens s), true)
    with Failure "unmatched open comment" -> (None, true)
       | Failure "unmatched closed comment" -> (None, false)
 }
