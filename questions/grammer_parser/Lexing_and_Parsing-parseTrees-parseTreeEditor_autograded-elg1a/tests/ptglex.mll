{
  open Ptgparse
}

(* You can assign names to commonly-used regular expressions in this part
   of the code, to save the trouble of re-typing them each time they are used *)

let digit = ['0' - '9']
let digits = digit+
let lowercase = ['a' - 'z']
let letter =['a' - 'z' 'A' - 'Z' '_']
let hex = ['0' - '9' 'a' - 'f']
let ident_char = letter | digit | '_' | '\''
let string_char = ident_char | ' ' | '~' | '`' | '!' | '@' | '#' | '$' | '%' | '^' | '&'
  | '*' | '(' | ')' | '-' | '+' | '=' | '{' | '[' | '}' | ']'
  | '|' | ':' | ';' | '<' | ',' | '>' | '.' | '?' | '/' | '\\'


rule token = parse
        | [' ' '\t' '\n']              { token lexbuf }  (* skip over whitespace *)
        | eof                          { EOF }
          (* binary operators *)
        | "\"noParseTree\""                { NOPARSETREE }
        | "true"                           { TRUE }
        | "false"                          { FALSE }
        | "\"parseTree\""                  { PARSETREE }
        | "\""(("node"digits) as s) "\""   { NODE s }
        | "\"to\""                         { TO }
        | "\"label\""                      { LABEL }
        | "\"&lt;"(letter* as nt) "&gt;\"" { STRING ("<"^nt^">") }
        | "\"meta\""                       { META }
        | "\"xPos\""                       { XPOS }
        | "\"yPos\""                       { YPOS }
(*        | "\""(digits"."digits as n)"\""   { FLOAT (float_of_string n) }
        | "\""(digits as n)"\""            { FLOAT (float_of_string (n^".0")) } *)
        | "\""(string_char* as s)"\""      { STRING s }
        | "["                              { LBRACK }
        | "]"                              { RBRACK }
        | "{"                              { LBRACE }
        | "}"                              { RBRACE }
        | ":"                              { COLON }
        | ","                              { COMMA }


{(* do not modify this function: *)
let lextest s = token (Lexing.from_string s)

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

let get_all_token_options s =
  let b = Lexing.from_string (s^"\n") in
  let rec g () =
    match (try Some (token b) with _ -> None) with Some EOF -> []
      | None -> [None]
      | t -> t :: g () in
  g ()

 }

