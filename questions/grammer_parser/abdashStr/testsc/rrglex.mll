{
  open Rrgparse

}

let numeric   = ['0' - '9']
let lowercase = ['a' - 'z']
let uppercase = ['A'-'Z']
let ident_char = uppercase | lowercase | "_" | "'" | numeric
let whitespace =  [' ' '\t' '\n' '\r']

let tm_char   = ['a' - 'z' '!' - '\'' ','-'<' '>' '{' '}' '[' - '_']

let nonterm = uppercase ident_char*

rule token = parse
	| whitespace { token lexbuf }
        | "Start" whitespace "Symbol" { STARTSYMBOL }
	| "epsilon" { EPSILON }
	| "::=" { GETS }
        | "::-" { GETS }
        | "|" { OR }
        | "="   { EQ }
	| eof { EOF }
	| tm_char as c { TERM (String.make 1 c) }
        | (nonterm as nt) { NONTERM nt }
