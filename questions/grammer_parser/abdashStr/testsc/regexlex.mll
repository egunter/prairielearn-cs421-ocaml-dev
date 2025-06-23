{
	open Regexparse
}

let id_char = ['a' - 'z' '!' - '\'' ','-'<' '>' '{' '}' '[' - '_']

rule token = parse
	| [' ' '\t' '\n' '\r'] { token lexbuf }
	| id_char as c { SYM c }
	| "E" { EPS }
	| "*" { STAR }
	| "+" { PLUS }
	| "?" { OPTION }
	| "V" { UNION }
	| "=" { EQ }
	| "(" { LPAREN }
	| ")" { RPAREN }
	| "exit" { EXIT }
	| eof { EOF }
