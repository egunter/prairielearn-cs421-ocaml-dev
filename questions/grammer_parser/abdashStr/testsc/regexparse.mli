type token =
  | SYM of (char)
  | EPS
  | STAR
  | PLUS
  | OPTION
  | UNION
  | EQ
  | LPAREN
  | RPAREN
  | EXIT
  | EOF

val stmt :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Regexast.check
val exp :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Regexast.regex
