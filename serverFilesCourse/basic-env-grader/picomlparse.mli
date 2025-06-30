type token =
  | INT of (int)
  | FLOAT of (float)
  | STRING of (string)
  | IDENT of (string)
  | TRUE
  | FALSE
  | NEG
  | PLUS
  | MINUS
  | TIMES
  | DIV
  | DPLUS
  | DMINUS
  | DTIMES
  | DDIV
  | MOD
  | EXP
  | CARAT
  | LT
  | GT
  | LEQ
  | GEQ
  | EQUALS
  | NEQ
  | PIPE
  | ARROW
  | SEMI
  | DSEMI
  | DCOLON
  | AT
  | NIL
  | LET
  | REC
  | AND
  | IN
  | IF
  | THEN
  | ELSE
  | FUN
  | RAISE
  | TRY
  | WITH
  | NOT
  | LOGICALAND
  | LOGICALOR
  | LBRAC
  | RBRAC
  | LBRACE
  | RBRACE
  | LCLOS
  | RCLOS
  | LPAREN
  | RPAREN
  | COMMA
  | UNDERSCORE
  | UNIT
  | HEAD
  | TAIL
  | PRINT
  | FST
  | SND
  | EOF

val dec :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Picoml_eval.dec
val environment :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> (string * Picoml_eval.value) list
