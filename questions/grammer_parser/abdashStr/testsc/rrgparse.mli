type token =
  | TERM of (string)
  | NONTERM of (string)
  | EPSILON
  | GETS
  | OR
  | STARTSYMBOL
  | EQ
  | EOF

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Rrg.right_regular_grammar
val ext_main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Rrg.ext_right_regular_grammar
val ext_grammar :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Rrg.ext_right_regular_grammar
val grammar :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Rrg.right_regular_grammar
val start_decl :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Rrg.nonterminal
val ext_rrg_rules :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Rrg.ext_rrg_rule list
val term_list :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Rrg.terminal list
val ext_prods_plus :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Rrg.ext_production list * Rrg.ext_rrg_rule list
