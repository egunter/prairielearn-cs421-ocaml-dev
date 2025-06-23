type token =
  | TERM of (string)
  | NONTERM of (string)
  | EPSILON
  | GETS
  | OR
  | STARTSYMBOL
  | EQ
  | EOF

open Parsing;;
let _ = parse_error;;
# 2 "rrgparse.mly"
	open Rrg
# 16 "rrgparse.ml"
let yytransl_const = [|
  259 (* EPSILON *);
  260 (* GETS *);
  261 (* OR *);
  262 (* STARTSYMBOL *);
  263 (* EQ *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  257 (* TERM *);
  258 (* NONTERM *);
    0|]

let yylhs = "\255\255\
\002\000\001\000\004\000\004\000\003\000\003\000\005\000\006\000\
\007\000\007\000\007\000\007\000\008\000\008\000\008\000\008\000\
\008\000\008\000\008\000\008\000\008\000\009\000\010\000\010\000\
\010\000\010\000\010\000\010\000\010\000\010\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000"

let yylen = "\002\000\
\002\000\002\000\002\000\002\000\002\000\002\000\003\000\003\000\
\001\000\002\000\001\000\002\000\001\000\002\000\003\000\004\000\
\002\000\003\000\001\000\003\000\002\000\003\000\001\000\003\000\
\001\000\003\000\002\000\004\000\002\000\003\000\002\000\002\000\
\002\000\002\000\002\000\002\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\031\000\000\000\000\000\000\000\000\000\
\032\000\000\000\000\000\000\000\033\000\034\000\035\000\036\000\
\000\000\000\000\037\000\000\000\000\000\038\000\000\000\000\000\
\002\000\004\000\003\000\000\000\001\000\006\000\005\000\012\000\
\010\000\000\000\021\000\000\000\000\000\017\000\000\000\000\000\
\022\000\007\000\008\000\020\000\000\000\018\000\015\000\000\000\
\000\000\029\000\000\000\016\000\000\000\030\000\026\000\024\000\
\028\000"

let yydgoto = "\009\000\
\012\000\017\000\018\000\013\000\014\000\020\000\029\000\030\000\
\015\000\049\000"

let yysindex = "\042\000\
\017\255\032\255\032\255\017\255\004\255\009\255\019\255\064\255\
\000\000\023\255\034\255\000\000\052\000\060\255\004\255\065\255\
\000\000\068\000\009\255\004\255\000\000\000\000\000\000\000\000\
\019\255\019\255\000\000\054\255\058\255\000\000\036\255\068\255\
\000\000\000\000\000\000\064\255\000\000\000\000\000\000\000\000\
\000\000\064\255\000\000\049\255\064\255\000\000\059\255\066\255\
\000\000\000\000\000\000\000\000\064\255\000\000\000\000\053\255\
\036\255\000\000\036\255\000\000\036\255\000\000\000\000\000\000\
\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\001\000\003\000\000\000\004\000\005\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\006\000\000\000\000\000\007\000\008\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\009\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000"

let yygindex = "\000\000\
\000\000\000\000\069\000\070\000\011\000\252\255\010\000\232\255\
\242\255\227\255"

let yytablesize = 271
let yytable = "\034\000\
\011\000\024\000\009\000\019\000\013\000\014\000\025\000\023\000\
\027\000\011\000\016\000\051\000\019\000\019\000\038\000\023\000\
\027\000\052\000\010\000\025\000\055\000\026\000\011\000\043\000\
\046\000\035\000\031\000\063\000\060\000\064\000\039\000\065\000\
\058\000\016\000\040\000\041\000\047\000\011\000\048\000\054\000\
\032\000\062\000\001\000\002\000\003\000\004\000\005\000\006\000\
\007\000\008\000\016\000\033\000\036\000\053\000\010\000\016\000\
\031\000\061\000\042\000\044\000\056\000\010\000\045\000\057\000\
\025\000\028\000\026\000\037\000\036\000\050\000\059\000\021\000\
\000\000\022\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\011\000\000\000\009\000\011\000\011\000\009\000\
\009\000\019\000\013\000\014\000\025\000\023\000\027\000"

let yycheck = "\014\000\
\000\000\006\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\006\001\002\001\036\000\002\000\003\000\019\000\005\000\
\007\000\042\000\002\001\001\001\045\000\003\001\006\001\028\000\
\029\000\015\000\004\001\057\000\053\000\059\000\020\000\061\000\
\047\000\002\001\025\000\026\000\001\001\006\001\003\001\044\000\
\007\001\056\000\001\000\002\000\003\000\004\000\005\000\006\000\
\007\000\008\000\002\001\000\000\004\001\005\001\002\001\002\001\
\004\001\005\001\005\001\002\001\002\001\002\001\005\001\005\001\
\001\001\002\001\003\001\000\000\004\001\002\001\005\001\003\000\
\255\255\004\000\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\002\001\255\255\002\001\005\001\006\001\005\001\
\006\001\006\001\006\001\006\001\006\001\006\001\006\001"

let yynames_const = "\
  EPSILON\000\
  GETS\000\
  OR\000\
  STARTSYMBOL\000\
  EQ\000\
  EOF\000\
  "

let yynames_block = "\
  TERM\000\
  NONTERM\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Rrg.ext_right_regular_grammar) in
    Obj.repr(
# 22 "rrgparse.mly"
                                ( _1 )
# 180 "rrgparse.ml"
               : Rrg.ext_right_regular_grammar))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Rrg.right_regular_grammar) in
    Obj.repr(
# 25 "rrgparse.mly"
                                ( _1 )
# 187 "rrgparse.ml"
               : Rrg.right_regular_grammar))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Rrg.rrg_rule list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Rrg.nonterminal) in
    Obj.repr(
# 28 "rrgparse.mly"
                               ( (_1, _2) )
# 195 "rrgparse.ml"
               : Rrg.right_regular_grammar))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Rrg.nonterminal) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Rrg.rrg_rule list) in
    Obj.repr(
# 29 "rrgparse.mly"
                               ( (_2, _1) )
# 203 "rrgparse.ml"
               : Rrg.right_regular_grammar))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Rrg.ext_rrg_rule list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Rrg.nonterminal) in
    Obj.repr(
# 32 "rrgparse.mly"
                                   ( (_1, _2) )
# 211 "rrgparse.ml"
               : Rrg.ext_right_regular_grammar))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Rrg.nonterminal) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Rrg.ext_rrg_rule list) in
    Obj.repr(
# 33 "rrgparse.mly"
                                   ( (_2, _1) )
# 219 "rrgparse.ml"
               : Rrg.ext_right_regular_grammar))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 36 "rrgparse.mly"
                            ( NT _3 )
# 226 "rrgparse.ml"
               : Rrg.nonterminal))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Rrg.ext_production list * Rrg.ext_rrg_rule list) in
    Obj.repr(
# 39 "rrgparse.mly"
                                ( (let (ext_prods, ext_rules) = _3 in
                                   (List.map (fun p -> (NT _1, p)) ext_prods)@ext_rules) )
# 235 "rrgparse.ml"
               : Rrg.ext_rrg_rule list))
; (fun __caml_parser_env ->
    Obj.repr(
# 43 "rrgparse.mly"
                                           ( [] )
# 241 "rrgparse.ml"
               : Rrg.terminal list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Rrg.terminal list) in
    Obj.repr(
# 44 "rrgparse.mly"
                                           ( _2 )
# 248 "rrgparse.ml"
               : Rrg.terminal list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 45 "rrgparse.mly"
                                           ( [T _1] )
# 255 "rrgparse.ml"
               : Rrg.terminal list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Rrg.terminal list) in
    Obj.repr(
# 46 "rrgparse.mly"
                                           ( (T _1) :: _2 )
# 263 "rrgparse.ml"
               : Rrg.terminal list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Rrg.terminal list) in
    Obj.repr(
# 49 "rrgparse.mly"
                                            ( ([ExtOne _1],[]) )
# 270 "rrgparse.ml"
               : Rrg.ext_production list * Rrg.ext_rrg_rule list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Rrg.terminal list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 50 "rrgparse.mly"
                                            ( ([ExtMore (_1, NT _2)],[]) )
# 278 "rrgparse.ml"
               : Rrg.ext_production list * Rrg.ext_rrg_rule list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Rrg.terminal list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Rrg.ext_production list * Rrg.ext_rrg_rule list) in
    Obj.repr(
# 51 "rrgparse.mly"
                                            ( let (prods, rules) = _3
                                              in (insert_uniq
                                                    ext_production_compare
                                                    (ExtOne _1)
                                                    prods, rules) )
# 290 "rrgparse.ml"
               : Rrg.ext_production list * Rrg.ext_rrg_rule list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : Rrg.terminal list) in
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Rrg.ext_production list * Rrg.ext_rrg_rule list) in
    Obj.repr(
# 56 "rrgparse.mly"
                                            ( let (prods, rules) = _4
                                              in (insert_uniq
                                                    ext_production_compare
                                                    (ExtMore (_1, NT _2))
                                                    prods, rules) )
# 303 "rrgparse.ml"
               : Rrg.ext_production list * Rrg.ext_rrg_rule list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Rrg.terminal list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Rrg.ext_rrg_rule list) in
    Obj.repr(
# 61 "rrgparse.mly"
                                            ( ([ExtOne _1],_2) )
# 311 "rrgparse.ml"
               : Rrg.ext_production list * Rrg.ext_rrg_rule list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Rrg.terminal list) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Rrg.ext_rrg_rule list) in
    Obj.repr(
# 62 "rrgparse.mly"
                                            ( ([ExtMore (_1, NT _2)], _3) )
# 320 "rrgparse.ml"
               : Rrg.ext_production list * Rrg.ext_rrg_rule list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 63 "rrgparse.mly"
                                            ( ([ExtMore ([], NT _1)],[]) )
# 327 "rrgparse.ml"
               : Rrg.ext_production list * Rrg.ext_rrg_rule list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Rrg.ext_production list * Rrg.ext_rrg_rule list) in
    Obj.repr(
# 64 "rrgparse.mly"
                                            ( let (prods, rules) = _3
                                              in (insert_uniq
                                                    ext_production_compare
                                                    (ExtMore ([],NT _1))
                                                    prods, rules) )
# 339 "rrgparse.ml"
               : Rrg.ext_production list * Rrg.ext_rrg_rule list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Rrg.ext_rrg_rule list) in
    Obj.repr(
# 69 "rrgparse.mly"
                                            ( ([ExtMore ([],NT _1)], _2) )
# 347 "rrgparse.ml"
               : Rrg.ext_production list * Rrg.ext_rrg_rule list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Rrg.production list * Rrg.rrg_rule list) in
    Obj.repr(
# 73 "rrgparse.mly"
                            ( (let (prods, rules) = _3 in (List.map (fun p -> (NT _1, p)) prods)@rules) )
# 355 "rrgparse.ml"
               : Rrg.rrg_rule list))
; (fun __caml_parser_env ->
    Obj.repr(
# 76 "rrgparse.mly"
                                           ( ([Epsilon],[]) )
# 361 "rrgparse.ml"
               : Rrg.production list * Rrg.rrg_rule list))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Rrg.production list * Rrg.rrg_rule list) in
    Obj.repr(
# 77 "rrgparse.mly"
                                           ( let (prods, rules) = _3 in (Epsilon::prods, rules) )
# 368 "rrgparse.ml"
               : Rrg.production list * Rrg.rrg_rule list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 78 "rrgparse.mly"
                                           ( ([One(T _1)], []) )
# 375 "rrgparse.ml"
               : Rrg.production list * Rrg.rrg_rule list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Rrg.production list * Rrg.rrg_rule list) in
    Obj.repr(
# 79 "rrgparse.mly"
                                           ( let (prods, rules) = _3 in (One(T _1)::prods, rules) )
# 383 "rrgparse.ml"
               : Rrg.production list * Rrg.rrg_rule list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 80 "rrgparse.mly"
                                           ( ([More (T _1, NT _2)],[]) )
# 391 "rrgparse.ml"
               : Rrg.production list * Rrg.rrg_rule list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Rrg.production list * Rrg.rrg_rule list) in
    Obj.repr(
# 81 "rrgparse.mly"
                                           ( let (prods, rules) = _4 in (More (T _1, NT _2)::prods, rules) )
# 400 "rrgparse.ml"
               : Rrg.production list * Rrg.rrg_rule list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Rrg.rrg_rule list) in
    Obj.repr(
# 82 "rrgparse.mly"
                                           ( ([One (T _1)], _2) )
# 408 "rrgparse.ml"
               : Rrg.production list * Rrg.rrg_rule list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Rrg.rrg_rule list) in
    Obj.repr(
# 83 "rrgparse.mly"
                                           ( ([More (T _1, NT _2)], _3) )
# 417 "rrgparse.ml"
               : Rrg.production list * Rrg.rrg_rule list))
(* Entry main *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
(* Entry ext_main *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
(* Entry ext_grammar *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
(* Entry grammar *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
(* Entry start_decl *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
(* Entry ext_rrg_rules *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
(* Entry term_list *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
(* Entry ext_prods_plus *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let main (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Rrg.right_regular_grammar)
let ext_main (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 2 lexfun lexbuf : Rrg.ext_right_regular_grammar)
let ext_grammar (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 3 lexfun lexbuf : Rrg.ext_right_regular_grammar)
let grammar (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 4 lexfun lexbuf : Rrg.right_regular_grammar)
let start_decl (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 5 lexfun lexbuf : Rrg.nonterminal)
let ext_rrg_rules (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 6 lexfun lexbuf : Rrg.ext_rrg_rule list)
let term_list (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 7 lexfun lexbuf : Rrg.terminal list)
let ext_prods_plus (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 8 lexfun lexbuf : Rrg.ext_production list * Rrg.ext_rrg_rule list)
