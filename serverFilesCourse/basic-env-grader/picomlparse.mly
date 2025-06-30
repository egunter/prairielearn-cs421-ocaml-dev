/* Use the expression datatype defined in expressions.ml: */
%{
    open Picoml_eval
    let andsugar l r = IfExp(l,r,ConstExp FalseConst)
    let orsugar l r = IfExp(l,ConstExp TrueConst,r)
    let ltsugar l r = BinOpAppExp(GreaterOp,r,l)
    let leqsugar l r = orsugar (ltsugar l r) (BinOpAppExp(EqOp, l, r))
    let geqsugar l r = orsugar (BinOpAppExp(GreaterOp,l,r))
                               (BinOpAppExp(EqOp, l, r))
    let neqsugar l r = IfExp(BinOpAppExp (EqOp,l,r), ConstExp FalseConst,
    		       			 ConstExp TrueConst)

    let mk_fun args body =
      List.fold_right (fun a -> fun b -> FunExp(a,b)) args body
%}

/* Define the tokens of the language: */
%token <int> INT
%token <float> FLOAT
%token <string> STRING IDENT
%token TRUE FALSE NEG PLUS MINUS TIMES DIV DPLUS DMINUS DTIMES DDIV MOD EXP CARAT
       LT GT LEQ GEQ EQUALS NEQ PIPE ARROW SEMI DSEMI DCOLON AT NIL
       LET REC AND IN IF THEN ELSE FUN MOD RAISE TRY WITH NOT LOGICALAND
       LOGICALOR LBRAC RBRAC LBRACE RBRACE LCLOS RCLOS LPAREN RPAREN COMMA
       UNDERSCORE UNIT HEAD TAIL PRINT FST SND EOF

/* Define the "goal" nonterminal of the grammar: */
%start dec environment
%type <Picoml_eval.dec> dec
%type <(string * Picoml_eval.value) list> environment

%%


dec:
    expression DSEMI      			   { (Anon ( $1)) }
  | LET IDENT EQUALS expression	DSEMI 	           { (Let ($2,$4)) }
  | LET IDENT extra_args EQUALS expression DSEMI   { (Let ($2,mk_fun $3 $5)) }
  | LET REC IDENT IDENT EQUALS expression DSEMI    { (LetRec ($3, $4, $6)) }
  | LET REC IDENT LPAREN IDENT RPAREN EQUALS expression DSEMI    { (LetRec ($3, $5, $8)) }
  | LET REC IDENT IDENT extra_args EQUALS expression DSEMI    { (LetRec ($3, $4, mk_fun $5 $7)) }
  | LET REC IDENT LPAREN IDENT RPAREN extra_args EQUALS expression DSEMI    { (LetRec ($3, $5, mk_fun $7 $9)) }

extra_args:
    IDENT                              { [$1] }
  | LPAREN IDENT RPAREN                { [$2] }
  | IDENT extra_args                   { $1::$2 }
  | LPAREN IDENT RPAREN extra_args     { $2::$4 }

environment:
    LBRACE env_item_list RBRACE         { $2 }
  | LBRACE RBRACE                       { [] }

env_item_list:
    env_item COMMA env_item_list        { $1 :: $3 }
  | env_item                            { [$1] }

env_item:
    IDENT ARROW value                   { ($1,$3) }

value:
    atomic_value                        { $1 }
  | atomic_value DCOLON value           { match $3
                                          with ListVal l
                                              -> ListVal ($1 :: l)
                                            | _ -> raise (Failure
                                                   (short_string_of_value (*is_html*) false $3 ^
                                                    " is not a list")) }

atomic_value:
    UNIT                                { BasicVal UnitVal }
  | INT                                 { BasicVal (IntVal $1) }
  | FLOAT                               { BasicVal (FloatVal $1) }
  | TRUE                                { BasicVal TrueVal }
  | FALSE                               { BasicVal FalseVal }
  | STRING                              { BasicVal (StringVal $1) }
  | LPAREN value_comma_seq RPAREN       { $2 }
  | NIL                                 { ListVal [] }
  | LBRAC  value_list_contents RBRAC    { ListVal $2 }
  | LT IDENT ARROW expression COMMA environment GT     { Closure ($2, $4, $6) }
  | LT LT IDENT IDENT ARROW expression COMMA environment GT GT  { RecVarVal ($3, $4, $6, $8) }
  | LT LT IDENT LPAREN IDENT RPAREN ARROW expression COMMA environment GT GT  { RecVarVal ($3, $5, $8, $10) }

value_comma_seq:
    value COMMA value_comma_seq          { PairVal ($1, $3) }
  | value                                { $1 }

value_list_contents:
    value SEMI value_list_contents      { $1 :: $3 }
  | value                               { [$1] }

expression:
    op_exp				{ $1 }

op_exp:
  | pure_or_exp LOGICALOR and_exp	{ orsugar $1 $3 }
  | and_exp				{ $1 }

and_exp:
  | pure_and_exp LOGICALAND rel_exp	{ andsugar $1 $3 }
  | rel_exp				{ $1 }

rel_exp:
  | pure_rel_exp GT cons_exp		{ BinOpAppExp (GreaterOp,$1,$3) }
  | pure_rel_exp EQUALS cons_exp	{ BinOpAppExp (EqOp,$1,$3) }
  | pure_rel_exp LT cons_exp		{ ltsugar $1 $3 }
  | pure_rel_exp LEQ cons_exp		{ leqsugar $1 $3 }
  | pure_rel_exp GEQ cons_exp		{ geqsugar $1 $3 }
  | pure_rel_exp NEQ cons_exp		{ neqsugar $1 $3 }
  | cons_exp	     			{ $1 }

cons_exp:
  | pure_add_exp DCOLON cons_exp	{ BinOpAppExp(ConsOp,$1,$3) }
  | add_exp				{ $1 }

add_exp:
  | pure_add_exp plus_minus mult_exp	{ BinOpAppExp($2,$1,$3) }
  | mult_exp				{ $1 }

mult_exp:
  | pure_mult_exp times_div expo_exp 	{ BinOpAppExp($2,$1,$3) }
  | expo_exp	       			{ $1 }

expo_exp:
  | pure_app_raise_exp EXP expo_exp	{ BinOpAppExp (ExpoOp,$1,$3) }
  | nonop_exp	       	   		{ $1 }

nonop_exp:
    if_let_fun_try_monop_exp			{ $1 }
  | app_raise_exp			{ $1 }

app_raise_exp:
    app_exp				{ $1 }
  | monop_raise				{ $1 }
  | pure_app_exp monop_raise		{ AppExp($1,$2) }

monop_raise:
    monop RAISE nonop_exp		{ MonOpAppExp ($1,RaiseExp($3)) }
  | RAISE nonop_exp			{ RaiseExp $2 }

app_exp:
  | atomic_expression		{ $1 }
  | pure_app_exp nonapp_exp 	{ AppExp($1,$2) }

nonapp_exp:
    atomic_expression		{ $1 }
  | if_let_fun_try_monop_exp		{ $1 }


if_let_fun_try_monop_exp:
    TRY expression WITH exp_matches	{ match $4 with (x,e,ms) -> TryWithExp ($2, x,e, ms) }
  | LET REC IDENT IDENT EQUALS expression IN expression	{ LetRecInExp($3, $4, $6, $8) }
  | LET REC IDENT LPAREN IDENT RPAREN EQUALS expression IN expression	{ LetRecInExp($3, $5, $8, $10) }
  | LET REC IDENT IDENT extra_args EQUALS expression IN expression	{ LetRecInExp($3, $4, (mk_fun $5 $7), $9) }
  | LET REC IDENT LPAREN IDENT RPAREN extra_args EQUALS expression IN expression	{ LetRecInExp($3, $5, (mk_fun $7 $9), $11) }
  | LET IDENT EQUALS expression IN expression		{ LetInExp($2, $4, $6) }
  | LET IDENT extra_args EQUALS expression IN expression		{ LetInExp($2, (mk_fun $3 $5), $7) }
  | FUN IDENT ARROW expression				{ FunExp($2, $4) }
  | FUN IDENT extra_args ARROW expression				{ FunExp($2, (mk_fun $3 $5)) }
  | IF expression THEN expression ELSE expression	{ IfExp($2, $4, $6) }
  | monop if_let_fun_try_monop_exp     			{ MonOpAppExp ($1,$2) }

exp_matches:
    exp_match					{ (match $1 with (x,e) -> (x,e,[])) }
  | no_try_exp_match PIPE exp_matches		{ (match ($1,$3) with (x,e),(y,f,l) -> (x,e,((y,f)::l))) }

exp_match:
    pat ARROW expression { ($1, $3) }

no_try_exp_match:
    pat ARROW no_try_expression		{ ($1, $3) }


no_try_expression:
    no_try_op_exp			{ $1 }

no_try_op_exp:
  | pure_or_exp LOGICALOR no_try_and_exp	{ orsugar $1 $3 }
  | no_try_and_exp	   		{ $1 }

no_try_and_exp:
    pure_and_exp LOGICALAND no_try_eq_exp	{ andsugar $1 $3 }
  | no_try_eq_exp	     		{ $1 }

no_try_eq_exp:
  no_try_rel_exp     			{ $1 }

no_try_rel_exp:
  | pure_rel_exp GT no_try_cons_exp	{ BinOpAppExp (GreaterOp,$1,$3) }
  | pure_rel_exp EQUALS no_try_cons_exp	{ BinOpAppExp (EqOp,$1,$3) }
  | pure_rel_exp LT no_try_cons_exp	{ ltsugar $1 $3 }
  | pure_rel_exp GEQ no_try_cons_exp	{ geqsugar $1 $3 }
  | pure_rel_exp LEQ no_try_cons_exp	{ leqsugar $1 $3 }
  | pure_rel_exp NEQ no_try_cons_exp	{ neqsugar $1 $3 }
  | no_try_cons_exp    			{ $1 }

no_try_cons_exp:
  | pure_add_exp DCOLON no_try_cons_exp { BinOpAppExp(ConsOp,$1,$3) }
  | no_try_add_exp			{ $1 }

no_try_add_exp:
  | pure_add_exp plus_minus no_try_mult_exp	{ BinOpAppExp($2,$1,$3) }
  | no_try_mult_exp				{ $1 }

no_try_mult_exp:
  | pure_mult_exp times_div no_try_expo_exp	{ BinOpAppExp(IntTimesOp,$1,$3) }
  | no_try_expo_exp				{ $1 }

no_try_expo_exp:
  | pure_app_raise_exp EXP no_try_expo_exp	{ BinOpAppExp(ExpoOp,$1,$3) }
  | no_try_nonop_exp                    	{ $1 }

no_try_nonop_exp:
    no_try_if_let_fun_monop_exp		{ $1 }
  | no_try_app_raise_expression		{ $1 }

no_try_app_raise_expression:
    no_try_app_expression		{ $1 }
  | no_try_monop_expression		{ $1 }
  | pure_app_exp no_try_monop_expression	{ $1 }

no_try_monop_expression:
  | monop RAISE no_try_app_raise_expression { MonOpAppExp($1,RaiseExp($3)) }
  | RAISE no_try_app_raise_expression  { RaiseExp($2) }

no_try_app_expression:
    atomic_expression				{ $1 } 
  | pure_app_exp no_try_nonapp_expression 	{ AppExp($1,$2) }

no_try_nonapp_expression:
    atomic_expression			{ $1 }
  | no_try_if_let_fun_monop_exp		{ $1 }

no_try_if_let_fun_monop_exp:
    IF expression THEN expression ELSE no_try_expression	{ IfExp($2,$4,$6) }
  | LET IDENT EQUALS expression IN no_try_expression		{ LetInExp($2,$4,$6) }
  | LET IDENT extra_args EQUALS expression IN no_try_expression		{ LetInExp($2,(mk_fun $3 $5),$7) }
  | LET REC IDENT IDENT EQUALS expression IN no_try_expression	{ LetRecInExp($3,$4,$6,$8) }
  | LET REC IDENT LPAREN IDENT RPAREN EQUALS expression IN no_try_expression	{ LetRecInExp($3,$5,$8,$10) }
  | LET REC IDENT IDENT extra_args EQUALS expression IN no_try_expression	{ LetRecInExp($3,$4,(mk_fun $5 $7),$9) }
  | LET REC IDENT LPAREN IDENT RPAREN extra_args EQUALS expression IN no_try_expression	{ LetRecInExp($3,$5,(mk_fun $7 $9),$11) }
  | FUN IDENT ARROW no_try_expression				{ FunExp($2, $4) }
  | FUN IDENT extra_args ARROW no_try_expression				{ FunExp($2, (mk_fun $3 $5)) }
  | monop no_try_if_let_fun_monop_exp				{ MonOpAppExp ($1,$2) }

pat:
  | UNDERSCORE	{ None }
  | INT		{ Some $1 }

pure_or_exp:
  | pure_or_exp LOGICALOR pure_and_exp		{ orsugar $1 $3 }
  | pure_and_exp   			{ $1 }

pure_and_exp:
  | pure_and_exp LOGICALAND pure_eq_exp	{ andsugar $1 $3 }
  | pure_eq_exp	     			{ $1 }

pure_eq_exp:
  pure_rel_exp	     		{ $1 }

pure_rel_exp:
  | pure_rel_exp GT pure_cons_exp	{ BinOpAppExp (GreaterOp,$1,$3) }
  | pure_rel_exp EQUALS pure_cons_exp	{ BinOpAppExp (EqOp,$1,$3) }
  | pure_rel_exp LT pure_cons_exp	{ ltsugar $1 $3 }
  | pure_rel_exp GEQ pure_cons_exp	{ geqsugar $1 $3 }
  | pure_rel_exp LEQ pure_cons_exp	{ leqsugar $1 $3 }
  | pure_rel_exp NEQ pure_cons_exp	{ neqsugar $1 $3 }
  | pure_cons_exp	     		{ $1 }

pure_cons_exp:
  | pure_add_exp DCOLON pure_cons_exp   { BinOpAppExp(ConsOp,$1,$3) }
  | pure_add_exp			{ $1 }

pure_add_exp:
  | pure_add_exp plus_minus pure_mult_exp	{ BinOpAppExp($2,$1,$3) }
  | pure_mult_exp				{ $1 }

pure_mult_exp:
  | pure_mult_exp times_div pure_expo_exp 	{ BinOpAppExp($2,$1,$3) }
  | pure_expo_exp	       			{ $1 }

pure_expo_exp:
  | pure_app_raise_exp EXP pure_expo_exp	{ BinOpAppExp (ExpoOp,$1,$3) }
  | pure_app_raise_exp           { $1 }

pure_app_raise_exp:
    pure_app_exp		{ $1 }
  | pure_monop_raise 		{ $1 }
  | pure_app_exp pure_monop_raise { AppExp($1,$2) }

pure_monop_raise:
    monop RAISE pure_app_raise_exp { MonOpAppExp($1,RaiseExp($3)) }
  | RAISE pure_app_raise_exp  { RaiseExp($2) }

pure_app_exp:
    atomic_expression			{ $1 }
  | pure_app_exp atomic_expression 	{ AppExp($1,$2) }

atomic_expression:
    constant_expression         { ConstExp $1 }
  | IDENT			{ VarExp $1 }
  | list_expression		{ $1 }
  | paren_expression            { $1 }
  | monop atomic_expression		{ MonOpAppExp ($1,$2) }

list_expression:
    LBRAC list_contents			{ $2 }
 
list_exp_end:
    RBRAC				{ ConstExp NilConst }
  | SEMI list_tail				{ $2 }

list_tail:
    RBRAC				{ ConstExp NilConst }
  | list_contents			{ $1 }

list_contents:
    expression list_exp_end	{ BinOpAppExp(ConsOp,$1,$2) }

paren_expression:
    LPAREN par_exp_end			{ $2 }

par_exp_end:
    RPAREN								{ ConstExp UnitConst }
  | expression RPAREN			{ $1 }
  | expression COMMA expression RPAREN	{ BinOpAppExp (CommaOp,$1,$3) }

constant_expression:
    INT                         { IntConst $1 }
  | TRUE			{ TrueConst }
  | FALSE			{ FalseConst }
  | FLOAT			{ FloatConst $1 }
  | NIL	  			{ NilConst }
  | STRING			{ StringConst $1 }
  | UNIT			{ UnitConst }


monop:
  | HEAD			{ HdOp }
  | TAIL			{ TlOp }
  | PRINT			{ PrintOp }
  | NEG				{ IntNegOp }
  | FST				{ FstOp }
  | SND				{ SndOp }

plus_minus:
    PLUS				{ IntPlusOp }
  | MINUS				{ IntMinusOp }
  | DPLUS				{ FloatPlusOp }
  | DMINUS				{ FloatMinusOp }
  | CARAT				{ ConcatOp }

times_div:
    TIMES				{ IntTimesOp }
  | DIV					{ IntDivOp }
  | MOD					{ ModOp }
  | DTIMES				{ FloatTimesOp }
  | DDIV				{ FloatDivOp }
