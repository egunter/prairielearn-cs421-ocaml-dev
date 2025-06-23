/* Use the expression datatype defined in expressions.ml: */
%{

  open Picoml_exp_and_cps
  (* We assume all sugar has been removed. *)

  let mk_fun args body =
    List.fold_right (fun a -> fun b -> FunExp(a,b)) args body 
(* We probably shouldn't have this either *)
%}

/* Define the tokens of the language: */
%token <int> INT
%token <float> FLOAT
%token <string> STRING IDENT RULE METAVAR
%token TRUE FALSE NEG PLUS MINUS TIMES DIV DPLUS DMINUS DTIMES DDIV MOD EXP CARAT
       LT GT LEQ GEQ EQUALS NEQ PIPE ARROW DARROW SEMI DSEMI DCOLON AT NIL
       LET REC AND IN IF THEN ELSE FUN MOD RAISE TRY WITH NOT LOGICALAND
       LOGICALOR LBRAC RBRAC LBRACE RBRACE LCLOS RCLOS LPAREN RPAREN COMMA
       UNDERSCORE UNIT HEAD TAIL PRINT FST SND EOF
       IFc THENc ELSEc FUNc DLBRAC DRBRAC KVAR FNc FIXc REPORTc DOT

/* Define the "goal" nonterminal of the grammar: */
%start interactive expression continuation exp_cps

%type <Picoml_exp_and_cps.exp> expression
%type <Picoml_exp_and_cps.exp_cps option> interactive
%type <Picoml_exp_and_cps.cps_cont> continuation
%type <Picoml_exp_and_cps.exp_cps> exp_cps
%%

interactive:
DARROW exp_cps /* BY RULE */                              { Some ($2) }
  | exp_cps                                     { (print_string "\nPlease start with => \n"; Some $1) }
  | EOF                                            { None }

extra_args:
    IDENT                              { [$1] }
  | LPAREN IDENT RPAREN                { [$2] }
  | IDENT extra_args                   { $1::$2 }
  | LPAREN IDENT RPAREN extra_args     { $2::$4 }


expression:
    rel_exp				{ $1 }

rel_exp:
  | pure_rel_exp GT cons_exp		{ BinOpAppExp (GreaterOp,$1,$3) }
  | pure_rel_exp EQUALS cons_exp	{ BinOpAppExp (EqOp,$1,$3) }
  | cons_exp	     			{ $1 }

cons_exp:
  | pure_add_exp DCOLON cons_exp	{ BinOpAppExp(ConsOp,$1,$3) }
  | add_exp				{ $1 }

add_exp:
  | pure_add_exp plus_minus mult_exp	{ BinOpAppExp($2,$1,$3) }
  | mult_exp				{ $1 }

mult_exp:
  | pure_mult_exp times_div nonop_exp 	{ BinOpAppExp($2,$1,$3) }
  | nonop_exp	       			{ $1 }

nonop_exp:
    if_let_fun_monop_exp			{ $1 }
  | app_exp			{ $1 }
/* replace line above with line below if adding raise
  | app_raise_exp			{ $1 }

app_raise_exp:
    app_exp				{ $1 }
  | monop_raise				{ $1 }
  | pure_app_exp monop_raise		{ AppExp($1,$2) }

monop_raise:
    monop RAISE nonop_exp		{ MonOpAppExp ($1,RaiseExp($3)) }
  | RAISE nonop_exp			{ RaiseExp $2 }
    */

app_exp:
  | atomic_expression		{ $1 }
  | pure_app_exp nonapp_exp 	{ AppExp($1,$2) }

nonapp_exp:
    atomic_expression		{ $1 }
  | if_let_fun_monop_exp		{ $1 }


if_let_fun_monop_exp:
    LET REC IDENT IDENT EQUALS expression IN expression	{ LetRecInExp($3, $4, $6, $8) }
  | LET REC IDENT LPAREN IDENT RPAREN EQUALS expression IN expression	{ LetRecInExp($3, $5, $8, $10) }
  | LET REC IDENT IDENT extra_args EQUALS expression IN expression	{ LetRecInExp($3, $4, (mk_fun $5 $7), $9) }
  | LET REC IDENT LPAREN IDENT RPAREN extra_args EQUALS expression IN expression	{ LetRecInExp($3, $5, (mk_fun $7 $9), $11) }
  | LET IDENT EQUALS expression IN expression		{ LetInExp($2, $4, $6) }
  | LET IDENT extra_args EQUALS expression IN expression		{ LetInExp($2, (mk_fun $3 $5), $7) }
  | FUN IDENT ARROW expression				{ FunExp($2, $4) }
  | FUN IDENT extra_args ARROW expression				{ FunExp($2, (mk_fun $3 $5)) }
  | IF expression THEN expression ELSE expression	{ IfExp($2, $4, $6) }
  | monop if_let_fun_monop_exp     			{ MonOpAppExp ($1,$2) }

/*
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
*/
pure_rel_exp:
  | pure_rel_exp GT pure_cons_exp	{ BinOpAppExp (GreaterOp,$1,$3) }
  | pure_rel_exp EQUALS pure_cons_exp	{ BinOpAppExp (EqOp,$1,$3) }
  | pure_rel_exp LT pure_cons_exp	{ BinOpAppExp (GreaterOp,$3,$1) }
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
  | pure_app_exp		{ $1 }
/* replace line above with line below if adding raise
  | pure_app_raise_exp           { $1 }

pure_app_raise_exp:
    pure_app_exp		{ $1 }
  | pure_monop_raise 		{ $1 }
  | pure_app_exp pure_monop_raise { AppExp($1,$2) }

pure_monop_raise:
    monop RAISE pure_app_raise_exp { MonOpAppExp($1,RaiseExp($3)) }
  | RAISE pure_app_raise_exp  { RaiseExp($2) }
*/

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
  | TRUE			{ BoolConst true }
  | FALSE			{ BoolConst false }
  | FLOAT			{ FloatConst $1 }
  | NIL	  			{ NilConst }
  | STRING			{ StringConst $1 }
  | UNIT			{ UnitConst }


monop:
  | HEAD			{ HdOp }
  | TAIL			{ TlOp }
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


/* CPS stuff after here */

continuation:
  | REPORTc                             { External }
  | KVAR                                { ContVarCPS Kvar }
  | FNc IDENT ARROW exp_cps             { FnContCPS ($2, $4) }
  | LPAREN continuation RPAREN          { $2 }

exp_cps:
  | DLBRAC expression DRBRAC continuation         { CPS_Trans ($2, $4) }
  | continuation IDENT                            { VarCPS ($1, $2) }
  | continuation constant_expression              { ConstCPS ($1, $2) }
  | continuation monop IDENT                      { MonOpAppCPS ($1, $2, $3) }
  | continuation LPAREN IDENT binop IDENT RPAREN  { BinOpAppCPS ($1, $4, $3, $5) }
  | IFc IDENT THENc exp_cps ELSEc exp_cps         { IfCPS ($2, $4, $6) }
  | IDENT IDENT continuation                      { AppCPS ($3, $1, $2) }
  | continuation LPAREN FUNc IDENT KVAR ARROW exp_cps RPAREN
                                                  { FunCPS ($1, $4, Kvar, $7) }
  | continuation LPAREN FIXc IDENT DOT FUNc IDENT KVAR exp_cps RPAREN
      { FixCPS ($1, $4, $7, Kvar, $9) }
  | LPAREN exp_cps RPAREN                         { $2 }

binop:
  | GT              { GreaterOp }
  | EQUALS          { EqOp }
  | DCOLON          { ConsOp }
  | plus_minus      { $1 }
  | times_div       { $1 }
  | COMMA           { CommaOp }
