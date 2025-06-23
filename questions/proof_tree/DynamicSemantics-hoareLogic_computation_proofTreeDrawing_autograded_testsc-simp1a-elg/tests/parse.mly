/* Use the expression datatype defined in expressions.ml: */
%{
    open Genutils
    open Common
%}

/* Define the tokens of the language: */
%token <int> INT
%token <string> IDENT
%token LBRAC RBRAC LBRACE RBRACE LPAREN RPAREN COMMA ARROW PLUS MINUS 
       TIMES SKIP 
       SEMICOLON ASSIGN IF THEN ELSE FI WHILE DO OD TRUE FALSE AND OR NOT
       LT LE GT GE EQUALS IMP EOF

/* Define the "goal" nonterminal of the grammar: */
%start program side_condition bool_exp
%type <Common.command> program
%type <Common.booleanexp> bool_exp
%type <Common.booleanexp> side_condition

%%

side_condition:
  | bool_exp                        { $1 }

  /* command parsing */

program:
| cmd                                { $1 }
| cmd SEMICOLON program              { SeqCommand ($1, $3) }

cmd:
/* | SKIP                               { SkipCommand } */
| IDENT ASSIGN exp                   { AssignCommand ($1, $3) }
| IF bool_exp THEN program ELSE program FI { IfCommand ($2, $4, $6) }
| WHILE bool_exp DO program OD             { WhileCommand ($2, $4) }
| LPAREN program RPAREN               { $2 }

  /* exp parsing */

exp:
| exp_prod                { $1 }
| exp plus_minus exp_prod { BinOpAppExp ($2, $1, $3) }

plus_minus:
| PLUS                    { IntPlusOp }
| MINUS                   { IntMinusOp }

exp_prod:
| exp_neg                 { $1 }
| exp_neg TIMES exp_prod  { BinOpAppExp (IntTimesOp, $1, $3) }

exp_neg:
| exp_atom       { $1 }
| MINUS exp_atom { MonOpAppExp (IntNegOp, $2) }

exp_atom:
| INT            { IntConst $1 }
| IDENT          { Ident $1 }
| LPAREN exp RPAREN { $2 }

  /* boolean expression parsing */

bool_exp: 
| iff            { $1}

iff:
| disj           { $1 }
| disj IMP iff { ImpExp($1, $3) }

disj:
| conj           { $1 }
| disj OR conj   { OrExp($1, $3) }

conj:
| neg            { $1 }
| conj AND neg   { AndExp($1, $3) }

neg:
| compare        { $1 }
| NOT compare    { NotExp $2 }

compare:
| exp EQUALS exp { EqExp($1, $3) }
| exp LT exp     { LessExp($1, $3) }
| exp LE exp     { LessEqExp($1, $3) }
| exp GT exp     { GreaterExp($1, $3) }
| exp GE exp     { GreaterEqExp($1, $3) }
| bool           { $1 }

bool:
| TRUE           { BoolConst true }
| FALSE          { BoolConst false }
| LPAREN bool_exp RPAREN { $2 }
