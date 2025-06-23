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
       LT EQUALS EOF

/* Define the "goal" nonterminal of the grammar: */
%start memory program side_condition thing_to_eval trans_result term_mem_pair
%type <Common.memory> memory
%type <Common.cmd> program
%type <Common.bool_exp> side_condition
%type <Common.thing_to_eval> thing_to_eval
%type <Common.thing_to_eval * Common.memory> term_mem_pair
%type <Common.trans_result> trans_result

%%

trans_result:
  | memory { MemRes $1 }
  | term_mem_pair { InterRes (fst $1, snd $1) }

term_mem_pair:
  LPAREN thing_to_eval COMMA memory RPAREN { ($2, $4) }

thing_to_eval:
  | exp                             { ExpTTE $1 }
  | bool_exp                        { BoolExpTTE $1 }
  | program                             { CmdTTE $1 }

side_condition:
  | bool_exp                        { $1 }

  /* memory parsing */

memory:
| LBRACE RBRACE                      { [] }
| LBRACE memory_item_list RBRACE    { $2 }

memory_item_list:
| memory_item                        { [$1] }
| memory_item COMMA memory_item_list { $1 :: $3 }

memory_item:
| IDENT ARROW INT                    { ($1, $3) }
| IDENT ARROW MINUS INT              { ($1, -$4)}

  /* command parsing */

program:
| cmd                                { $1 }
| cmd SEMICOLON program              { Seq ($1, $3) }

cmd:
| SKIP                               { Skip }
| IDENT ASSIGN exp                   { Assign ($1, $3) }
| IF bool_exp THEN program ELSE program FI { If ($2, $4, $6) }
| WHILE bool_exp DO program OD             { While ($2, $4) }
| LPAREN program RPAREN               { $2 }

  /* exp parsing */

exp:
| exp_prod                { $1 }
| exp plus_minus exp_prod { BinOp ($2, $1, $3) }

plus_minus:
| PLUS                    { PlusOp }
| MINUS                   { MinusOp }

exp_prod:
| exp_neg                 { $1 }
| exp_neg TIMES exp_prod  { BinOp (TimesOp, $1, $3) }

exp_neg:
| exp_atom       { $1 }
| MINUS exp_atom { NegOp $2 }

exp_atom:
| INT            { Const $1 }
| IDENT          { Ident $1 }
| LPAREN exp RPAREN { $2 }

  /* boolean expression parsing */

bool_exp: 
| iff            { $1}

iff:
| disj           { $1 }
/*| disj EQUALS bool { BoolBinOp (IffOp, $1, $3) }*/

disj:
| conj           { $1 }
| conj OR disj   { BoolBinOp (OrOp, $1, $3) }

conj:
| neg            { $1 }
| neg AND conj   { BoolBinOp (AndOp, $1, $3) }

neg:
| compare        { $1 }
| NOT compare    { NotOp $2 }

compare:
| exp EQUALS exp { Compare (EqOp, $1, $3) }
| exp LT exp     { Compare (LtOp, $1, $3) }
| bool           { $1 }

bool:
| TRUE           { True }
| FALSE          { False }
| LPAREN bool_exp RPAREN { $2 }
