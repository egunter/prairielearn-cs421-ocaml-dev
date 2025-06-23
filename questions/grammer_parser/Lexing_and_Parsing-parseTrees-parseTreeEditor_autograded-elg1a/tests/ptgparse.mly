/* Use the expression datatype defined in expressions.ml: */
%{

(* You may want to add extra code here *)

%}


/* Define the tokens of the language: */
%token <int> INT
%token <float> FLOAT
%token <string> NODE STRING
%token NOPARSETREE PARSETREE TRUE FALSE TO LABEL META XPOS YPOS
       LBRACK RBRACK LBRACE RBRACE COLON COMMA EOF

/* Define the "goal" nonterminal of the grammar: */
%start main nodelist nodenames meta
%type <(bool * ((string * ((string list) * string * (float * float))) list))> main
%type < ((string * ((string list) * string * (float * float))) list)> nodelist
%type <string list> nodenames
%type <(float * float)> meta
%%

main:
  | LBRACE NOPARSETREE COLON bool COMMA PARSETREE COLON LBRACE nodelist RBRACE RBRACE EOF { ($4, $9) }
  | LBRACE NOPARSETREE COLON bool COMMA PARSETREE COLON LBRACE RBRACE RBRACE EOF { ($4, []) }

bool:
  | TRUE                                           { true }
  | FALSE                                          { false }

nodelist:
  | node                                           { [$1] }
  | node COMMA nodelist                            { $1 :: $3 }

node:
  | NODE COLON LBRACE TO COLON LBRACK nodenames RBRACK COMMA LABEL COLON STRING COMMA meta RBRACE
                                                  { ($1, ($7, $12, $14)) }
  | NODE COLON LBRACE TO COLON LBRACK RBRACK COMMA LABEL COLON STRING COMMA meta RBRACE
                                                  { ($1, ([], $11, $13)) }
nodenames:
  | NODE                                          { [$1]}
  | NODE COMMA nodenames                          { $1 :: $3 }

meta:
  | META COLON LBRACE XPOS COLON STRING COMMA YPOS COLON STRING RBRACE { (float_of_string $6, float_of_string $10) }
