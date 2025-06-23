%{
	open Regexast
%}

%token <char> SYM
%token EPS STAR PLUS OPTION UNION EQ LPAREN RPAREN EXIT
%token EOF

%start stmt
%type <Regexast.check> stmt

%start exp
%type <Regexast.regex> exp

%%

stmt:
	exp EQ exp { Eq($1,$3) }
	| exp { Print $1 }
	| EXIT { Exit }

exp:
	exp UNION concat_exp { union_regex $1 $3 }
	| concat_exp { $1 }

concat_exp:
	concat_exp star_exp { concat_regex $1 $2 }
	| star_exp { $1 }

star_exp:
	star_exp STAR { star_regex $1 }
	| star_exp PLUS { concat_regex $1 (star_regex $1) }
	| star_exp OPTION { union_regex $1 (Concat []) }
	| atom_exp { $1 }

atom_exp:
	SYM { Sym $1 }
	| EPS { Concat [] }
	| LPAREN exp RPAREN { $2 }
