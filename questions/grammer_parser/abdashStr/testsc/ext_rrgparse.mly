%{
	open Rrg
%}

%token <string> TERM NONTERM
%token EPSILON GETS OR START SYMBOL EQ
%token EOF

%start main grammar start_decl rrg_rules prods_plus
%type <Rrg.right_regular_grammar> grammar main
%type <Rrg.nonterminal> start_decl
%type <Rrg.rrg_rule list> rrg_rules
%type <Rrg.production list * Rrg.rrg_rule list> prods_plus

%%

main:
grammar EOF                 { $1 }

grammar:
  rrg_rules start_decl         { ($1, $2) }
| start_decl rrg_rules         { ($2, $1) }

ext_grammar:
  ext_rrg_rules start_decl         { ($1, $2) }
| start_decl ext_rrg_rules         { ($2, $1) }

start_decl:
   START SYMBOL EQ NONTERM   { NT $4 }

rrg_rules:
   NONTERM GETS prods_plus  { (let (prods, rules) = $3 in (List.map (fun p -> (NT $1, p)) prods)@rules) }

ext_rrg_rules:
   NONTERM GETS ext_prods_plus  { (let (prods, rules) = $3 in (List.map (fun p -> (NT $1, p)) prods)@rules) }

prods_plus:
   | EPSILON                               { ([Epsilon],[]) }
   | EPSILON OR prods_plus                 { let (prods, rules) = $3 in (Epsilon::prods, rules) }
   | TERM                                  { ([One(T $1)], []) }
   | TERM OR prods_plus                    { let (prods, rules) = $3 in (One(T $1)::prods, rules) }
   | TERM NONTERM                          { ([More (T $1, NT $2)],[]) }
   | TERM NONTERM OR prods_plus            { let (prods, rules) = $4 in (More (T $1, NT $2)::prods, rules) }
   | TERM rrg_rules                        { ([One (T $1)], $2) }
   | TERM NONTERM rrg_rules                { ([More (T $1, NT $2)], $3) }

ext_prods_plus:
   | EPSILON                               { ([[]],[]) }
   | EPSILON OR prods_plus                 { let (prods, rules) = $3 in (Epsilon::prods, rules) }
   | TERM                                  { ([One(T $1)], []) }
   | TERM OR prods_plus                    { let (prods, rules) = $3 in (One(T $1)::prods, rules) }
   | TERM NONTERM                          { ([More (T $1, NT $2)],[]) }
   | TERM NONTERM OR prods_plus            { let (prods, rules) = $4 in (More (T $1, NT $2)::prods, rules) }
   | TERM rrg_rules                        { ([One (T $1)], $2) }
   | TERM NONTERM rrg_rules                { ([More (T $1, NT $2)], $3) }
