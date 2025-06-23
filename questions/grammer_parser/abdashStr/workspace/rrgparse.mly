%{
	open Rrg
%}

%token <string> TERM NONTERM
%token EPSILON GETS OR STARTSYMBOL EQ
%token EOF

%start main ext_main ext_grammar grammar start_decl ext_rrg_rules term_list ext_prods_plus
%type <Rrg.ext_right_regular_grammar> ext_grammar ext_main
%type <Rrg.right_regular_grammar> grammar main
%type <Rrg.nonterminal> start_decl
%type <Rrg.ext_rrg_rule list> ext_rrg_rules
%type <Rrg.rrg_rule list> rrg_rules
%type <Rrg.ext_production list * Rrg.ext_rrg_rule list> ext_prods_plus
%type <Rrg.production list * Rrg.rrg_rule list> prods_plus
%type <Rrg.terminal list> term_list

%%

ext_main:
ext_grammar EOF                 { $1 }

main:
  grammar EOF                   { $1 }

grammar:
  rrg_rules start_decl         { ($1, $2) }
| start_decl rrg_rules         { ($2, $1) }

ext_grammar:
  ext_rrg_rules start_decl         { ($1, $2) }
| start_decl ext_rrg_rules         { ($2, $1) }

start_decl:
   STARTSYMBOL EQ NONTERM   { NT $3 }

ext_rrg_rules:
   NONTERM GETS ext_prods_plus  { (let (ext_prods, ext_rules) = $3 in
                                   (List.map (fun p -> (NT $1, p)) ext_prods)@ext_rules) }

term_list :
| EPSILON                                  { [] }
| EPSILON term_list                        { $2 }
| TERM                                     { [T $1] }
| TERM term_list                           { (T $1) :: $2 }

ext_prods_plus:
| term_list                                 { ([ExtOne $1],[]) }
| term_list NONTERM                         { ([ExtMore ($1, NT $2)],[]) }
| term_list OR ext_prods_plus               { let (prods, rules) = $3
                                              in (insert_uniq
                                                    ext_production_compare
                                                    (ExtOne $1)
                                                    prods, rules) }
| term_list NONTERM OR ext_prods_plus       { let (prods, rules) = $4
                                              in (insert_uniq
                                                    ext_production_compare
                                                    (ExtMore ($1, NT $2))
                                                    prods, rules) }
| term_list ext_rrg_rules                   { ([ExtOne $1],$2) }
| term_list NONTERM ext_rrg_rules           { ([ExtMore ($1, NT $2)], $3) }
| NONTERM                                   { ([ExtMore ([], NT $1)],[]) }
| NONTERM OR ext_prods_plus                 { let (prods, rules) = $3
                                              in (insert_uniq
                                                    ext_production_compare
                                                    (ExtMore ([],NT $1))
                                                    prods, rules) }
| NONTERM ext_rrg_rules                     { ([ExtMore ([],NT $1)], $2) }


rrg_rules:
   NONTERM GETS prods_plus  { (let (prods, rules) = $3 in (List.map (fun p -> (NT $1, p)) prods)@rules) }

prods_plus:
   | EPSILON                               { ([Epsilon],[]) }
   | EPSILON OR prods_plus                 { let (prods, rules) = $3 in (Epsilon::prods, rules) }
   | TERM                                  { ([One(T $1)], []) }
   | TERM OR prods_plus                    { let (prods, rules) = $3 in (One(T $1)::prods, rules) }
   | TERM NONTERM                          { ([More (T $1, NT $2)],[]) }
   | TERM NONTERM OR prods_plus            { let (prods, rules) = $4 in (More (T $1, NT $2)::prods, rules) }
   | TERM rrg_rules                        { ([One (T $1)], $2) }
   | TERM NONTERM rrg_rules                { ([More (T $1, NT $2)], $3) }
