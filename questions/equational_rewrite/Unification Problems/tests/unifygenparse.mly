%{
  open UnifySteps
%}

%token <string> VAR CONST
%token EOF ARROW EQUALS LPAREN RPAREN COMMA LBRACE RBRACE
       UNIFY BY ON COMPOSE DELETE DECOMPOSE ORIENT ELIMINATE FAIL

%start term subst question_prompt student_problem step full_input
%type <UnifySteps.term> term
%type <UnifySteps.substitution> subst
%type <UnifySteps.problem * UnifySteps.subst_seq>student_problem question_prompt
/*%type <UnifySteps.student_problem> student_problem start_step*/
%type <UnifySteps.input_step> step
%type <((UnifySteps.problem * UnifySteps.subst_seq) option * (UnifySteps.input_step list))> full_input
/*%type <UnifySteps.student_problem option * (UnifySteps.input_step list)> full_input*/

%%

term:
     VAR     { Var $1 }
   | CONST   { Const ($1, [] ) }
   | CONST LPAREN terms RPAREN  { Const ($1, $3) }

terms:
     term    { [ $1 ] }
   | term COMMA terms  { $1 :: $3 }

equation:
     /*     LPAREN term EQUALS term RPAREN  { ($2 , $4) } */
     term EQUALS term       { ($1 , $3) }
   | LPAREN equation RPAREN { $2 }

equations:
     equation  { [ $1 ] }
   | equation COMMA equations { insert_uniq compare $1 $3 }

problem:
     LBRACE RBRACE   { [] }
   | LBRACE equations RBRACE  { $2 }

simple_subst:
     VAR ARROW term   { ($1 , $3) }

simple_substs:
     simple_subst  { [ $1 ] }
   | simple_subst COMMA simple_substs { update_assoc compare $1 $3 }

subst:
     LBRACE RBRACE   { [] }
   | LBRACE simple_substs RBRACE  { $2 }

subst_seq:
     subst  { Subst $1 }
   | subst COMPOSE subst_seq  { Compose ($1, $3) }

student_problem:
   | UNIFY problem   { ($2, Subst []) }
   | UNIFY problem COMPOSE subst_seq  { ($2, $4) }

step_label:
     DELETE      { Delete }
   | DECOMPOSE   { Decompose }
   | ORIENT      { Orient }
   | ELIMINATE   { Eliminate }

step:
     EQUALS student_problem BY step_label ON equation  { MidStep($2, $4, $6, true) }
   | student_problem BY step_label ON equation  { MidStep($1, $3, $5, false) }

question_prompt:
step                 { match $1 with MidStep((prob,subst),_,_,_) -> (prob,subst)
                       | _ -> raise (Failure "Step must match")}
   | student_problem      { $1 }

last_step:
     EQUALS subst                            { LastStep $2 }
   | subst                                   { LastStep $1 }
   | EQUALS FAIL BY step_label ON equation   { FailStep($4, $6, true) }
   | FAIL BY step_label ON equation          { FailStep($3, $5, false) }

steps:
     step              { [$1]}
   | steps step        { $1 @ [$2] }

input:
     steps                           { (None, $1) }
   | last_step                       { (None, [$1]) }
   | steps last_step                 { (None, ($1 @ [$2])) }
   | student_problem steps                { (Some $1, $2) }
   | student_problem last_step            { (Some $1, [$2]) }
   | student_problem steps last_step      { (Some $1, ($2 @ [$3])) }

full_input: 
     input EOF         { $1 }
