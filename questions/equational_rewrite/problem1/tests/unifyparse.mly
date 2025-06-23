%{
  open UnifySteps
%}

%token <string> VAR CONST
%token EOF ARROW EQUALS LPAREN RPAREN COMMA LBRACE RBRACE
       UNIFY BY ON COMPOSE DELETE DECOMPOSE ORIENT ELIMINATE FAIL

%start term subst student_problem step full_input
%type <UnifySteps.term> term
%type <UnifySteps.substitution> subst
%type <UnifySteps.student_problem> student_problem
%type <UnifySteps.input_step> step
%type <UnifySteps.full_input> input full_input

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

middle_student_problem:
     UNIFY problem   { ($2, Subst []) }
   | UNIFY problem COMPOSE subst_seq  { ($2, $4) }

student_problem:
     middle_student_problem { (fun (p,s) -> Problem (p,s)) $1 }
/*   | subst  { Final $1 } */
   | FAIL { Fail }

step_label:
     DELETE      { Delete }
   | DECOMPOSE   { Decompose }
   | ORIENT      { Orient }
   | ELIMINATE   { Eliminate }

step:
     EQUALS student_problem BY step_label ON equation  { MidStep($2, $4, $6 ) }
   | EQUALS subst  { LastStep $2 }

input:
     UNIFY problem step { ($2, Subst [], $3) }
   | EQUALS middle_student_problem BY step_label ON equation step
       { (match $2 with (problem, subst) -> (problem, subst, $7)) }
   | middle_student_problem BY step_label ON equation step
       { (match $2 with (problem, subst) -> (problem, subst, $7)) }

full_input: 
     input EOF         { [$1] }
   | input full_input  { $1 :: $2 }
