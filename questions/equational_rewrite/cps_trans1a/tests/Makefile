PROBLEM=cps_trans1a

OCAML=ocaml
OCAMLC=ocamlc
OCAMLLEX=ocamllex
OCAMLYACC=ocamlyacc

OBJLANG=picoml
INTERACTIVE_EXE=cps_trans_tester
GRADER=grader

COMMON_OBJECTS=utils.cmo gen_repl.cmo gen_env.cmo picoml_exp_and_cps.cmo picoml_cps_parse.cmo picoml_cps_lex.cmo problem.cmo problem_points.cmo one_step_cps_trans.cmo one_step_cps_trans_rules.cmo picomlCPSTransGrader.cmo 


#all: $(INTERACTIVE_EXE)
all: $(GRADER)

$(GRADER): $(COMMON_OBJECTS) student.cmo mk_grader.cmo
	$(OCAMLC) -o $(GRADER) $(COMMON_OBJECTS) student.cmo mk_grader.cmo

mk_grader.cmo: mk_grader.ml  picomlCPSTransGrader.cmo student.cmo
	$(OCAMLC) -c mk_grader.ml

student.cmo: $(PROBLEM).ml
	cp $(PROBLEM).ml student.ml
	$(OCAMLC) -c student.ml

#$(PROBLEM).ml: $(INTERACTIVE_EXE)
#	$(INTERACTIVE_EXE)

$(INTERACTIVE_EXE): $(COMMON_OBJECTS) mk_cps_trans_tester.cmo 
	$(OCAMLC) -o $(INTERACTIVE_EXE) $(COMMON_OBJECTS) mk_cps_trans_tester.cmo

mk_cps_trans_tester.cmo: mk_cps_trans_tester.ml picomlCPSTransGrader.cmo
	$(OCAMLC) -c mk_cps_trans_tester.ml

picomlCPSTransGrader.cmo: picomlCPSTransGrader.ml gen_repl.cmo gen_env.cmo picoml_cps_parse.cmo picoml_cps_lex.cmo one_step_cps_trans.cmo one_step_cps_trans_rules.cmo problem_points.cmo problem.cmo 
	$(OCAMLC) -c picomlCPSTransGrader.ml

picoml_cps_lex.cmo: picoml_cps_lex.mll picoml_cps_parse.cmo one_step_cps_trans.cmo
	$(OCAMLLEX) picoml_cps_lex.mll
	$(OCAMLC) -c picoml_cps_lex.ml

picoml_cps_parse.cmo: picoml_cps_parse.mly picoml_exp_and_cps.cmo
	$(OCAMLYACC) picoml_cps_parse.mly
	$(OCAMLC) -c picoml_cps_parse.mli
	$(OCAMLC) -c picoml_cps_parse.ml

picoml_exp_and_cps.cmo: picoml_exp_and_cps.ml gen_env.cmo
	$(OCAMLC) -c picoml_exp_and_cps.ml

one_step_cps_trans.cmo: one_step_cps_trans.ml utils.cmo picoml_exp_and_cps.cmo gen_env.cmo
	$(OCAMLC) -c one_step_cps_trans.ml

one_step_cps_trans_rules.cmo: one_step_cps_trans_rules.ml
	$(OCAMLC) -c one_step_cps_trans_rules.ml

# picoml_type_system.cmo: picoml_type_system.ml utils.cmo picoml_exp_and_cps.cmo gen_env.cmo picoml_types.cmo
# 	$(OCAMLC) -c picoml_type_system.ml

# picoml_types.cmo: picoml_types.ml utils.cmo
# 	$(OCAMLC) -c picoml_types.ml

utils.cmo: utils.ml
	$(OCAMLC) -c utils.ml

gen_repl.cmo: gen_repl.ml
	$(OCAMLC) -c gen_repl.ml

gen_env.cmo: gen_env.ml
	$(OCAMLC) -c gen_env.ml

problem_points.cmo: problem_points.ml
	$(OCAMLC) -c problem_points.ml

problem.cmo: problem.ml
	$(OCAMLC) -c problem.ml


json.cmo: json.ml
	$(OCAMLC) -c json.ml

json_parse.cmo: json.cmo json_parse.mly
	$(OCAMLYACC) json_parse.mly
	$(OCAMLC) -c json_parse.mli
	$(OCAMLC) -c json_parse.ml

json_lex.cmo: json_parse.cmo json_lex.mll
	$(OCAMLLEX) json_lex.mll
	$(OCAMLC) -c json_lex.ml

clean:
	rm -f student.* $(PROBLEM).ml grade_report.txt $(GRADER)


dist-clean:
	rm -f *.cmo
	rm -f *.cmi
	rm -f student.* $(PROBLEM).grader
	rm -f picoml_cps_lex.ml picoml_cps_parse.ml picoml_cps_parse.mli
	rm -f json_lex.ml json_parse.ml json_parse.mli
	rm -f conflicts.txt
	rm -f grader
