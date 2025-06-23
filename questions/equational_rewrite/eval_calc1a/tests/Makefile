PROBLEM=eval_calc1a

OCAML=ocaml
OCAMLC=ocamlc
OCAMLLEX=ocamllex
OCAMLYACC=ocamlyacc

OBJLANG=picoml
INTERACTIVE_EXE=eval_tester
GRADER=grader

COMMON_OBJECTS=utils.cmo gen_repl.cmo gen_env.cmo picoml_exp_and_val.cmo picoml_types.cmo picoml_type_system.cmo picomlparse.cmo picomllex.cmo problem.cmo problem_points.cmo picoml_eval.cmo picoml_eval_rules.cmo picomlEvalGrader.cmo 


#all: $(INTERACTIVE_EXE)
all: $(GRADER)

$(GRADER): $(COMMON_OBJECTS) student.cmo mk_grader.cmo
	$(OCAMLC) -o $(GRADER) $(COMMON_OBJECTS) student.cmo mk_grader.cmo

mk_grader.cmo: mk_grader.ml  picomlEvalGrader.cmo student.cmo
	$(OCAMLC) -c mk_grader.ml

student.cmo: $(PROBLEM).ml
	cp $(PROBLEM).ml student.ml
	$(OCAMLC) -c student.ml

#$(PROBLEM).ml: $(INTERACTIVE_EXE)
#	$(INTERACTIVE_EXE)

$(INTERACTIVE_EXE): $(COMMON_OBJECTS) mk_eval_tester.cmo 
	$(OCAMLC) -o $(INTERACTIVE_EXE) $(COMMON_OBJECTS) mk_eval_tester.cmo

mk_eval_tester.cmo: mk_eval_tester.ml picomlEvalGrader.cmo
	$(OCAMLC) -c mk_eval_tester.ml

picomlEvalGrader.cmo: picomlEvalGrader.ml gen_repl.cmo gen_env.cmo picomlparse.cmo picomllex.cmo picoml_eval.cmo picoml_type_system.cmo picoml_eval_rules.cmo problem_points.cmo problem.cmo 
	$(OCAMLC) -c picomlEvalGrader.ml

picomllex.cmo: picomllex.mll picomlparse.cmo picoml_eval.cmo
	$(OCAMLLEX) picomllex.mll
	$(OCAMLC) -c picomllex.ml

picomlparse.cmo: picomlparse.mly picoml_exp_and_val.cmo
	$(OCAMLYACC) picomlparse.mly
	$(OCAMLC) -c picomlparse.mli
	$(OCAMLC) -c picomlparse.ml

picoml_exp_and_val.cmo: picoml_exp_and_val.ml gen_env.cmo
	$(OCAMLC) -c picoml_exp_and_val.ml

picoml_eval.cmo: picoml_eval.ml utils.cmo picoml_exp_and_val.cmo gen_env.cmo
	$(OCAMLC) -c picoml_eval.ml

picoml_eval_rules.cmo: picoml_eval_rules.ml
	$(OCAMLC) -c picoml_eval_rules.ml

picoml_type_system.cmo: picoml_type_system.ml utils.cmo picoml_exp_and_val.cmo gen_env.cmo picoml_types.cmo
	$(OCAMLC) -c picoml_type_system.ml

picoml_types.cmo: picoml_types.ml utils.cmo
	$(OCAMLC) -c picoml_types.ml

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
	rm -f picomllex.ml picomlparse.ml picomlparse.mli
	rm -f json_lex.ml json_parse.ml json_parse.mli
	rm -f conflicts.txt
	rm -f grader
