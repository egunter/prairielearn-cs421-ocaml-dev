PROBLEM=ocaml-comp-basicEnvEval-v1

OCAMLC=ocamlc
OCAMLLEX=ocamllex
OCAMLYACC=ocamlyacc

OBJLANG=picoml
SOURCE=picomlEnvTest
INTERACTIVE_EXE=grader

all: $(INTERACTIVE_EXE)

$(INTERACTIVE_EXE): picoml_eval.cmo picomlparse.cmo picomllex.cmo picomlEnvChecker.cmo student.cmo $(SOURCE).ml
	$(OCAMLC) -c $(SOURCE).ml
	$(OCAMLC) -o $(INTERACTIVE_EXE) picoml_eval.cmo picomlparse.cmo picomllex.cmo picomlEnvChecker.cmo student.cmo $(SOURCE).cmo 

student.cmo: $(PROBLEM).ml
	cp $(PROBLEM).ml student.ml
	$(OCAMLC) -c student.ml

picomlEnvChecker.cmo: picoml_eval.cmo picomlparse.cmo
	$(OCAMLC) -c picomlEnvChecker.ml

picomlparse.cmo: picomlparse.mly picoml_eval.cmo 
	$(OCAMLYACC) picomlparse.mly
	$(OCAMLC) -c picomlparse.mli
	$(OCAMLC) -c picomlparse.ml

picomllex.cmo: picomllex.mll picomlparse.cmo
	$(OCAMLLEX) picomllex.mll
	$(OCAMLC) -c picomllex.ml

picoml_eval.cmo: picoml_eval.ml
	$(OCAMLC) -c picoml_eval.ml

clean:
	rm -f *.cmo
	rm -f *.cmi
	rm -f student.* $(PROBLEM).ml
	rm -f conflicts.txt
	rm -f grader

