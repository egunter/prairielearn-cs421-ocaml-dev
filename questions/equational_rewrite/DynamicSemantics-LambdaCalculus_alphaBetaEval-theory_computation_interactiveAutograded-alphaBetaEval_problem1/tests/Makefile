PROB=alphaBetaEval_problem1
STUDENTSRC=$(PROB).json
UTILS=utils

OCAMLC=ocamlc
OCAMLLEX=ocamllex
OCAMLYACC=ocamlyacc
GMAKE=make
RM=rm
CP=cp
LN=ln
MV=mv
TAR=tar
GZIP=gzip
MKDIR=mkdir

GRADER_NAME=grader

#LIBRARY_GRADER=lib/grader.cma
MODULE_STUDENT=student
MODULE_SOLUTION=solution
OBJLANG=lambda


all: $(GRADER_NAME)

OBJECTS=$(UTILS).cmo $(OBJLANG).cmo $(OBJLANG)_parse.cmo $(OBJLANG)_lex.cmo $(MODULE_SOLUTION).cmo $(MODULE_STUDENT).cmo $(OBJLANG)Grader.cmo

STUDENT_CLEAN=$(MODULE_STUDENT).cm? $(MODULE_STUDENT).ml $(MODULE_STUDENT).output $(GRADER_NAME) conflicts.txt $(PROB).grader

$(PROB).ml: $(STUDENTSRC)
	echo "Making $(PROB).ml now!"
	echo "type input = {answer: string; prompt: string; step: int}" > $(PROB).ml
	echo "type student_inputs = {inputHistory: input list}" >> $(PROB).ml
	echo "let blank = \"\";;" >> $(PROB).ml
	echo "" >> $(PROB).ml
	echo "let student_inputs = " >> $(PROB).ml
#	cat "$(STUDENTSRC)" | sed s%"\"\""%" blank "%g | sed s%":"%" = "%g | sed s%"\"{\"inputHistory\" = \"\[{\"step\""%"{ inputHistory = \[ { step "%g | sed s%",\"prompt\""%"; prompt "%g| sed s%",\"ans\""%"; answer "%g | sed s%"\"step\""%" step "%g | sed s%"},{"%"};{"%g | sed s%"\]\"}\""%"\]}"%g >> $(PROB).ml
	cat "$(STUDENTSRC)" | sed s%"\"\""%" blank "%g | sed s%":"%" = "%g | sed s%",\"prompt\""%"; prompt "%g| sed s%",\"ans\""%"; answer "%g | sed s%"\"step\""%" step "%g | sed s%"},{"%"};{"%g >> $(PROB).ml

$(MODULE_STUDENT).ml: $(PROB).ml
	cp $(PROB).ml $(MODULE_STUDENT).ml

$(GRADER_NAME): $(OBJECTS)
	$(OCAMLC) -o $(GRADER_NAME) $(OBJECTS) 

$(OBJLANG)Grader.cmo: $(OBJLANG).cmo $(OBJLANG)_parse.cmo $(OBJLANG)_lex.cmo $(MODULE_STUDENT).cmo $(OBJLANG)Grader.ml
	$(OCAMLC) -c $(OBJLANG)Grader.ml

$(MODULE_STUDENT).cmo: $(MODULE_STUDENT).ml
	$(OCAMLC) -c $(MODULE_STUDENT).ml

$(MODULE_SOLUTION).cmo: $(OBJLANG).cmo $(MODULE_SOLUTION).ml
	$(OCAMLC) -c $(MODULE_SOLUTION).ml

$(OBJLANG)_parse.cmo: $(OBJLANG).cmo $(OBJLANG)_parse.mly
	$(OCAMLYACC) $(OBJLANG)_parse.mly
	$(OCAMLC) -c $(OBJLANG)_parse.mli
	$(OCAMLC) -c $(OBJLANG)_parse.ml

$(OBJLANG)_lex.cmo: $(OBJLANG)_parse.cmo $(OBJLANG)_lex.mll 
	$(OCAMLLEX) $(OBJLANG)_lex.mll
	$(OCAMLC) -c $(OBJLANG)_lex.ml

$(OBJLANG).cmo: utils.cmo $(OBJLANG).ml
	$(OCAMLC) -c $(OBJLANG).ml

$(UTILS).cmo: $(UTILS).ml
	$(OCAMLC) -c $(UTILS).ml

clean:
	$(RM) -f $(STUDENT_CLEAN)

dist-clean: clean
	$(RM) -f $(MODULE_STUDENT).* $(PROB).ml
	$(RM) -f $(UTILS).cm? $(OBJLANG).cm? $(OBJLANG)_lex.ml $(OBJLANG)_lex.cm? $(OBJLANG)_parse.cm? $(OBJLANG)_parse.ml $(OBJLANG)_parse.mli $(OBJLANG)Grader.cm? $(MODULE_SOLUTION).cm?

