MPNAME=problem1
STUDENTSRC=$(MPNAME).json

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
OBJLANG=unify


all: $(GRADER_NAME)

OBJECTS=problem_points.cmo $(OBJLANG)Steps.cmo $(OBJLANG)genparse.cmo $(OBJLANG)lex.cmo $(MODULE_STUDENT).cmo $(OBJLANG)Grader.cmo

STUDENT_CLEAN=$(MODULE_STUDENT).cm? $(MODULE_STUDENT).ml $(MODULE_STUDENT).output $(MPNAME).grader $(GRADER_NAME) 

$(GRADER_NAME): $(OBJECTS)
	$(OCAMLC) -o $(GRADER_NAME) $(OBJECTS) 

$(OBJLANG)Grader.cmo: problem_points.cmo $(OBJLANG)Steps.cmo $(OBJLANG)genparse.cmo $(OBJLANG)lex.cmo $(MODULE_STUDENT).cmo $(OBJLANG)Grader.ml
	$(OCAMLC) -c $(OBJLANG)Grader.ml

$(MODULE_STUDENT).ml: $(STUDENTSRC)
		echo "type input = {answer: string; prompt: string; step: int}" > $(MODULE_STUDENT).ml
	echo "type student_inputs = {inputHistory: input list}" >> $(MODULE_STUDENT).ml
	echo "let blank = \"\";;" >> $(MODULE_STUDENT).ml
	echo "" >> $(MODULE_STUDENT).ml
	echo "let student_inputs = " >> $(MODULE_STUDENT).ml
#	cat "$(STUDENTSRC)" | sed s%"\"\""%" blank "%g | sed s%":"%" = "%g | sed s%"\"{\"inputHistory\" = \"\[{\"step\""%"{ inputHistory = \[ { step "%g | sed s%",\"prompt\""%"; prompt "%g| sed s%",\"ans\""%"; answer "%g | sed s%"\"step\""%" step "%g | sed s%"},{"%"};{"%g | sed s%"\]\"}\""%"\]}"%g >> $(MODULE_STUDENT).ml
	cat "$(STUDENTSRC)" | sed s%"\"\""%" blank "%g | sed s%":"%" = "%g | sed s%",\"prompt\""%"; prompt "%g| sed s%",\"ans\""%"; answer "%g | sed s%"\"step\""%" step "%g | sed s%"},{"%"};{"%g >> $(MODULE_STUDENT).ml

$(MODULE_STUDENT).cmo: $(MODULE_STUDENT).ml
	$(OCAMLC) -c $(MODULE_STUDENT).ml

problem_points.cmo: problem_points.ml
	$(OCAMLC) -c problem_points.ml

$(OBJLANG)genparse.cmo: $(OBJLANG)Steps.cmo $(OBJLANG)genparse.mly
	$(OCAMLYACC) $(OBJLANG)genparse.mly
	$(OCAMLC) -c $(OBJLANG)genparse.mli
	$(OCAMLC) -c $(OBJLANG)genparse.ml

$(OBJLANG)lex.cmo: $(OBJLANG)genparse.cmo $(OBJLANG)lex.mll 
	$(OCAMLLEX) $(OBJLANG)lex.mll
	$(OCAMLC) -c $(OBJLANG)lex.ml

$(OBJLANG)Steps.cmo: $(OBJLANG)Steps.ml
	$(OCAMLC) -c $(OBJLANG)Steps.ml

clean:
	$(RM) -f $(STUDENT_CLEAN)
	$(RM) -f $(MODULE_STUDENT).*

dist-clean: clean
	$(RM) -f $(MODULE_STUDENT).*
	$(RM) -f $(OBJLANG)Steps.cm? $(OBJLANG)lex.ml $(OBJLANG)lex.cm? $(OBJLANG)genparse.cm? $(OBJLANG)genparse.ml $(OBJLANG)genparse.mli $(OBJLANG)Grader.cm?

