MPVERSION=nat_sem_problem
STUDENTSRC=$(MPVERSION).ml  # Shouldn't we start with the json file?

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

MODULE_COMMON=common


GRADER_NAME=grader

MODULE_STUDENT=student
MODULE_SOLUTION=solution
MODULE_COMMON=common
OBJLANG=picoml


OBJECTS=genutils.cmo gencheck.cmo $(MODULE_COMMON).cmo $(MODULE_SOLUTION).cmo interface.cmo parse.cmo lex.cmo treedata.cmo $(MODULE_STUDENT).cmo natSemCheck.cmo

all: $(GRADER_NAME)

##all: $(MODULE_COMMON).cmo parse.cmo lex.cmo

STUDENT_CLEAN=$(OBJECTS) *.cm? $(MODULE_STUDENT).ml  parse.ml parse.mli lex.ml $(GRADER_NAME)

$(GRADER_NAME): $(OBJECTS)
	$(OCAMLC) -o $(GRADER_NAME) $(OBJECTS) 

natSemCheck.cmo: genutils.cmo gencheck.cmo $(MODULE_COMMON).cmo interface.cmo treedata.cmo $(MODULE_SOLUTION).cmo $(MODULE_STUDENT).cmo natSemCheck.ml
	$(OCAMLC) -c natSemCheck.ml

parse.cmo: $(MODULE_COMMON).cmo parse.mly
	$(OCAMLYACC) parse.mly
	$(OCAMLC) -c parse.mli
	$(OCAMLC) -c parse.ml

lex.cmo: parse.cmo lex.mll
	$(OCAMLLEX)  lex.mll
	$(OCAMLC) -c lex.ml

treedata.cmo: genutils.cmo $(MODULE_COMMON).cmo parse.cmo lex.cmo treedata.ml
	$(OCAMLC) -c treedata.ml

interface.cmo: genutils.cmo gencheck.cmo solution.cmo interface.ml
	$(OCAMLC) -c interface.ml

gencheck.cmo: genutils.cmo gencheck.ml
	$(OCAMLC) -c gencheck.ml

$(MODULE_COMMON).cmo: genutils.cmo $(MODULE_COMMON).ml
	$(OCAMLC) -c $(MODULE_COMMON).ml

#processing.cmo: genutils.cmo $(MODULE_COMMON).cmo parse.cmo lex.cmo processing.ml
#	$(OCAMLC) -c processing.ml 

$(MODULE_SOLUTION).cmo: $(MODULE_SOLUTION).ml
	$(OCAMLC) -c $(MODULE_SOLUTION).ml

$(MODULE_STUDENT).cmo: genutils.cmo $(MODULE_STUDENT).ml
	$(OCAMLC) -c $(MODULE_STUDENT).ml

$(MODULE_STUDENT).ml: $(STUDENTSRC)
	cp $(STUDENTSRC) $(MODULE_STUDENT).ml

genutils.cmo: genutils.ml
	$(OCAMLC) -c genutils.ml

clean:
	rm -fr $(STUDENT_CLEAN)

dist-clean: clean

