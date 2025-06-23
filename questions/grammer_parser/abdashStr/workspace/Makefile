MPNAME=abdashStr
STUDENTSRC=student.ml

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
MODULE_SOLUTION=solution
RRG=rrg
RE=regex
REp=$(RE)p


all: $(GRADER_NAME)

OBJECTS=$(RRG).cmo $(RRG)parse.cmo $(RRG)lex.cmo $(MODULE_SOLUTION).cmo $(RRG)Grader.cmo $(RE)ast.cmo $(RE)parse.cmo $(RE)lex.cmo $(RE)int.cmo $(RE)grader.cmo grader.cmo

STUDENT_CLEAN= $(GRADER_NAME)

grader.cmo: grader.ml
	$(OCAMLC) -c grader.ml

$(GRADER_NAME): $(OBJECTS) $(STUDENTSRC)
	$(OCAMLC) -o $(GRADER_NAME) $(OBJECTS) 

$(RE)grader.cmo: $(RE)ast.cmo $(RE)parse.cmo $(RE)lex.cmo $(RE)int.cmo $(MODULE_SOLUTION).cmo $(RE)grader.ml
	$(OCAMLC) -c $(RE)grader.ml

$(RRG)Grader.cmo: $(RRG).cmo $(RRG)parse.cmo $(RRG)lex.cmo $(MODULE_SOLUTION).cmo $(RRG)Grader.ml
	$(OCAMLC) -c $(RRG)Grader.ml

$(STUDENTSRC): $(MPNAME)_$(REp) $(MPNAME)_$(RRG)
	cat $(MPNAME)_$(REp) > $(STUDENTSRC)
	echo " " >> $(STUDENTSRC)
	echo " " >> $(STUDENTSRC)
	cat $(MPNAME)_$(RRG) >> $(STUDENTSRC)


$(RE)int.cmo: $(RE)int.ml $(RE)ast.cmo
	$(OCAMLC) -c $(RE)int.ml

$(RE)parse.cmo: $(RE)parse.mly
	$(OCAMLYACC) $(RE)parse.mly
	$(OCAMLC) -c $(RE)parse.mli
	$(OCAMLC) -c $(RE)parse.ml

$(RRG)parse.cmo: $(RRG)parse.mly
	$(OCAMLYACC) $(RRG)parse.mly
	$(OCAMLC) -c $(RRG)parse.mli
	$(OCAMLC) -c $(RRG)parse.ml

$(RE)lex.cmo: $(RE)parse.cmo $(RE)lex.mll
	$(OCAMLLEX) $(RE)lex.mll
	$(OCAMLC) -c $(RE)lex.ml 

$(RRG)lex.cmo: $(RRG)parse.cmo $(RRG)lex.mll
	$(OCAMLLEX) $(RRG)lex.mll
	$(OCAMLC) -c $(RRG)lex.ml

$(RE)ast.cmo: $(RE)ast.ml
	$(OCAMLC) -c $(RE)ast.ml

$(RRG).cmo: $(RRG).ml
	$(OCAMLC) -c $(RRG).ml

clean:
	$(RM) -f $(STUDENT_CLEAN)

dist-clean: clean
	$(RM) -f $(RE)ast.cm? $(RE)lex.ml $(RE)lex.cm? $(RE)parse.cm? $(RE)parse.ml $(RE)parse.mli $(RE)int.cm? $(RE)grader.cm? $(MODULE_SOLUTION).cm?
	$(RM) -f $(RRG).cm? $(RRG)lex.ml $(RRG)lex.cm? $(RRG)parse.cm? $(RRG)parse.ml $(RRG)parse.mli $(RRG)parse.oputput $(RRG)Grader.cm? $(MODULE_SOLUTION).cm? 
	$(RM) -f grader.cmo

