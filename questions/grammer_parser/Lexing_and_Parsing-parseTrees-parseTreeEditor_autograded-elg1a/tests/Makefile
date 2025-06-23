STUDENTSRC=elg1a.json

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


all: grader

OBJECTS= ptgparse.cmo ptglex.cmo genset.cmo genmap.cmo ptgload.cmo solution.cmo ptggettree.cmo ptgcheck.cmo ptggrader.cmo


grader: student.ml $(OBJECTS)
	$(OCAMLC) -o grader $(OBJECTS) unix.cma

ptggrader.cmo:  solution.cmo ptgcheck.cmo ptgload.cmo genmap.cmo genset.cmo
	$(OCAMLC) -c ptggrader.ml

ptgcheck.cmo: solution.cmo ptgload.cmo genmap.cmo genset.cmo
	$(OCAMLC) -c ptgcheck.ml

student.ml: $(STUDENTSRC) # ptgload.cmo genmap.cmo
	cp $(STUDENTSRC) student.ml
#	$(OCAMLC) -c student.ml

ptggettree.cmo: genmap.cmo ptgload.cmo
	$(OCAMLC) -c ptggettree.ml


solution.cmo: solution.ml genmap.cmo
	$(OCAMLC) -c solution.ml

ptgload.cmo: genmap.cmo
	$(OCAMLC) -c ptgload.ml

genmap.cmo: genset.cmo
	$(OCAMLC) -c genmap.ml

genset.cmo:
	$(OCAMLC) -c genset.ml

ptglex.cmo: ptglex.mll
	$(OCAMLLEX) ptglex.mll
	$(OCAMLC) -c ptglex.ml

ptgparse.cmo:  ptgparse.mly
	$(OCAMLYACC) -v ptgparse.mly
	$(OCAMLC) -c ptgparse.mli
	$(OCAMLC) -c ptgparse.ml

clean:
	rm -f *.cmo
	rm -f *.cmi
	rm -f *~
	rm -f student.cm?
	rm -f conflicts.txt
	rm -f grader
	rm -f ptglex.ml ptgparse.mli ptgparse.ml ptgparse.output

dist-clean: clean
	rm -f student.ml
