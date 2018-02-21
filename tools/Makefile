# tools/Makefile

all : nodexp risc86

nodexp : nclex.cmo ncparse.cmo ncmain.cmo
	ocamlc ../lib/common.cma $^ -o $@

nclex.ml : nclex.mll
	ocamllex nclex.mll

ncparse.mli ncparse.ml : ncparse.mly
	ocamlyacc -v ncparse.mly

risc86 : r86lex.cmo r86ops.cmo r86parse.cmo r86main.cmo
	ocamlc ../lib/common.cma $^ -o $@ 

r86lex.ml : r86lex.mll
	ocamllex $<

r86parse.mli r86parse.ml : r86parse.mly
	ocamlyacc -v $<

%.cmi: %.mli
	ocamlc $(MLFLAGS) -c $<

%.cmo: %.ml
	ocamlc $(MLFLAGS) -c $<

MLGEN = nclex.ml ncparse.mli ncparse.ml r86lex.ml r86parse.mli r86parse.ml

ML = $(MLGEN) ncmain.ml r86main.ml r86ops.ml

clean : force
	rm -f nodexp risc86 *.cmo *.cmi *.output $(MLGEN)

depend: $(ML) force
	(sed '/^###/q' Makefile; echo; ocamldep $(ML)) >new
	mv new Makefile

force:

MLFLAGS = -I ../lib

###

nclex.cmo : ncparse.cmi
nclex.cmx : ncparse.cmx
ncmain.cmo : ncparse.cmi nclex.cmo
ncmain.cmx : ncparse.cmx nclex.cmx
ncparse.cmo : ncparse.cmi
ncparse.cmx : ncparse.cmi
ncparse.cmi :
r86lex.cmo : r86parse.cmi
r86lex.cmx : r86parse.cmx
r86main.cmo : r86parse.cmi r86lex.cmo
r86main.cmx : r86parse.cmx r86lex.cmx
r86ops.cmo :
r86ops.cmx :
r86parse.cmo : r86ops.cmo r86parse.cmi
r86parse.cmx : r86ops.cmx r86parse.cmi
r86parse.cmi :
