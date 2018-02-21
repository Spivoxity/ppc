all: common.cma

LIB = print.cmo source.cmo growvect.cmo

common.cma: $(LIB)
	ocamlc -a $^ -o $@ 

%.cmi: %.mli
	ocamlc -c $< -o $@

%.cmo: %.ml
	ocamlc -c $< -o $@

clean: force
	rm -f *.cmi *.cmo common.cma

force:

###

print.cmo: print.cmi
source.cmo: source.cmi
growvect.cmo: growvect.cmi
