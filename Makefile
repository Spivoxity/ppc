# ppcu/Makefile

include config.mk

all: $(PPC)

ALLARCH = ppc-amd64 ppc-mips ppc-arm ppc-thumb ppc-risc86

TOOLS = tools

COMMON = util.cmo optree.cmo dict.cmo tree.cmo lexer.cmo \
	parser.cmo check.cmo simp.cmo jumpopt.cmo \
	regs.cmo share.cmo coder.cmo tgen.cmo main.cmo

all-arch: $(ALLARCH)

ppc-%: $(COMMON) %.cmo
	ocamlc -g lib/common.cma $^ -o $@ 

parser.ml parser.mli: parser.mly
	ocamlyacc -v parser.mly

lexer.ml: lexer.mll
	ocamllex lexer.mll

%.cmi: %.mli
	ocamlc $(MLFLAGS) -c -g $<

%.cmo: %.ml $(TOOLS)/nodexp
	ocamlc $(MLFLAGS) -c -g -pp $(TOOLS)/nodexp $<

MLFLAGS = -I lib

$(TOOLS)/nodexp:
	$(MAKE) -C $(TOOLS) $(@F)

test: force
	@echo "Say..."
	@echo "  'make test0'  to compare assembly code for ARM"
	@echo "  'make test1'  to test the native backend"
	@echo "  'make test2a' to test with qemu-arm"
	@echo "  'make test2t' to test with qemu-arm (thumb mode)"
	@echo "  'make test2m' to test with qemu-mips"

EXCLUDE = nasty
ALLSRC := $(shell ls test/*.p)
TESTSRC := $(filter-out $(EXCLUDE),$(basename $(notdir $(ALLSRC))))
OPT = -O2

SCRIPT1 = -e '1,/^(\*\[\[/d' -e '/^]]\*)/q' -e p
SCRIPT2 = -e '1,/^(\*<</d' -e '/^>>\*)/q' -e p

# test0 -- compile tests and diff object code
test0 : $(TESTSRC:%=test0-%)

test0-%: ppc-arm force
	@echo "*** Test $*.p"
	./ppc-arm $(OPT) test/$*.p >b.s
	-sed -n $(SCRIPT1) test/$*.p | diff -u -b - b.s
	@echo

# test1 -- compile tests and execute with QEMU-mips
test1 : $(TESTSRC:%=test1-%)

test1-% : pas0.o force
	@echo "*** Test $*.p"
	./$(PPC) $(PPCFLAGS) -d 1 $(OPT) test/$*.p >b.s
	as $(ASFLAGS) b.s -o b.o
	gcc b.o pas0.o -o b.out 
	./b.out >b.test
	sed -n $(SCRIPT2) test/$*.p | diff - b.test
	@echo "*** Passed"; echo

pas0.o : pas0.c
	gcc -c pas0.c

test2a : $(TESTSRC:%=test2a-%)
test2t : $(TESTSRC:%=test2t-%)
test2m : $(TESTSRC:%=test2m-%)

GCC-ARM = arm-linux-gnueabihf-gcc -marm -march=armv6

test2a-% : ppc-arm pas0-arm.o force
	@echo "*** Test $*.p"
	./ppc-arm -d 1 $(OPT) test/$*.p >b.s
	$(GCC-ARM) b.s pas0-arm.o -static -o b.out 
	qemu-arm ./b.out >b.test
	sed -n $(SCRIPT2) test/$*.p | diff - b.test
	@echo "*** Passed"; echo

pas0-arm.o: pas0.c
	$(GCC-ARM) -c $< -o $@

GCC-THUMB = arm-linux-gnueabihf-gcc -marm -march=armv6 -mthumb-interwork

test2t-% : ppc-thumb pas0-thumb.o force
	@echo "*** Test $*.p"
	./ppc-thumb -d 1 $(OPT) test/$*.p >b.s
	$(GCC-THUMB) b.s pas0-thumb.o -static -o b.out 
	qemu-arm ./b.out >b.test
	sed -n $(SCRIPT2) test/$*.p | diff - b.test
	@echo "*** Passed"; echo

pas0-thumb.o: pas0.c
	$(GCC-THUMB) -c $< -o $@

test2m-% : ppc-mips pas0-mips.o force
	@echo "*** Test $*.p"
	./ppc-mips $(OPT) test/$*.p >b.s
	mipsel-linux-gnu-as b.s -o b.o
	mipsel-linux-gnu-gcc b.o pas0-mips.o -static -o b.out 
	qemu-mipsel ./b.out >b.test
	sed -n $(SCRIPT2) test/$*.p | diff - b.test
	@echo "*** Passed"; echo

pas0-mips.o: pas0.c
	mipsel-linux-gnu-gcc -fno-pic -c $< -o $@

promote: $(TESTSRC:%=promote-%)

promote-%: force
	./ppc-arm $(OPT) test/$*.p >b.s
	sed -f promote.sed test/$*.p >test/$*.new
	mv test/$*.new test/$*.p

force:

MLGEN = parser.mli parser.ml lexer.ml

ML = $(MLGEN) optree.ml tgen.ml tgen.mli simp.ml share.ml share.mli \
	jumpopt.ml check.ml check.mli dict.ml dict.mli lexer.mli \
	main.ml main.mli optree.mli tree.ml coder.mli coder.ml \
	tree.mli util.ml mips.ml simp.mli target.mli \
	regs.mli regs.ml jumpopt.mli arm.ml risc86.ml amd64.ml thumb.ml

clean: force
	rm -f *.cmi *.cmo *.o *.output b.out b.s b.test
	rm -f $(MLGEN)
	rm -f $(ALLARCH)

depend: $(ML) $(TOOLS)/nodexp force
	(sed '/^###/q' Makefile; echo; ocamldep -pp $(TOOLS)/nodexp $(ML)) >new
	mv new Makefile

CC = gcc

###

amd64.cmo : util.cmo target.cmi regs.cmi optree.cmi main.cmi
amd64.cmx : util.cmx target.cmi regs.cmx optree.cmx main.cmx
arm.cmo : target.cmi regs.cmi optree.cmi main.cmi
arm.cmx : target.cmi regs.cmx optree.cmx main.cmx
check.cmo : util.cmo tree.cmi target.cmi optree.cmi dict.cmi check.cmi
check.cmx : util.cmx tree.cmx target.cmi optree.cmx dict.cmx check.cmi
check.cmi : tree.cmi dict.cmi
coder.cmo : util.cmo target.cmi simp.cmi share.cmi optree.cmi jumpopt.cmi \
    coder.cmi
coder.cmx : util.cmx target.cmi simp.cmx share.cmx optree.cmx jumpopt.cmx \
    coder.cmi
coder.cmi : target.cmi optree.cmi
dict.cmo : util.cmo target.cmi optree.cmi dict.cmi
dict.cmx : util.cmx target.cmi optree.cmx dict.cmi
dict.cmi : target.cmi optree.cmi
jumpopt.cmo : util.cmo optree.cmi jumpopt.cmi
jumpopt.cmx : util.cmx optree.cmx jumpopt.cmi
jumpopt.cmi : optree.cmi
lexer.cmo : util.cmo parser.cmi optree.cmi dict.cmi lexer.cmi
lexer.cmx : util.cmx parser.cmx optree.cmx dict.cmx lexer.cmi
lexer.cmi : parser.cmi optree.cmi dict.cmi
main.cmo : tree.cmi tgen.cmi target.cmi parser.cmi lexer.cmi dict.cmi \
    coder.cmi check.cmi main.cmi
main.cmx : tree.cmx tgen.cmx target.cmi parser.cmx lexer.cmx dict.cmx \
    coder.cmx check.cmx main.cmi
main.cmi : target.cmi
mips.cmo : target.cmi regs.cmi optree.cmi main.cmi
mips.cmx : target.cmi regs.cmx optree.cmx main.cmx
optree.cmo : optree.cmi
optree.cmx : optree.cmi
optree.cmi :
parser.cmo : tree.cmi optree.cmi lexer.cmi dict.cmi parser.cmi
parser.cmx : tree.cmx optree.cmx lexer.cmx dict.cmx parser.cmi
parser.cmi : tree.cmi optree.cmi dict.cmi
regs.cmo : util.cmo target.cmi regs.cmi
regs.cmx : util.cmx target.cmi regs.cmi
regs.cmi : target.cmi
risc86.cmo : target.cmi regs.cmi optree.cmi main.cmi
risc86.cmx : target.cmi regs.cmx optree.cmx main.cmx
share.cmo : target.cmi optree.cmi share.cmi
share.cmx : target.cmi optree.cmx share.cmi
share.cmi : target.cmi optree.cmi
simp.cmo : util.cmo optree.cmi simp.cmi
simp.cmx : util.cmx optree.cmx simp.cmi
simp.cmi : optree.cmi
target.cmi : optree.cmi
tgen.cmo : util.cmo tree.cmi target.cmi optree.cmi lexer.cmi dict.cmi \
    coder.cmi tgen.cmi
tgen.cmx : util.cmx tree.cmx target.cmi optree.cmx lexer.cmx dict.cmx \
    coder.cmx tgen.cmi
tgen.cmi : tree.cmi target.cmi
thumb.cmo : util.cmo target.cmi regs.cmi optree.cmi main.cmi
thumb.cmx : util.cmx target.cmi regs.cmx optree.cmx main.cmx
tree.cmo : optree.cmi dict.cmi tree.cmi
tree.cmx : optree.cmx dict.cmx tree.cmi
tree.cmi : optree.cmi dict.cmi
util.cmo :
util.cmx :
