# Makefile contributed by Alain Frisch

-include ../Makefile.config

MODULES = \
 enum bitSet dynArray extArray extHashtbl extList extString global IO option \
 pMap std uChar uTF8 base64 unzip refList optParse dllist

# the list is topologically sorted

MLI = $(MODULES:=.mli)
SRC = $(MLI) $(MODULES:=.ml) extLib.ml

all:bc opt
bc: 
	$(FINDER) ocamlc -g -a -o extLib.cma $(SRC)
opt: 
	$(FINDER) ocamlopt -g -a -o extLib.cmxa $(SRC)
doc:
	$(FINDER) ocamlc -c $(MODULES:=.mli)
	mkdir -p doc/
	$(FINDER) ocamldoc -sort -html -d doc/ $(MODULES:=.mli)
	cp odoc_style.css doc/style.css

install:
	cp META.txt META
	$(INSTALL) extlib META *.cmi *.cma $(MLI) $(wildcard *.cmxa) $(wildcard *.a)

remove:
	$(FINDER) remove extlib

clean:
	rm -f *.cmo *~ *.cmx *.o
	rm -Rf doc
cleanall:clean
	$(RM) *.cmi *.cma *.cmxa *.a *.annot META
