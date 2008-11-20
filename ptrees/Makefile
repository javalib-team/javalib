OCAMLC=ocamlc.opt
OCAMLOPT=ocamlopt.opt
OCAMLDEP=ocamldep.opt

OCAMLOPTFLAGS=$(INCLUDES) $(OPT_FLAGS)
OCAMLFLAGS=$(INCLUDES) -noassert

MODULES=ptmap.ml ptset.ml
MODULE_INTERFACES=ptmap.mli ptset.mli

all:ptrees.cma ptrees.cmxa

.PHONY : default all clean cleanall doc


ptrees.cma:$(MODULES:.ml=.cmo)
	$(OCAMLC) $(OCAMLFLAGS) -o $@ -a $^

ptrees.cmxa:$(MODULES:.ml=.cmx)
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -o $@ -a $^

.SUFFIXES: .ml .mli .cmo .cmi .cmx .cma .cmxa

.ml.cmo:.depend Makefile
	$(OCAMLC) $(OCAMLFLAGS) -c $< 
.mli.cmi:.depend Makefile
	$(OCAMLC) $(OCAMLFLAGS) -c $<
.ml.cmx:.depend Makefile
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -c $< 

# Clean up
clean:
	rm -f *.o *.a *.cm[iox] *~
cleanall:clean
	rm -f *.cma *.cmxa

# Dependencies
.depend:Makefile $(MODULE_INTERFACES) $(MODULES)
	$(OCAMLDEP) $(MODULE_INTERFACES) $(MODULES) > $@

ifneq ($(MAKECMDGOALS),clean)
ifneq ($(MAKECMDGOALS),cleanall)
-include .depend
endif
endif
