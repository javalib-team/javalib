-include ../../Makefile.config

INC_PATH=-I ../../src
BIN=test_parser

.PHONY:clean cleanall

test_parser:test_parser.ml
	$(OCAMLOPT) $(INCLUDE) $(INC_PATH) -linkpkg -w Ae -annot -o $@ javalib.cmxa $^

clean:
	$(RM) *.cm* *.o *.a *.annot *~ $(BIN)

