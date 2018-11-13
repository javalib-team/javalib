-include ../../Makefile.config

INC_PATH=-I ../../src
BIN=jar_parser unparser

.PHONY:clean cleanall

all: jar_parser unparser

test_parser:test_parser.ml ../../src/javalib.cmxa
	$(OCAMLOPT) $(INCLUDE) $(INC_PATH) -linkpkg -w Ae -annot -o $@ javalib.cmxa $^

jar_parser:jar_parser.ml ../../src/javalib.cmxa
	$(OCAMLOPT) $(INCLUDE) $(INC_PATH) -linkpkg -w Ae -annot -o $@ javalib.cmxa $^

unparser:unparser.ml ../../src/javalib.cmxa
	$(OCAMLOPT) $(INCLUDE) $(INC_PATH) -linkpkg -w Ae -annot -o $@ javalib.cmxa $^

clean:
	$(RM) *.cm* *.o *.a *.annot *~ $(BIN)

