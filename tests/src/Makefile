-include ../../Makefile.config

INC_PATH=-I ../../src
BIN=jar_parser unparser test_parser replace_dynamics test_collections

.PHONY:clean cleanall

all: $(BIN)

%:%.ml ../../src/javalib.cmxa
	$(OCAMLOPT) $(INCLUDE) $(INC_PATH) -linkpkg -w Ae -annot -o $@ javalib.cmxa $^

clean:
	$(RM) *.cm* *.o *.a *.annot *~ $(BIN)

