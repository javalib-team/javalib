include ../../Makefile.config

default: launch-test

launch-test: main plugin.cmxs META.plugin META.javalib
	./main plugin

main: run.cmx main.cmx
	$(OCAMLOPT) -package findlib.dynload -linkpkg -o $@ $^

%.cmx: %.ml
	$(OCAMLOPT) -package findlib.dynload -c $<

plugin.cmxs: plugin.ml run.cmx
	$(OCAMLOPT) -I ../../src -shared -o $@  $<

META.javalib: ../../src/META Makefile
	cp $< $@
	echo 'directory = "../../src"' >> $@
