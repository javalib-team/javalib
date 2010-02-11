# This file is part of JavaLib
# Copyright (c)2004 Nicolas Cannasse
# Copyright (c)2007 Tiphaine Turpin (Université de Rennes 1)
# Copyright (c)2007, 2008, 2009 Laurent Hubert (CNRS)
# Copyright (c)2009 Nicolas Barre (INRIA)
#
# This program is free software: you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public License
# as published by the Free Software Foundation, either version 3 of
# the License, or (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# Lesser General Public License for more details.
# 
# You should have received a copy of the GNU Lesser General Public
# License along with this program.  If not, see 
# <http://www.gnu.org/licenses/>.



-include ../Makefile.config
DOCDIR:=../$(DOCDIR)

CSS=\"style.css\"

# ------ 
MODULES=jBasics jBasicsLow jCode jClass jDumpBasics jDumpLow jParseCode		\
jInstruction jUnparseSignature jHigh2Low jDumpJasmin jUnparse jParseSignature	\
jAnnotation jLow2High jParse jFile jPrint javalib

MODULE_INTERFACES=jBasics jBasicsLow jCode jClassLow jSignature jClass		\
jDumpBasics jDumpLow jDumpJasmin jParseCode jInstruction jUnparseSignature	\
jUnparse jParseSignature jParse jAnnotation jLow2High jHigh2Low jFile javalib

.SUFFIXES : .cmo .cmx .cmi .ml .mli
.PHONY : all install clean cleanall cleandoc doc remove

all: javalib.cma javalib.cmxa

install: META javalib.cma javalib.cmxa $(MODULE_INTERFACES:=.mli) $(MODULE_INTERFACES:=.cmi)
	$(INSTALL) javalib $^ javalib.a
remove:
	ocamlfind remove javalib

ocaml:javalib.cma
	$(OCAMLMKTOP) $(INCLUDE) -linkpkg -o $@ ptrees.cma javalib.cma

# compilation
jParseSignature.cmo:jParseSignature.ml
	$(OCAMLC) $(INCLUDE) $(PP) -c $<
jParseSignature.cmx jParseSignature.o:jParseSignature.ml
	$(OCAMLOPT) $(INCLUDE) $(PP) -c $<

.ml.cmo:
	$(OCAMLC) $(INCLUDE) -c $<
%.cmx %.o:%.ml
	$(OCAMLOPT) $(INCLUDE) -c $<
.mli.cmi:
	$(OCAMLC) $(INCLUDE) -c $<

javalib.cma: ../ptrees/ptrees.cma $(MODULE_INTERFACES:=.cmi) $(MODULES:=.cmo)
	$(OCAMLC) $(INCLUDE) -a $(MODULES:=.cmo) -o $@

javalib.cmxa: ../ptrees/ptrees.cmxa $(MODULE_INTERFACES:=.cmi) $(MODULES:=.cmx)
	$(OCAMLOPT) $(INCLUDE) -a $(MODULES:=.cmx) -o $@

../ptrees/ptrees.cma ../ptrees/ptrees.cmxa:$(wildcard ../ptrees/*.ml ../ptrees/*.mli)
	$(MAKE) -C $(@D) $(@F)

# doc
doc: $(MODULE_INTERFACES:=.cmi) $(MODULES:=.ml) ../intro.ocamldoc
	mkdir -p $(DOCDIR)
	$(OCAMLDOC) $(RECODE) $(INCLUDE) -d $(DOCDIR) -html -stars		\
		-colorize-code -intro ../intro.ocamldoc -t Javalib	\
		$(MODULE_INTERFACES:=.mli) ../ptrees/ptmap.mli		\
		 ../ptrees/ptset.mli
	echo "</body></html>" >> $(DOCDIR)/index.html # workarround for a bug in ocamldoc

# cleaning
clean:
	$(RM) .depend *.cmi *.cmo *.cmx *.annot *.obj *.o *.a *~

cleanall: clean
	$(RM) -r ocaml tests tests.opt sample sample.opt *.cmi	\
		*.cma *.cmxa
cleandoc:
	$(RM) -r $(DOCDIR)

# Dependencies
.depend:../Makefile.config $(MODULE_INTERFACES:=.mli) $(MODULES:=.ml)
	$(OCAMLDEP) $(PP) $^ > $@

ifneq ($(MAKECMDGOALS),clean)
ifneq ($(MAKECMDGOALS),cleanall)
ifneq ($(MAKECMDGOALS),cleandoc)
-include .depend
endif
endif
endif
