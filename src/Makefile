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
INCLUDE:=$(INCLUDE) -I ../ptrees/

# ------ 
MODULES= jBasics jClass jDumpBasics jDumpLow jCode jInstruction		\
jUnparseSignature jHigh2Low jDump jUnparse jParseSignature jLow2High	\
jParse jFile jProgram jPrintHtml jPrint jControlFlow jCRA               \
jPrintHierarchy jRTA
MODULE_INTERFACES=jBasics jClassLow jSignature jClass jDumpBasics	\
jDumpLow jDump jCode jInstruction jUnparseSignature jUnparse		\
jParseSignature jParse jLow2High jHigh2Low jFile jProgram jCRA		\
jPrintHtml jControlFlow jPrint jPrintHierarchy jRTA

.SUFFIXES : .cmo .cmx .cmi .ml .mli
.PHONY : all install clean cleanall cleandoc doc

all: ../ptrees/ptrees.cma ../ptrees/ptrees.cmxa javaLib.cma javaLib.cmxa #ocaml tests tests.opt

../ptrees/ptrees.cma ../ptrees/ptrees.cmxa:$(wildcard ../ptrees/*.ml ../ptrees/*.mli)
	$(MAKE) -C $(@D) $(@F)

install: javaLib.cma javaLib.cmxa
	mkdir -p $(INSTALL_DIR)
	cp -f javaLib.cma javaLib.cmxa javaLib.a $(MODULE_INTERFACES:=.cmi) $(INSTALL_DIR)
	cp -f $(MODULE_INTERFACES:=.mli) $(INSTALL_DIR)

ocaml:
	$(OCAMLMKTOP) $(INCLUDE) -o $@ unix.cma zip.cma extLib.cma

tests:javaLib.cma tests.ml
	$(OCAMLC) $(INCLUDE) -o $@ unix.cma zip.cma extLib.cma ptrees.cma javaLib.cma tests.ml
tests.opt:javaLib.cmxa tests.ml
	$(OCAMLOPT) $(INCLUDE) -o $@ unix.cmxa zip.cmxa extLib.cmxa ptrees.cmxa javaLib.cmxa tests.ml

sample:
	$(OCAMLC) $(INCLUDE) extLib.cma ptrees.cma javaLib.cma sample.ml -o $@

sample.opt:
	$(OCAMLOPT) $(INCLUDE) extLib.cmxa ptrees.cmxa javaLib.cmxa sample.ml -o $@

ifeq ($(CMO),no)
javaLib.cma:$(MODULE_INTERFACES:=.mli) $(MODULES:=.ml)
	$(OCAMLC) $(PP) $(INCLUDE) -a -o $@ $^
javaLib.cmxa:$(MODULE_INTERFACES:=.mli) $(MODULES:=.ml)
	$(OCAMLOPT) $(PP) $(INCLUDE) -a -o $@ $^
else
# Dependencies
.depend:$(MODULE_INTERFACES:=.mli) $(MODULES:=.ml)
	$(OCAMLDEP) $(PP) $(INCLUDE) $^ > $@
ifneq ($(MAKECMDGOALS),clean)
ifneq ($(MAKECMDGOALS),cleanall)
-include .depend
endif
endif

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

javaLib.cma: $(MODULE_INTERFACES:=.cmi) $(MODULES:=.cmo)
	$(OCAMLC) -a $(MODULES:=.cmo) -o $@

javaLib.cmxa: $(MODULE_INTERFACES:=.cmi) $(MODULES:=.cmx)
	$(OCAMLOPT) -a $(MODULES:=.cmx) -o $@
endif

doc: $(MODULE_INTERFACES:=.cmi) $(MODULES:=.ml) intro.ocamldoc
	mkdir -p $(DOCDIR)
	$(OCAMLDOC) $(INCLUDE) -d $(DOCDIR) -html -stars		\
		-colorize-code -intro intro.ocamldoc -t JavaLib		\
		$(MODULE_INTERFACES:=.mli) ../ptrees/ptmap.mli		\
		 ../ptrees/ptset.mli

clean:
	rm -rf .depend *.cmi *.cmo *.cmx *.annot *.obj *.o *.a *~

cleanall: clean
	rm -rf ocaml tests tests.opt sample sample.opt *.cmi	\
		*.cma *.cmxa
cleandoc:
	rm -rf $(DOCDIR)
