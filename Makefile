
#  This file is part of JavaLib
#  Copyright (c)2004 Nicolas Cannasse
#  Copyright (c)2007 Université de Rennes 1 / CNRS
#  Tiphaine Turpin <first.last@irisa.fr>
#  Laurent Hubert <first.last@irisa.fr>
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with this program; if not, write to the Free Software
#  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
#

include Makefile.config

OCAMLC = ocamlc.opt -w Ae -dtypes -g -pp camlp4o.opt
OCAMLOPT = ocamlopt.opt -pp camlp4o.opt
OCAMLDOC = ocamldoc.opt -pp camlp4o.opt
OCAMLDEP = ocamldep.opt -pp camlp4o.opt
OCAMLMKTOP = ocamlmktop
INCLUDE = -I $(EXTLIB_PATH) -I $(CAMLZIP_PATH)

# ------ 
MODULES= jBasics jClass jDumpBasics jDumpLow jCode jInstruction	\
jHigh2Low jDump jUnparse jLow2High jParse jFile jProgram	\
jControlFlow
MODULE_INTERFACES=jBasics jClassLow jClass jDumpBasics jDumpLow		\
jDump jCode jInstruction jUnparse jParse jLow2High jHigh2Low jFile	\
jProgram jControlFlow

.SUFFIXES : .cmo .cmx .cmi .ml .mli

all: javaLib.cma javaLib.cmxa ocaml tests

install: javaLib.cma javaLib.cmxa
	mkdir -p $(INSTALL_DIR)
	cp -f javaLib.cma javaLib.cmxa javaLib.a $(MODULE_INTERFACES:=.cmi) $(INSTALL_DIR)
	cp -f $(MODULE_INTERFACES:=.mli) $(INSTALL_DIR)

ocaml:
	$(OCAMLMKTOP) $(INCLUDE) -o $@ unix.cma zip.cma extLib.cma

tests:javaLib.cma tests.ml
	$(OCAMLC) $(INCLUDE) -o $@ unix.cma zip.cma extLib.cma javaLib.cma tests.ml
tests.opt:javaLib.cmxa tests.ml
	$(OCAMLOPT) $(INCLUDE) -o $@ unix.cmxa zip.cmxa extLib.cmxa javaLib.cmxa tests.ml

sample:
	$(OCAMLC) $(INCLUDE) extLib.cma javaLib.cma sample.ml -o $@

sample.opt:
	$(OCAMLOPT) $(INCLUDE) extLib.cmxa javaLib.cmxa sample.ml -o $@

javaLib.cma: $(MODULE_INTERFACES:=.cmi) $(MODULES:=.cmo)
	$(OCAMLC) -a $(MODULES:=.cmo) -o $@

javaLib.cmxa: $(MODULE_INTERFACES:=.cmi) $(MODULES:=.cmx)
	$(OCAMLOPT) -a $(MODULES:=.cmx) -o $@

doc: $(MODULE_INTERFACES:=.cmi) $(MODULES:=.ml)
	mkdir -p doc
	$(OCAMLDOC) $(INCLUDE) -keep-code -d doc -html -stars -colorize-code \
		-intro intro.ocamldoc -t JavaLib $(MODULE_INTERFACES:=.mli) $(MODULES:=.ml)

clean:
	rm -rf .depend *.cmi *.cmo *.cmx *.annot *.obj *.o *.a *~

cleanall: clean
	rm -rf doc ocaml tests tests.opt sample sample.opt *.cmi *.cma *.cmxa

# Dependencies
.depend:$(MODULE_INTERFACES:=.mli) $(MODULES:=.ml)
	$(OCAMLDEP) $(INCLUDES) $^ > $@
-include .depend

.ml.cmo:
	$(OCAMLC) $(INCLUDE) -c $<


%.cmx %.o:%.ml
	$(OCAMLOPT) $(INCLUDE) -c $<

.mli.cmi:
	$(OCAMLC) $(INCLUDE) -c $<
