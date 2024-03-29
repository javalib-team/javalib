# This file is part of JavaLib
# Copyright (c)2004 Nicolas Cannasse
# Copyright (c)2007 Tiphaine Turpin (Université de Rennes 1)
# Copyright (c)2007, 2008, 2009 Laurent Hubert (CNRS)
# Copyright (c)2009 Nicolas Barre (INRIA)
#
# This software is free software; you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public License
# version 2.1, with the special exception on linking described in file
# LICENSE.
# 
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# Lesser General Public License for more details.
# 
# You should have received a copy of the GNU Lesser General Public
# License along with this program.  If not, see 
# <http://www.gnu.org/licenses/>.


DOCDIR := doc/api

-include ../Makefile.config

DOCDIR:=../$(DOCDIR)

# ------

PTREES= ptrees/ptset ptrees/ptmap ptrees/genericSet ptrees/genericMap

MODULES= $(PTREES)							\
jLib jBasics jBasicsLow jSignature jCode jClass jDumpBasics jParseCode	\
jInstruction jUnparseSignature jDumpLow jHigh2Low jUnparse		\
jParseSignature jLow2High jParse jFile jManifest jPrint javalib

MODULE_INTERFACES= $(PTREES)					\
jLib jBasics jBasicsLow jSignature jCode jClassLow jClass	\
jDumpBasics jUnparseSignature jDumpLow jParseCode jInstruction	\
jUnparse jParseSignature jParse jLow2High jHigh2Low jFile jManifest javalib

FOR_PACK = -for-pack Javalib_pack

.SUFFIXES : .cmo .cmx .cmi .ml .mli
.PHONY : all install clean cleanall cleandoc doc remove
.DELETE_ON_ERROR : .depend

all: javalib.cma javalib.cmxa $(SHARED)

install: META javalib.cm[ia] javalib.cmxa $(SHARED) \
         javalib_pack.cm[iox] javalib_pack.o $(MODULE_INTERFACES:=.mli)
	$(INSTALL) javalib $^ javalib.a
remove:
	$(FINDER) remove javalib

ocaml:javalib.cma
	$(OCAMLMKTOP) $(INCLUDE) -linkpkg -o $@ javalib.cma

# compilation
.ml.cmo:
	$(OCAMLC) $(INCLUDE) -I ptrees -c $<
%.cmx %.o:%.ml
	$(OCAMLOPT) $(INCLUDE) -I ptrees $(FOR_PACK) -c $<
.mli.cmi:
	$(OCAMLC) $(INCLUDE) -I ptrees -c $<
%.ml: %.mll
	$(OCAMLLEX) $<

javalib.cma: javalib_pack.cmo
	$(OCAMLC) $(INCLUDE) -a -o $@ $^

javalib.cmxa: javalib_pack.cmx
	$(OCAMLOPT) $(INCLUDE) -a -o $@ $^

javalib.cmxs: javalib_pack.cmx
	$(OCAMLOPT) $(INCLUDE) -shared -o $@ $^

javalib_pack.cmo: $(MODULE_INTERFACES:=.cmi) $(MODULES:=.cmo)
	$(OCAMLC) $(INCLUDE) -I ptrees -pack  $(MODULES:=.cmo) -o $@

javalib_pack.cmx: $(MODULE_INTERFACES:=.cmi) $(MODULES:=.cmx)
	$(OCAMLOPT) $(INCLUDE) -I ptrees -pack $(MODULES:=.cmx) -o $@

# doc
doc: $(MODULE_INTERFACES:=.cmi) $(MODULES:=.ml) ../intro.ocamldoc
	mkdir -p $(DOCDIR)
	$(OCAMLDOC) $(RECODE) $(INCLUDE) -I ptrees -d $(DOCDIR) -html -stars 	\
		-css-style style.css -colorize-code -intro ../intro.ocamldoc	\
		-t Javalib $(MODULE_INTERFACES:=.mli)
	cp style.css $(DOCDIR)

# cleaning
clean:
	$(RM) .depend *.cmi *.cmo *.cmx *.annot *.obj *.o *.a *~ 	\
	$(RM) $(addprefix ptrees/, *.o *.a *.cm[iox] *~ *.annot)	\
	jManifest.ml

cleanall: clean
	$(RM) -r ocaml *.cmi	\
		*.cma *.cmxa *.cmxs

cleandoc:
	$(RM) -r $(DOCDIR)

# Dependencies
.depend:../Makefile.config $(MODULE_INTERFACES:=.mli) $(MODULES:=.ml) 
	$(OCAMLDEP) -I ptrees $^ > $@

ifneq ($(MAKECMDGOALS),clean)
ifneq ($(MAKECMDGOALS),cleanall)
ifneq ($(MAKECMDGOALS),cleandoc)
-include .depend
endif
endif
endif
