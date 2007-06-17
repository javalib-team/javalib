
#  This file is part of JavaLib
#  Copyright (c)2004 Nicolas Cannasse
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

# last modified by eandre@irisa.fr 19/05/2006

# Installation directory
prefix=/usr/local
exec_prefix=$(prefix)
libdir=$(exec_prefix)/lib

# Dependencies
EXTLIB_PATH = /usr/lib/ocaml/site-packages/extlib/
CAMLZIP_PATH = +site-packages/camlzip

OCAMLC = ocamlc -pp camlp4o
OCAMLOPT = ocamlopt -pp camlp4o
OCAMLDOC = ocamldoc -pp camlp4o
INSTALL_DIR = $(libdir)/javaLib/
INCLUDE = -I $(EXTLIB_PATH) -I $(CAMLZIP_PATH)

# ------ 

.SUFFIXES : .cmo .cmx .cmi .ml .mli

FILES =  jDump.+ jConsts.+ jCode.+ jInstruction.+ jUnparse.+ jParse.+ jFile.+ jTest.+

all: javaLib.cma

opt: javaLib.cmxa

install: all opt
	mkdir -p $(INSTALL_DIR)
	cp -f javaLib.cma javaLib.cmxa javaLib.a jClass.cmi jParse.cmi jConsts.cmi jDump.cmi jUnparse.cmi jFile.cmi $(INSTALL_DIR)
	cp -f jClass.mli jConsts.mli jParse.mli jDump.mli jUnparse.mli jFile.mli $(INSTALL_DIR)

sample:
	$(OCAMLC)  $(INCLUDE) extLib.cma javaLib.cma sample.ml -o sample.exe

sample.opt:
	$(OCAMLOPT)  $(INCLUDE) extLib.cmxa javaLib.cmxa sample.ml -o sample.opt.exe

javaLib.cma:  jClass.cmi jOpCode.cmi $(FILES:+=cmi) $(FILES:+=cmo)
	$(OCAMLC)  $(INCLUDE) -a $(FILES:+=cmo) -o javaLib.cma

# javaLib: jClass.cmi $(FILES:+=cmi) $(FILES:+=cmo)
# 	$(OCAMLC)  $(INCLUDE) -custom extLib.cma $(FILES:+=cmo) -o javaLib

# exe:
# 	make javaLib
# 	chmod 775 javaLib
# 	./javaLib

javaLib.cmxa:  jClass.cmi jOpCode.cmi $(FILES:+=cmi) $(FILES:+=cmx)
	$(OCAMLOPT) -a $(FILES:+=cmx) -o javaLib.cmxa

doc: $(FILES:+=cmi)
	mkdir -p doc
	$(OCAMLDOC) -keep-code -d doc -html -stars -colorize-code -intro intro.ocamldoc -t JavaLib $(INCLUDE) \
	jClass.mli jOpCode.mli jConsts.mli jCode.mli jParse.mli jDump.mli jInstruction.mli jUnparse.mli jFile.mli $(FILES:+=ml)

clean:
	rm -rf $(FILES:+=cmo) $(FILES:+=cmx) $(FILES:+=cmi) $(FILES:+=o) $(FILES:+=obj)
	rm -rf sample.cmo sample.cmi sample.cmx sample.o sample.obj sample.exe sample.opt.exe
	rm -rf jClass.cmi jOpCode.cmi javaLib.cma javaLib.cmxa javaLib.lib javaLib.a
	rm -rf doc
	rm -rf *.ml~ *.mli~ Makefile~

.ml.cmo:
	$(OCAMLC)  $(INCLUDE) -c $*.ml 

.ml.cmx:
	$(OCAMLOPT)  $(INCLUDE) -c $*.ml 

.mli.cmi:
	$(OCAMLC)  $(INCLUDE) -c $*.mli
