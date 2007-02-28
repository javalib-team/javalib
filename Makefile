
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

OCAMLC = ocamlc -g
OCAMLDOC = ocamldoc
INSTALL_DIR = /home/turpin/experiences/javaLib/
EXTLIB_PATH = /usr/lib/ocaml/site-packages/extlib/
INCLUDE = -I $(EXTLIB_PATH)

# ------ 

.SUFFIXES : .cmo .cmx .cmi .ml .mli

FILES =  jDump.+ jConsts.+ jUnparse.+ jCode.+ jParse.+ jTest.+

all: javaLib.cma

opt: javaLib.cmxa

install: all opt
	-mkdir $(INSTALL_DIR)
	cp -f javaLib.cma javaLib.cmxa javaLib.a jClass.cmi jParse.cmi jConsts.cmi jDump.cmi jUnparse.cmi $(INSTALL_DIR)
	cp -f jClass.mli jConsts.mli jParse.mli jDump.mli jUnparse.mli $(INSTALL_DIR)

sample:
	$(OCAMLC) -I $(EXTLIB_PATH) extLib.cma javaLib.cma sample.ml -o sample.exe

sample.opt:
	ocamlopt -I $(EXTLIB_PATH) extLib.cmxa javaLib.cmxa sample.ml -o sample.opt.exe

javaLib.cma: jClass.cmi $(FILES:+=cmi) $(FILES:+=cmo)
	$(OCAMLC) -I $(EXTLIB_PATH) -a $(FILES:+=cmo) -o javaLib.cma

# javaLib: jClass.cmi $(FILES:+=cmi) $(FILES:+=cmo)
# 	$(OCAMLC) -I $(EXTLIB_PATH) -custom extLib.cma $(FILES:+=cmo) -o javaLib

# exe:
# 	make javaLib
# 	chmod 775 javaLib
# 	./javaLib

javaLib.cmxa: jClass.cmi $(FILES:+=cmi) $(FILES:+=cmx)
	ocamlopt -a $(FILES:+=cmx) -o javaLib.cmxa

doc: $(FILES:+=cmi)
	mkdir -p doc
	$(OCAMLDOC) -keep-code -d doc -html -stars -colorize-code -intro intro.ocamldoc -t JavaLib $(INCLUDE) \
	jClass.mli jConsts.mli jCode.mli jParse.mli jDump.mli jUnparse.mli $(FILES:+=ml)

clean:
	rm -rf $(FILES:+=cmo) $(FILES:+=cmx) $(FILES:+=cmi) $(FILES:+=o) $(FILES:+=obj)
	rm -rf sample.cmo sample.cmi sample.cmx sample.o sample.obj sample.exe sample.opt.exe
	rm -rf jClass.cmi javaLib.cma javaLib.cmxa javaLib.lib javaLib.a
	rm -rf doc
	rm -rf *.ml~ *.mli~ Makefile~

.ml.cmo:
	$(OCAMLC) -I $(EXTLIB_PATH) -c $*.ml 

.ml.cmx:
	ocamlopt -I $(EXTLIB_PATH) -c $*.ml 

.mli.cmi:
	$(OCAMLC) -I $(EXTLIB_PATH) -c $*.mli
