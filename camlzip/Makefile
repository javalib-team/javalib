### Configuration section

-include ../Makefile.config

# The name of the Zlib library.  Usually -lz
ZLIB_LIB=-lz

# The directory containing the Zlib library (libz.a or libz.so)
ZLIB_LIBDIR=/usr/local/lib

# The directory containing the Zlib header file (zlib.h)
ZLIB_INCLUDE=/usr/local/include

### End of configuration section

# The following are now in ../Makefile.config
#OCAMLC=ocamlc -g
#OCAMLOPT=ocamlopt -g
#OCAMLDEP=ocamldep
#OCAMLMKLIB=ocamlmklib

OBJS=zlib.cmo zip.cmo gzip.cmo
C_OBJS=zlibstubs.o

all: libcamlzip.a zip.cma zip.cmxa

zip.cma: $(OBJS)
	$(OCAMLMKLIB) -o zip -oc camlzip $(OBJS) \
            -L$(ZLIB_LIBDIR) $(ZLIB_LIB)

zip.cmxa: $(OBJS:.cmo=.cmx)
	$(OCAMLMKLIB) -o zip -oc camlzip $(OBJS:.cmo=.cmx) \
            -L$(ZLIB_LIBDIR) $(ZLIB_LIB)

libcamlzip.a: $(C_OBJS)
	$(OCAMLMKLIB) -oc camlzip $(C_OBJS) \
            -L$(ZLIB_LIBDIR) $(ZLIB_LIB)

.SUFFIXES: .mli .ml .cmo .cmi .cmx

.mli.cmi:
	$(OCAMLC) -c $<
.ml.cmo:
	$(OCAMLC) -c $<
.ml.cmx:
	$(OCAMLOPT) -c $<
.c.o:
	$(OCAMLC) -c -ccopt -g -ccopt -I$(ZLIB_INCLUDE) $<

clean:
	$(RM) -f *.cmo *.cmx *.annot *~
	$(RM) -f *.o .depend

cleanall:clean
	$(RM) *.cma *.cmxa *.a *.so *.cmi

install:
	$(INSTALL) \
		camlzip META zip.cma zip.cmi gzip.cmi zip.mli gzip.mli \
		libcamlzip.a $(wildcard dll*.so) zip.cmxa zip.a zip.cmx gzip.cmx


remove:
	$(FINDER) remove camlzip


.DELETE_ON_ERROR:.depend

.depend:
	gcc -MM -I`ocamlc -where` -I$(ZLIB_INCLUDE) *.c > .depend
	$(OCAMLDEP) *.mli *.ml >> .depend

-include .depend
