-include ../Makefile.config

JAVALIB_PATH=../src

INC=-I $(JAVALIB_PATH)

LIBS=.libs
CAMLLIBS=jNativeStubs.cmo

# JDK_PATH=/System/Library/Frameworks/JavaVM.framework/Versions/1.5.0/Home/
INCLUDES=-I ${JDK_PATH}/include -I ${JDK_PATH}/include/linux


all: NativeStubsGen.cmo NativeStubsGen.cmx

tracer: $(LIBS)/libCallsTracer.so

$(LIBS)/libCallsTracer.so: CallsTracer.c
	if [[ ! ( -d $(LIBS) ) ]]; then mkdir $(LIBS); fi
	libtool --mode=compile gcc -c CallsTracer.c ${INCLUDES} -o ${LIBS}/CallsTracer.lo
	libtool --mode=link gcc -rpath ${PWD} ${INCLUDES} ${LIBS}/CallsTracer.lo -o libCallsTracer.la
# sous MacOS, la ligne suivante semble donner des résultats un peu
# meilleur (i.e. ça va plus loin dans la compilation)
# gcc -o CallsTracer.so -c ${INCLUDES} CallsTracer.c \
#   && ld -o libCallsTracer.so -arch i386 -dylib -undefined dynamic_lookup -ldylib1.o CallsTracer.o
stubs: NativeStubsGen.cmo NativeStubsGen.cmx

NativeStubsGen.cmo: NativeStubsGen.ml NativeStubsGen.mli
	ocamlc $(INC) -pp camlp4o -o NativeStubsGen.cmo $(CAMLLIBS) NativeStubsGen.mli NativeStubsGen.ml 

NativeStubsGen.cmx: NativeStubsGen.ml NativeStubsGen.mli
	ocamlopt $(INC) -pp camlp4o -o NativeStubsGen.cmx $(CAMLLIBS:.cmo=.cmx) NativeStubsGen.mli NativeStubsGen.ml 

clean:
	rm -rf $(LIBS) *.la *.cm* *.o
