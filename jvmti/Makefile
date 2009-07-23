LIBS=.libs

all: $(LIBS)/libCallsTracer.so NativeStubsGen.cmo NativeStubsGen.cmx

tracer: $(LIBS)/libCallsTracer.so

$(LIBS)/libCallsTracer.so: CallsTracer.c
	if [[ ! ( -d $(LIBS) ) ]]; then mkdir $(LIBS); fi
	libtool --mode=compile gcc -c CallsTracer.c -I${JDK_PATH}/include -I${JDK_PATH}/include/linux -o ${LIBS}/CallsTracer.lo
	libtool --mode=link gcc -rpath ${PWD} ${LIBS}/CallsTracer.lo -o libCallsTracer.la

stubs: NativeStubsGen.cmo NativeStubsGen.cmx

NativeStubsGen.cmo: NativeStubsGen.ml NativeStubsGen.mli
	ocamlc -pp camlp4o -c NativeStubsGen.mli NativeStubsGen.ml

NativeStubsGen.cmx: NativeStubsGen.ml NativeStubsGen.mli
	ocamlopt -pp camlp4o -c NativeStubsGen.mli NativeStubsGen.ml

clean:
	rm -rf $(LIBS) *.la *.cm* *.o