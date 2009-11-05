install:
	ocamlfind query zip 2>/dev/null \
	 || ocamlfind install zip META `ocamlc -where`/zip/* \
	 && rm -rf `ocamlc -where`/zip/
