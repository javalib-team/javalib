# This is the tutorial Makefile
# You need to install pandoc

# Just for test on tutorial code
-include ../../Makefile.config

all: javalib_tutorial.tex javalib_tutorial.pdf javalib-tut.html

.DELETE_ON_ERROR:javalib_tutorial.tex javalib-tut.html

javalib_tutorial.tex: javalib_tutorial.md
	cat header.tex > javalib_tutorial.tex
	pandoc -f markdown -t latex -o javalib_tutorial_part.tex javalib_tutorial.md
	cat javalib_tutorial_part.tex >> javalib_tutorial.tex
	cat footer.tex >> javalib_tutorial.tex
	rm javalib_tutorial_part.tex

javalib-tut.html: javalib_tutorial.md header.html
	pandoc -s -c style-tut.css -B header.html --toc -T 'Javalib tutorial' -f markdown -t html javalib_tutorial.md | tidy -q > javalib-tut.html

javalib_tutorial.pdf: javalib_tutorial.tex
	pdflatex javalib_tutorial.tex
	pdflatex javalib_tutorial.tex
	rm -f javalib_tutorial.aux 
	rm -f javalib_tutorial.log 
	rm -f javalib_tutorial.toc 

test: tutorial.ml
	$(OCAMLC) -package javalib -c $^
	@echo "tutorial.ml file is compiling with version of Javalib installed."
	@echo "please check javalib_tutorial.md correspond to the same code."
	rm tutorial.cmi tutorial.cmo tutorial.annot

clean:
	rm -f javalib_tutorial.tex javalib_tutorial.pdf javalib-tut.html