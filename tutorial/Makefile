# This is the tutorial Makefile
# You need to install pandoc

all: javalib_tutorial.tex javalib_tutorial.pdf

javalib_tutorial.tex: javalib_tutorial.rst
	cat header.tex > javalib_tutorial.tex
	pandoc -f rst -t latex -o javalib_tutorial_part.tex javalib_tutorial.rst
	cat javalib_tutorial_part.tex >> javalib_tutorial.tex
	cat footer.tex >> javalib_tutorial.tex
	rm javalib_tutorial_part.tex

javalib_tutorial.pdf: javalib_tutorial.tex
	pdflatex javalib_tutorial.tex
	pdflatex javalib_tutorial.tex

clean:
	rm -f javalib_tutorial.tex javalib_tutorial.pdf