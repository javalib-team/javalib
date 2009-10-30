# This is the tutorial Makefile
# You need to install pandoc

all: javalib_tutorial.tex javalib_tutorial.pdf javalib-tut.html

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

clean:
	rm -f javalib_tutorial.tex javalib_tutorial.pdf javalib-tut.html