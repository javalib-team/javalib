
main:javatests
	make -C src/
	src/test_parser --silent javatests/huge/

javatests:
	mkdir -p out javatests
	wget -r -nH --cut-dirs=4 -np --reject="*.html,*.tmp" -e robots=off \
	https://scm.gforge.inria.fr/anonscm/svn/javalib/tests/javatests/

.PHONY:clean 

clean:
	make -C src clean
	rm -rf out javatests

