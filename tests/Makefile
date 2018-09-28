all:javatests
	make -C src/
	src/test_parser --silent javatests/huge/

	ls out/*.class | xargs javap 1>/dev/null 2>.errors
	if [ -s .errors ]; then cat .errors >> /dev/stderr; exit 1; fi

javatests:
	mkdir -p out javatests
	wget -r -nH --cut-dirs=4 -np --reject="*.html,*.tmp" -e robots=off \
	https://scm.gforge.inria.fr/anonscm/svn/javalib/tests/javatests/

.PHONY:clean 

clean cleantests:
	make -C src clean
	rm -rf out javatests .errors
