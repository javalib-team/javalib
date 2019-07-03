all:javatests java/javac java/unparsed
	make -C src/
	@rm -f java/javac/*.class
	@rm -f java/unparsed/*.class
	@rm -f java/javac/out/*
	@rm -f java/unparsed/out/*
	@rm -f java/sources.txt
	@rm -f java/classes.txt
	@find ./java/src/ -type f -name "*.java" > java/sources.txt
	bash java/javac.sh
	@find ./java/javac -type f -name "*.class" > java/classes.txt
	src/unparser java/classes.txt -out java/unparsed
	bash java/java_exec_dump.sh javac
	bash java/java_exec_dump.sh unparsed
	diff java/javac/out java/unparsed/out

	rm -f javatests/out/*
	if [ -s `cat java/javac/out/Properties` ]; then \
	src/jar_parser `cat java/javac/out/Properties` -out javatests/out --silent; \
	find ./javatests/out -type f -name '*.class' | xargs javap 1>/dev/null 2>.errors; \
	fi

	rm -f javatests/out/*
	src/test_parser --silent javatests/huge/
	find ./javatests/out -type f -name '*.class' | xargs javap 1>/dev/null 2>>.errors

	if [ -s `cat java/javac/out/JmodPath` ]; then \
	rm -rf ./javatests/jmods/*; \
	bash java/java_jmods.sh; \
	find ./javatests/jmods/classes -type f -name "*.class" > javatests/jmods/classes.txt; \
	rm -f javatests/out/*; \
	src/unparser javatests/jmods/classes.txt -out javatests/out; \
	find ./javatests/out -type f -name '*.class' | xargs javap 1>/dev/null 2>>.errors; \
	fi

	if [ -s .errors ]; then cat .errors >> /dev/stderr; exit 1; fi


javatests: javatests/huge javatests/jmods
	mkdir -p javatests

javatests/huge:
	mkdir -p javatests/out
	wget -r -nH --cut-dirs=4 -np --reject="*.html,*.tmp" -e robots=off \
	https://scm.gforge.inria.fr/anonscm/svn/javalib/tests/javatests/

javatests/jmods:
	mkdir -p javatests/jmods

java/javac:
	mkdir -p java/javac/out

java/unparsed:
	mkdir -p java/unparsed/out

.PHONY:clean 

clean cleantests:
	make -C src clean
	rm -rf javatests java/javac java/unparsed java/*.txt .errors
