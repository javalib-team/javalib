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
	cat java/javac/out/Properties | xargs src/jar_parser -out javatests/out
	find . -type f -name '*.class' | xargs javap 1>/dev/null 2>.errors
	if [ -s .errors ]; then cat .errors >> /dev/stderr; exit 1; fi

javatests:
	mkdir -p javatests/out

java/javac:
	mkdir -p java/javac/out

java/unparsed:
	mkdir -p java/unparsed/out

.PHONY:clean 

clean cleantests:
	make -C src clean
	rm -rf javatests java/javac java/unparsed java/*.txt .errors
