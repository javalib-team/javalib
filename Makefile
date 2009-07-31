.PHONY:ptrees all javalib jvmti cleanall doc

all:ptrees javalib jvmti

ptrees:
	$(MAKE) -C ptrees

javalib:ptrees
	$(MAKE) -C src

jvmti:javalib
	$(MAKE) -C jvmti

cleanall:
	$(MAKE)	-C jvmti clean
	$(MAKE) -C src cleanall
	$(MAKE) -C ptrees cleanall

doc:
	$(MAKE) -C src doc