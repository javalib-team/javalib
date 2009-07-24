.PHONY:ptrees jvmti all javalib cleanall doc

all:ptrees jvmti javalib

ptrees:
	$(MAKE) -C ptrees

jvmti:
	$(MAKE) -C jvmti

javalib:ptrees
	$(MAKE) -C src

cleanall:
	$(MAKE)	-C jvmti clean
	$(MAKE) -C src cleanall
	$(MAKE) -C ptrees cleanall

doc:
	$(MAKE) -C src doc