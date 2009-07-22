.PHONY:ptrees all javalib cleanall doc

all:ptrees javalib

ptrees:
	$(MAKE) -C ptrees

javalib:ptrees
	$(MAKE) -C src

cleanall:
	$(MAKE) -C src cleanall
	$(MAKE) -C ptrees cleanall

doc:
	$(MAKE) -C src doc