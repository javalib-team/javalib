.PHONY:ptrees all javalib cleanall doc

all:ptrees javalib

ptrees:
	$(MAKE) -C ptrees

javalib:ptrees
	$(MAKE) -C src

cleanall clean:
	$(MAKE) -C src $@
	$(MAKE) -C ptrees $@

doc:
	$(MAKE) -C src doc
cleandoc:
	$(MAKE) -C src cleandoc