.PHONY:all ptrees javalib install clean cleanall cleandoc doc

all:ptrees javalib

ptrees:
	$(MAKE) -C ptrees

javalib:ptrees
	$(MAKE) -C src

install:
	$(MAKE) -C ptrees $@
	$(MAKE) -C src $@

cleanall clean:
	$(MAKE) -C src $@
	$(MAKE) -C ptrees $@
	$(MAKE) -C doc $@
	$(RM) *~
cleandoc doc:
	$(MAKE) -C src $@
	$(MAKE) -C doc $@
