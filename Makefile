.PHONY:all ptrees installptrees javalib install clean cleanall cleandoc doc

# should do : 
all:javalib

ptrees:
	$(MAKE) -C ptrees

javalib:ptrees
	$(MAKE) -C src

installptrees removeptrees:%ptrees:
	$(MAKE) -C ptrees $*
install remove:
	$(MAKE) -C src $@

cleanall clean:
	$(MAKE) -C src $@
	$(MAKE) -C ptrees $@
	$(MAKE) -C doc $@
	$(RM) *~
cleandoc doc:
	$(MAKE) -C src $@
	$(MAKE) -C doc $@
