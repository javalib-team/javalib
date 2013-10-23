-include Makefile.config

.PHONY:all ptrees javalib install clean cleanall cleandoc doc

# should do : 
all:javalib

javalib:
	$(MAKE) -C src

# Package-specific targets

ptrees installptrees removeptrees:%ptrees:
	$(MAKE) -C ptrees $*

install remove:
	$(MAKE) -C src $@

distclean:clean
	$(RM) Makefile.config

cleanall clean:
	$(MAKE) -C src $@
	$(MAKE) -C ptrees $@
	$(MAKE) -C doc $@
	$(RM) *~

cleandoc doc:
	$(MAKE) -C src $@

# Documentation for release (INSTALL and README)
cleandocr docr:
	$(MAKE) -C doc $@
