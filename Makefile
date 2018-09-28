-include Makefile.config

.PHONY:all javalib install clean cleanall cleandoc doc

# should do : 
all:javalib

javalib:
	$(MAKE) -C src

tests: FORCE
	$(MAKE) -C tests

# Package-specific targets

install remove:
	$(MAKE) -C src $@

distclean:clean
	$(RM) Makefile.config

cleanall:clean
	$(MAKE) -C doc clean cleanall
	$(MAKE) -C tests clean

clean:
	$(MAKE) -C src $@
	$(RM) *~

cleandoc doc:
	$(MAKE) -C src $@

# Documentation for release (INSTALL and README)
cleandocr docr:
	$(MAKE) -C doc $@

cleantests:
	$(MAKE) -C tests $@

FORCE:
