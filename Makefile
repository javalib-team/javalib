-include Makefile.config

.PHONY:all ptrees installptrees javalib install clean cleanall cleandoc doc

# should do : 
all:javalib

javalib:
ifeq ($(MAKEDEP),)
	$(MAKE) -C src
else
	@echo "The package(s) $(MAKEDEP) don't seem to have been installed. Don't forget to rerun the configure script once the installation is complete!" | fmt
endif

installptrees removeptrees:%ptrees:
	$(MAKE) -C ptrees $*
install remove:
	$(MAKE) -C src $@

distclean:clean
	$(RM) Makefile.config

cleanall clean:
	$(MAKE) -C src $@
	$(MAKE) -C ptrees $@
	$(MAKE) -C camlzip $@
	$(MAKE) -C extlib $@
	$(MAKE) -C doc $@
	$(RM) *~

cleandoc doc:
	$(MAKE) -C src $@
	$(MAKE) -C doc $@
