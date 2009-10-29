DEST=..
all: $(DEST)/INSTALL $(DEST)/README

$(DEST)/INSTALL:install.html
	lynx -dump $< > $@

$(DEST)/README:intro.html aboutversion.html history.html documentation.html contacts.html
	echo "" > $@
	for i in $^ ; do lynx -dump "$$i" >> $@ ;done

clean:
	rm -f $(DEST)/INSTALL $(DEST)/README *~
