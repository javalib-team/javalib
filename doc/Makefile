DEST=..
doc: $(DEST)/INSTALL $(DEST)/README
all:doc

$(DEST)/INSTALL:install.html
	lynx -dump $< > $@

$(DEST)/README:intro.html aboutversion.html history.html documentation.html contacts.html
	echo "" > $@
	for i in $^ ; do lynx -dump "$$i" >> $@ ;done

clean:
	rm -f *~
cleanall:clean
cleandoc:clean
	rm -f $(DEST)/INSTALL $(DEST)/README
