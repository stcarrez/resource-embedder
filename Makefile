NAME=are

-include Makefile.conf

ifeq ($(OS),Windows_NT)
URIL_OS=win64
else

ARE_SYSTEM := $(shell uname -sm | sed "s- -_-g")

ifeq ($(ARE_SYSTEM),Linux_x86_64)
UTIL_OS=linux64
endif

ifeq ($(ARE_SYSTEM),Linux_i686)
UTIL_OS=linux32
endif

ifeq ($(ARE_SYSTEM),Darwin_x86_64)
UTIL_OS=macos64
endif

endif

# Be conservative and use the GNAT.Command_Line.Define_Switch without a callback.
GNAT_SWITCH?=NO_CALLBACK

STATIC_MAKE_ARGS = $(MAKE_ARGS) -XARE_LIBRARY_TYPE=static -XUTIL_OS=$(UTIL_OS) -XARE_SWITCH=$(GNAT_SWITCH)

include Makefile.defaults

# Build and run the unit tests
test:	build
	bin/are_harness -xml are-aunit.xml


$(eval $(call ada_program,$(NAME)))

install:: install-data

install-data::
	mkdir -p $(DESTDIR)$(prefix)/bin
	$(INSTALL) bin/are $(DESTDIR)$(prefix)/bin/are
	mkdir -p $(DESTDIR)$(prefix)/share/man/man1
	$(INSTALL) docs/are.1 $(DESTDIR)$(prefix)/share/man/man1/are.1

uninstall::
	rm -rf $(DESTDIR)${prefix}/bin/are
	rm -rf $(DESTDIR)${prefix}/share/man/man1/are.1

ARE_DOC= \
  title.md \
  pagebreak.tex \
  index.md \
  pagebreak.tex \
  Installation.md \
  pagebreak.tex \
  Are_Using.md \
  pagebreak.tex \
  Are_Installer.md \
  pagebreak.tex \
  Are_Generator.md \
  pagebreak.tex

DOC_OPTIONS=-f markdown -o are-book.pdf --listings --number-sections --toc
HTML_OPTIONS=-f markdown -o are-book.html --listings --number-sections --toc --css pandoc.css

$(eval $(call pandoc_build,are-book,$(ARE_DOC),\
	cp docs/Using.md docs/Are_Using.md; \
	sed -e s/^\\\#\\\#/\\\#\\\#\\\#/ docs/are.md >> docs/Are_Using.md))

DIST_DIRS=ada-util ada-el
dist::
	rm -f $(DIST_FILE)
	git archive -o $(DIST_DIR).tar --prefix=$(DIST_DIR)/ HEAD
	for i in $(DIST_DIRS); do \
	   cd $$i && git archive -o ../$$i.tar --prefix=$(DIST_DIR)/$$i/ HEAD ; \
           cd .. && tar --concatenate --file=$(DIST_DIR).tar $$i.tar ; \
           rm -f $$i.tar; \
        done
	gzip $(DIST_DIR).tar
