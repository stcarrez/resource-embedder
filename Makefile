NAME=are
VERSION=1.5.1
prefix = /usr/local
exec_prefix = ${prefix}

-include Makefile.conf

ALIRE=alr --non-interactive
BUILD_COMMAND=$(ALIRE) build -- -XARE_BUILD=$(BUILD)

DIST_DIR=resource-embedder-$(VERSION)
DIST_FILE=$(DIST_DIR).tar.gz

PANDOC := $(shell which pandoc)
DYNAMO := $(shell which dynamo)

build::
	$(BUILD_COMMAND)

build-test::
	cd regtests && $(BUILD_COMMAND)

include Makefile.defaults

# Build and run the unit tests
test:	build-test
	bin/are_harness -xml are-aunit.xml

install:: install-data

install-data::
	mkdir -p $(DESTDIR)$(prefix)/bin
	$(INSTALL) bin/are $(DESTDIR)$(prefix)/bin/are
	mkdir -p $(DESTDIR)$(prefix)/share/man/man1
	$(INSTALL) man/man1/are.1 $(DESTDIR)$(prefix)/share/man/man1/are.1

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
