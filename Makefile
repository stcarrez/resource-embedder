NAME=are

-include Makefile.conf

STATIC_MAKE_ARGS = $(MAKE_ARGS) -XARE_LIBRARY_TYPE=static
SHARED_MAKE_ARGS = $(MAKE_ARGS) -XARE_LIBRARY_TYPE=relocatable
SHARED_MAKE_ARGS += -XUTILADA_BASE_BUILD=relocatable -XUTIL_LIBRARY_TYPE=relocatable
SHARED_MAKE_ARGS += -XXMLADA_BUILD=relocatable
SHARED_MAKE_ARGS += -XLIBRARY_TYPE=relocatable

include Makefile.defaults

# Build executables for all mains defined by the project.
build-test::	setup
	$(GNATMAKE) $(GPRFLAGS) -p -P$(NAME)_tests $(MAKE_ARGS)

# Build and run the unit tests
test:	build
	bin/are_harness -xml are-aunit.xml


$(eval $(call ada_library,$(NAME)))

ARE_DOC= \
  title.md \
  pagebreak.tex \
  index.md \
  pagebreak.tex \
  Installation.md \
  pagebreak.tex

DOC_OPTIONS=-f markdown -o are-book.pdf --listings --number-sections --toc
HTML_OPTIONS=-f markdown -o are-book.html --listings --number-sections --toc --css pandoc.css

$(eval $(call pandoc_build,are-book,$(ARE_DOC),))
