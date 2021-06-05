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
