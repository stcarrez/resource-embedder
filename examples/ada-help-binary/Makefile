GNATMAKE=gnatmake
CFLAGS=-g -O
ARE=../../bin/are

all:  src/resources-help.adb
	$(GNATMAKE) -O2 -g src/show_help.adb

src/resources-help.adb: Makefile
	$(ARE) --lang=Ada -o src --list-access --content-only --name-access --rule=package.xml .

clean:
	rm -f src/resources-help.adb src/resources-help.ads src/resources-man.ads \
          src/resources-man.adb show_help *.ali *.o *.ads *.adb
