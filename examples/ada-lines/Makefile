GNATMAKE=gnatmake
CFLAGS=-g -O
ARE=../../bin/are

all:  src/scripts.adb
	$(GNATMAKE) -O2 -g src/show_script.adb

src/scripts.adb: Makefile
	$(ARE) --lang=Ada -o src --var-access --content-only --rule=package.xml .

clean:
	rm -f src/scripts.ads src/scripts.adb show_script *.ali *.o *.ads *.adb
