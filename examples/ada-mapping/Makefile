GNATMAKE=gnatmake
CFLAGS=-g -O
ARE=../../bin/are

all:  src/extension_map.adb
	$(GNATMAKE) -O2 -g src/extension.adb

src/extension_map.adb: Makefile
	$(ARE) --lang=Ada -o src --rule=package.xml .

clean:
	rm -f src/extension_map.ads src/extension_map.adb extension *.ali *.o *.ads *.adb
