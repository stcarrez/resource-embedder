GNATMAKE=gnatmake
CFLAGS=-g -O
ARE=../../bin/are

all:  src/bundle.adb
	$(GNATMAKE) -O2 -g src/show_message.adb

src/bundle.adb: Makefile
	$(ARE) --lang=Ada -o src --name-access --content-only --rule=package.xml .

clean:
	rm -f src/bundle.adb src/bundle.ads show_config *.ali *.o *.ads *.adb
