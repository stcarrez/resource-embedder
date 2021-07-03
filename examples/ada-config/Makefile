GNATMAKE=gnatmake
CFLAGS=-g -O
ARE=../../bin/are

all:  src/config.adb
	$(GNATMAKE) -O2 -g src/show_config.adb

src/config.adb: config/example.conf
	$(ARE) --lang=Ada -o src --var-access --resource=config --name-access --fileset '**/*' config

clean:
	rm -f src/config.adb src/config.ads show_config *.ali *.o *.ads *.adb
