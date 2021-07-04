CC=cc
CFLAGS=-g -O
ARE=are

all:  src/config.c
	$(CC) -o show-config $(CFLAGS) src/show-config.c

src/config.c: config/example.conf
	$(ARE) --lang=c -o src --resource=config --name-access --fileset '*.conf' config

clean:
	rm -f src/config.c src/config.h show-config
