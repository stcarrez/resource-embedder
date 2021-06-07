CC=cc
CFLAGS=-g -O
ARE=../../bin/are

all:  src/help.c
	$(CC) -o show-help $(CFLAGS) src/show-help.c src/help.c src/man.c

src/help.c: Makefile package.xml
	$(ARE) --lang=c -o src --list-access --name-access --rule=package.xml .

clean:
	rm -f src/help.c src/help.h show-help
