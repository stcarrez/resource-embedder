CC=cc
CFLAGS=-g -O
ARE=are

all:  src/help.c
	$(CC) -o show-help $(CFLAGS) src/show-help.c src/help.c src/man.c

src/help.c: Makefile package.xml
	$(ARE) --list-access --name-access --lang=c -o src --rule=package.xml .

clean:
	rm -f src/help.c src/help.h show-help
