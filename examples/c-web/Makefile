CC=cc
CFLAGS=-g -O
ARE=../../bin/are

all:  src/web.c
	$(CC) -o show-web $(CFLAGS) src/show-web.c src/web.c

src/web.c: Makefile package.xml
	$(ARE) --list-access --name-access --lang=c -o src --rule=package.xml .

clean:
	rm -f src/web.c src/web.h show-web
