CC=cc
CFLAGS=-g -O
ARE=../../bin/are

all:  src/scripts.c
	$(CC) -o show-script $(CFLAGS) src/show-script.c

src/scripts.c: Makefile
	$(ARE) --lang=c -o src --var-access --rule=package.xml .

clean:
	rm -f src/scripts.c src/scripts.h show-script *.o
