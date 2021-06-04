GO=go
ARE=are

all:  help/help.go
	$(GO) build show-help.go

help/help.go: Makefile package.xml
	$(ARE) --lang=go --list-access --name-access --rule=package.xml .

clean:
	rm -f help/help.go man/man.go show-help
