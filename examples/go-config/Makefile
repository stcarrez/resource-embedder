GO=go
ARE=are

all:  config/config.go
	$(GO) build show-config.go

config/config.go: config/example.conf Makefile
	$(ARE) --lang=go -o config --resource=config --name-access --fileset '**/*.conf' config

clean:
	rm -f config/config.go show-config
