# Embedding configuration files in G

This example is an adaptation of the C example but in Go.

Note that Go provides an embed mechanism through the `embed.go`
package and the `go:embed` directive.

The configuration files are embedded by using the following
command:

```
are --lang=go -o config --resource=config --name-access --fileset '**/*.conf' config
```

The `--lang=go` option selects the Go generator for the output and the
`-o config` option indicates the target directory where files are generated.

The `--resource` option indicates the name of the resource and it is
used for the name of the target header and source file.  Hence, the tool will
generate the `config.go` source file in the `config` directory.  The `--name-access`
option tells the code generator to emit a Go function that allows to retrieve
the resource by using its name.  To describe the content that is embedded,
a Go structure is declared that gives information about the raw data content,
the content size, the modification date and data format.

The `--fileset '**/*.conf'` option is here to define the pattern for files that
must be taken into account and embedded.  Then, the `config` directory is
scanned for files matching the given pattern.   Multiple `--fileset` patterns
can be specified.


# Build

Run the command

```
make
```

and this generates:

* config/config.go: the generated Go file,
* show-config: binary that prints the configuration file

