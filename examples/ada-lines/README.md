# Embedding SQLite scripts in Ada

Through this example, you'll see how to embed some SQL scripts and
make it available through an array of strings, each string containing
a separate and single SQL statement.  The scripts are exposed through
Ada global variables and the Ada program only has to iterate over
the array of strings to execute the SQL statement.

Note: the example itself does not run the SQL statement but only
prints them: the SQL statement execution is left as an exercise to
the reader :-)

The Advance Resource Embedder scans the directory and prepares the
SQLite scripts by stipping any comment, splitting the SQL statements
and preparing the result as an array of strings.

The SQL scripts are embedded by using the following
command:

```
are --lang=Ada -o src --rule=package.xml \
    --var-access --content-only --no-type-declaration .
```

The `--lang=Ada` option selects the Ada generator for the output and the
`-o src` option indicates the target directory where files are generated.

The `--rule=package.xml` option indicates to read the `package.xml` file
which describes the rules to collect the resource and embed them.

The `--var-access` option tells the generator to emit a global variable
declaration for each script that was included.  The variable name is
created by using the script name.

The `package.xml` file describes a list of resources with their content.
The root XML element is `package` and each resource is described by a `resource`
XML element.  The resource is assigned a name that will be used for the Ada
package generation.  Each resource is generated in its separate package.

To simplify the Ada example, both resources share a set of common Ada types
which are declared in the `Resources` package.  The `help` resource is assigned
the `Resources.Help` name and the `man` resource is assigned the
`Resources.Man` name.  By doing so, both resources are generated as a child
package of the `Resources` package.  This is why we use the
`--no-type-declaration`: we are using the type declared in the `Resources` package.

The following resource definition declares the `Help` resource.  It contains
an installation rule that will copy the files under the `help` directory
in the resource set.  Only files matching the `*.txt` pattern will be taken
into account.  The `format` attribute indicates that the content is assumed
to be a string (the default being a binary) and it will be available
as an access to a constant string.

```XML
<package>
  <resource name='Scripts'
            format='lines'
            type='access constant String'>
    <line-separator>;</line-separator>

    <!-- Remove new lines -->
    <line-filter>[\r\n]</line-filter>

    <!-- Remove C comments -->
    <line-filter>/\*[^/]*\*/</line-filter>

    <!-- Remove contiguous spaces after C comments removal -->
    <line-filter replace=' '>[ \t][ \t]+</line-filter>

    <install mode='copy' strip-extension='yes'>
      <fileset dir="sql">
        <include name="**/*.sql"/>
      </fileset>
    </install>
  </resource>
</package>
```

With the above description, the Ada code generator produces the
following package specification:

```Ada
package Scripts is
   type Content_Array is array (Natural range <>) of access constant String;
   Id_create_database : aliased constant Content_Array;
   Id_drop_database : aliased constant Content_Array;
private
   ...
end Scripts;
```


# Build

Run the command

```
make
```

and this generates:

* src/script.ads: specification file for the `script` resources,
* show_script: binary that prints the SQL statements

The program usage is then:

```
show_script
```

