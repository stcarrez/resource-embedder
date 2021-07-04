# Embedding help and documentation in Ada

This example shows how you can embed help and documentation files
in an Ada program.  This example uses a `package.xml` definition to
describe the files and the methods to collect and embed the files.
The Advance Resource Embedder scans the directory, runs the `man`
command to obtain the documentation and put the formatted result
in the final Ada source.  It also collects the pre-defined text documentation
located in the `help` directory.

The configuration files are embedded by using the following
command:

```
are --lang=Ada -o src --rule=package.xml \
    --list-access --content-only --name-access --no-type-declaration .
```

The `--lang=Ada` option selects the Ada generator for the output and the
`-o src` option indicates the target directory where files are generated.

The `--rule=package.xml` option indicates to read the `package.xml` file
which describes the rules to collect the resource and embed them.

The `--list-access` option tells the generator to emit a declaration
of a sorted Ada array of names allowing to access the resource names.

The `--name-access` option tells the code generator to emit an Ada function that allows to retrieve
the resource by using its name.  To describe the content that is embedded,
an Ada type record is normally declared but the `--no-type-declaration` will
prevent that type generation.

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
  <resource name='Resources.Help'
            format='string'
            type='access constant String'>
    <install mode='copy'>
      <fileset dir="help">
        <include name="**/*.txt"/>
      </fileset>
    </install>
  </resource>
  ...
</package>
```

With the above description, the Ada code generator produces the
following package specification:

```Ada
package Resources.Help is
   Names : constant Name_Array;
   function Get_Content (Name : String)
      return access constant String;
private
   ...
end Resources.Help;
```

The next resource definition will run an external program to get the
content that must be embedded.  The `man` directory is scanned and it
will execute the command `man #{name}` on each filename found.
That directory contains the empty files `ls`, `pwd` and `sh` and this
will run and embed the man page for these commands.

```XML
<package>
   ...
  <resource name='Resources.Man'
            format='string'>
    <install mode='exec'>
      <command output='#{dst}'>man #{name}</command>
      <fileset dir="man">
        <include name="*"/>
      </fileset>
    </install>
  </resource>
</package>
```

With the above description, the Ada code generator produces the
following package specification:

```Ada
package Resources.Man is
   Names : constant Name_Array;
   function Get_Content (Name : String)
      return Content_Access;
private
   ...
end Resources.Help;
```

Note that the resource description does not specify the type of
the function and the Ada generator will use the default type
which is `Content_Access`.  Because the Ada code generator is
called with `--no-type-declaration`, it is assumed that this
type is already declared and visible: indeed, it is declared
in the `Resources` package that is not generated but
provided by the example.

# Build

Run the command

```
make
```

and this generates:

* src/resources-help.ads: specification file for the `help` resources,
* src/resources-help.adb: body file for the `help` resources,
* src/resources-man.ads: specification file for the `man` resources,
* src/resources-man.adb: body file for the `man` resources,
* show_help: binary that prints the documentation

The program usage is then:

```
show_help
show_help ls
show_help edit
```

