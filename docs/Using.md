# Using Advanced Resource Embedder

To embed files and generate Ada, C or Go source file, the `Advanced Resource Embedder`
must identify the files, organize them and may be perform some transformation on these
files before their integration.  To control this process, it is possible to use some
options passed to the `are (1)` tool but a better control is achieved by using
an XML configuration file.

## Defining resources

The XML file describes a list of resources that must be generated.  It is introduced
by the `package` root XML element and each resource is represented by a `resource` XML
element.  A resource is assigned a name and composed of several installation rules
that describe how files are integrated and whether some transformations are made
before their integration.

```XML
<package>
  <resource name='Help'>
    <install mode='xxx'>
      ...
    </install>
  <resource>
  <resource name='Web'>
    <install mode='xxx'>
      ...
    </install>
    <install mode='yyy'>
      ...
    </install>
  </resource>
  ...
</package>
```

To help you in the control of the generated code, the resource description can
also define specific attributes that allow you to tune the code generator.
The following XML definition:

```XML
<package>
  <resource name='Help'
            type="man_content"
            function-name="man_get_help_content">
    <install mode='copy'>
      <fileset dir="help">
        <include name="**/*.txt"/>
      </fileset>
    </install>
  </resource>
  ...
</package>
```

creates a resource named `Help` and composed of text files located in the `help`
directory and with the `.txt` file extension.  The code generator will use
the name `man_content` for the data type that represents the file description
and it will use `man_get_help_content` for the generated function name.


## Selecting files

An important step in the configuration of the `Advanced Resource Embedder` is
the selection of files that will be embedded.  The mechanism to select files
is heavily inspired by the `ant (1)` Java builder with the notion of filesets
and patterns.

A fileset describes a collection of files stored in a directory and it uses
a set of inclusion and exclusion patterns to select files of that directory.
A fileset is described by the `fileset` XML element and it can contain several
`include` and `exclude` XML element.  Each `include` element describes a
pattern that the file must match to be taken into account.  Sometimes a file
can be matched but you want to exclude it and you will use the `exclude`
XML element to reject that file.

A pattern a either a fixed relative path or it may contain wildcards.
A single wildcard pattern applies only to a single directory and the
special notation `**/` indicates to match any child directory.

The following definition:

```XML
<fileset>
  <include name="*.html"/>
  <include name="*.css"/>
  <include name="*.js"/>
  <exclude name="test.js"/>
</fileset>
```

will select files from the directories passed to the `are` tool and it takes
into account only files with `.html`, `.css` and `.js` extension.  Child directories
are excluded as well as the `test.js` file if it exists.

A fileset can indicate a directory name by using the `dir` attribute.
In that case, the file selection will start from the directory with the given
name.

```XML
<fileset dir='web'>
  <include name="**/*.html"/>
  <include name="**/*.css"/>
  <include name="**/*.js"/>
  <exclude name="preview/**"/>
</fileset>
```

That definition scans the `web` directory for each argument passed to the `are`
tool and selects recursively all `.html`, `.css` and `.js` files.  If the `web`
directory contains a `preview` directory, that directory and any file it contains
will be excluded.

You may include and combine several `fileset` XML element to describe
complex file selection.

## Integration modes

The `Advanced Resource Embedder` provides several modes for the integration
of a file.  After files are matched, a decision must be made on the files
to integrate them in the output.  Sometimes it happens that several source files
will correspond to a single output.  For this integration, it is possible to
make some specific transformations.

The installation rule is described by the `install` XML element.
That rule in fact contains the fileset that indicates the files that
must be taken into account by the installation rule.

```XML
<install mode='copy'>
  <fileset>
    <include name='**/*.txt'/>
  </fileset>
</install>
```

The installation modes are described more into details in the [Rules](Are_Installer.md) chapter.


## Man page

