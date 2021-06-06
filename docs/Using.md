# Using Advanced Resource Embedder

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

```
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

```
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

```
<install mode='copy'>
  <fileset>
    <include name='**/*.txt'/>
  </fileset>
</install>
```


## Defining resources



## Man page

