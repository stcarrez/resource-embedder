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
  <resource name='Help' format='string'>
    <install mode='xxx'>
      ...
    </install>
  <resource>
  <resource name='Config' format='lines'>
    <install mode='xxx'>
      ...
    </install>
  </resource>
  <resource name='Web' format='binary'>
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

The resource content can be available in several formats by the code generator.
This format is controlled by the `format` attribute.
The following data formats are supported:

* `binary` format provides the file content as a binary data.
* `string` format provides the file content as string.
* `lines` format splits the content in several lines and according to a set
  of customisable rules.

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

## Custom headers

It is possible to add custom headers in the generated files by using the `header` XML
element within each `resource`.  Each `header` element is written verbatim in the output code.
It can contain comment or some target source code.  A `type` attribute can be defined
to limit in which file the header content is written.  By default, the header line is written
in the specification  (`.ads`, `.h`) and body files (`.adb`, `.c`).

```XML
<package>
  <resource ...>
    <header>...</header>
    <header type='spec'>...</header>
    <header type='body'>...</header>
    ...
  </resource>
  ...
</package>
```

## Controlling the lines format

The `lines` format tells the code generator to represent the content as an
array of separate lines.  For this integration, some control is available to
indicate how the content must be split and optionaly apply some filter on
the input content.  These controls are made within the XML description
by using the `line-separator` and `line-filter` description:
The `line-separator` indicates the characters that represent a line separation.
There can be several `line-separator` definition.  The `line-filter`
defines a regular expression that when matched must be replaced by an empty
string or a specified content.  The `line-filter` are applied in the order
of the XML definition.

The example below is intended to integrate an SQL scripts with:

* a separate line for each SQL statement,
* remove spurious empty lines and SQL comments.

The SQL statements are separated by `;` (semi-colon) and the `line-separator`
indicates to split lines on that character.  By splitting on the `;`, we allow to
have an SQL statement on multiple lines.

```XML
<package>
  <resource name='Scripts'
            format='lines' keep-empty-lines="no"
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

Then the first `line-filter` will remove the `\r` and `\n` characters.

The regular expression `/\*[^/]*\*/` matches a C style comment and remove it.

The last `line-filter` replaces multiple tabs and spaces by a single occurence.

By default an empty line is discarded.  This behavior can be changed by
using the `keep-empty-lines` attribute and setting the value to `true`.


## Controlling the generation

The generation can be controlled with several attributes defined on the
`resource` XML element.

```XML
<package>
  <resource name='...'
            format='...'
            type='...'
            function-name='...'
            index-type='...'
            member-content='...'
            member-time='...'
            member-length='...'
            member-format='...'
            var-prefix='...'
            keep-empty-lines='...'
            content-only='...'
            var-access='...'
            name-access='...'
            list-access='...'>
  </resource>
</package>
```

| Attribute | Type | Description                                                        |
| --------- | ---- | ------------------------------------------------------------------ |
| name   | String | Name used for the target generation (file name or Ada package name) |
| format | String | Define the format used to embed the selected files |
| type   | String | The name of the type to access content |
| function-name | String | The name of the generated function |
| index-type | String | For Ada, the type of index used for array declaration |
| var-prefix | String | The prefix to used for variable name generation when a variable must be created for each content |
| var-access | Boolean | When true, generate a variable to access the content directly through a variable |
| name-access | Boolean | When true, generate a function to access the content from the file name |
| list-access | Boolean | When true, generate a function to list the available names |

## Man page


### NAME

are - Resource embedder to include files in Ada, C/C++, Go binaries

### SYNOPSIS

*are* [ -v ] [-vv] [-V] [--tmp
*directory* ] [-k] [--keep]
    [-o
*directory* ] [-l
*lang* ] [--rule
*path* ] [--resource
*name* ]
    [--fileset
*pattern* ] [--ignore-case] [--list-access] [--var-access]
    [--var-prefix
*prefix* ] [--no-type-declaration] [--type-name
*name* ]
    [--function-name
*name* ] [--member-content
*name* ]
    [--member-length
*name* ] [--member-modtime
*name* ]
    [--member-format
*name* ] [--preelaborate]
    [--content-only] directory...


### DESCRIPTION

**are** is a tool to generate C, Ada or Go source allowing to embed files
in a binary program by compiling and linking with the compiled generated sources.

The process to use **are** is simple and composed of three steps:

* First, you describe the resources that you want to embed.
The description is either made on command line arguments or by writing an XML file.
The XML description gives more flexibility as it allows to define a transformation rule that
must be executed on the original file before being embedded.  This allows to minify a Javascript
or CSS file, compress some files and even encrypt a file before its integration.

* You run the **are** command with the your target language and rule description and you give the tool
a list of directories that must be scanned to identify the files that must be collected.
The **are** tool scan the directories according to the patterns that you have given either on
the command line or in the XML rule description.  After identifying the files, the tool applies
the rules and execute the transformations.
The **are** tool then invokes the target language generator that writes one or several files depending
on the list of resources.

* Once the files are generated, you use them in your program and add them in your build process
as they are now part of your sources.  After building your program, it now embeds the
resource files that were collected and optionally transformed.

The identification of files is made by using fileset patterns similar to the
**ant**(1) tool.  The patterns are applied to the directories that are passed to the **are** tool.
Files that match the pattern are selected and taken into account.
The pattern can be an exact relative path definition or it may contain wildcards.
Below are some examples:


*.txt
This pattern matches all files with a
*.txt* extension in the directories passed to the command.  Only the root directories are taken
into account (the
*.txt* files in sub-directories are ignored).


**/*.txt
The
***/* pattern indicates that the pattern is applied on directories recursively.
The files must then match the
**.txt* pattern to be taken into account.  Therefore, the
***/*.txt* pattern will match all
*.txt* files in any directory.


config/*.conf
This pattern will match the
*.conf* files in the
*config* directory.


web/index.html
This pattern matches a fixed path.

### OPTIONS

The following options are recognized by **are**:


-V
Prints the
*are* version.


-v
Enable the verbose mode.


-vv
Enable debugging output.


--tmp **directory**
Use the directory to build the resource files.  The default directory is
*are-generator* and it is created in the current working directory.  This option allows to
choose another path.


--keep
Keep the directory used to prepare the resource files.  By default the
*are-generator* directory (which can be overriden by the
*--tmp* option) is removed when the code generation is finished.  By keeping the
directory, you can have a look at the files and their transformations.


--output **directory**
Set the output directory path where generators writes the code.


--lang **language**
Select the target generator language.  The supported languages are
**Ada**, **C**, and
**Go**. The default language is
**Ada**. 

--rule **path**
Read the XML file that describes the resources to generate.  The use of a XML resource
file allows to use the advance features of the tool such as doing some transformations
on the input files.  The XML resource file can describe several resources and
provides mechanisms to control the generation for each of them.


--resource **name**
Define the name of the resource collection.  This option is used to create a resource
with the given name.


--fileset **pattern**
Define the pattern to match files for the resource collection.
After the
*--resource* option, this indicates the pattern to match the files for that resource.


--name-access
Generate support to query content with a name.
The code generator will declare and genrate a function which given a name
returns the embedded content if that name is known.


--list-access
Generate support to list the content names.
Most code generator will declare a variable that represents a sorted list of
names which represents the resource.  It is possible to use a dichotomic
search on that name array.


--var-access
Declare a variable to give access to each content.  When this option is given,
the code generator will emit a global variable declaration with the name of the
file.  By using the global variable, the program can access the resource
directly.


--var-prefix **prefix**
Defines the prefix to be used for the variable declarations that give
access access to each content.  This option implies the
*--var-access* option.


--no-type-declaration
Do not declare any type in the package specification.  It is assumed that the
types used by the generated code is declared somewhere else and is visible during the
compilation.


--type-name **name**
Define the name of the type used to hold the information.  This is the name
of the C, Ada or Go type that is generated.  It must be a valid name
of the target language.


--member-content **name**
Define the name data structure member holding the content.


--member-length **name**
Define the name data structure member holding the length.


--member-modtime **name**
Define the name data structure member holding the modification time.


--member-format **name**
Define the name data structure member holding the content format


--preelaborate
This option is recognized by the Ada generator and it tells
it to emit a pragma Preelaborate in the generated specification file.


--content-only
This option is specific to the Ada generator and instructs
the generator to only give access to the content.

### RULE DESCRIPTION

The rule descritions are best expressed by using an XML file.
The XML file can describe several resources and for each of them
it defines the files that must be included with their optional
transformation.  The XML file must have a
*package* root element.

A resource is described by the
*resource* XML element with a mandatory
*name* attribute that indicates the name of the resource.
It then contains an
*install* XML element which describes the installation rule with the patterns
that identify the files.

```
 <package>
  <resource name='help' format='string'>
   <header>--  Some header comment</header>
   <install mode='copy'>
     <fileset dir='help'>
      <include name='**/*.txt'/>
     </fileset>
   </install>
  </resource>
 </package>
```

A resource can be represented as an array of strings by using the
*lines* format.  In that case, a
*line-separator* XML element indicates the character on which lines are split.
The
*keep-empty-lines* attribute controls whether an empty line is kept or must be
discarded.  The default will discard empty lines.  With the
*lines* format, the final content will be represented as an array of strings.

```
 <package>
  <resource name='help' format='lines' keep-empty-lines='true'>
   <line-separator>\\r</line-separator>
   <line-separator>\\n</line-separator>
   <install mode='copy'>
     <fileset dir='help'>
      <include name='**/*.txt'/>
     </fileset>
   </install>
  </resource>
 </package>
```

The special format
*map* reads the content of files which are collected and produce a mapping table with them.
The files can be a JSON file with name/value pairs and the mapping table will provide
an efficient conversion of a name into the corresponding value.

```
 <package>
  <resource name='Extensions_Map' format='map'
            type='access constant String'>
   <mapper type='json'/>
   <install mode='copy'>
     <fileset dir='.'>
      <include name='**/*.json'/>
     </fileset>
   </install>
  </resource>
 </package>
```

### INSTALL MODES

The **are** tool provides several installation modes:


copy
Copy the file.


copy-first
Copy the first file.


exec
Execute a command with the file.


copy-exec
The file is copied and a command is then executed with the target path for some transformations.


concat
The files that match the pattern are concatenated.


bundle
This mode concern Java like property files and allows to do some specific merge in
the files.


merge
This mode concern Java like property files and allows to do some specific merge in
the files.

### SEE ALSO

**ant(1)**, **gprbuild(1)**, **gzip(1)**, **closure(1)**,
**yui-compressor(1)**

### AUTHOR

Written by Stephane Carrez.

