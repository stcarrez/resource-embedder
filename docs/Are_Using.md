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

Then the first `line-filter` will remove the `\r` and `\n` characters.

The regular expression `/\*[^/]*\*/` matches a C style comment and remove it.

The last `line-filter` replaces multiple tabs and spaces by a single occurence.

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


### NAME

are - Resource embedder to include files in Ada, C/C++, Go binaries

### SYNOPSIS

*are* [ -v ] [-vv] [-V] [--tmp
_directory_ ] [-k] [--keep]
    [-o
_directory_ ] [-l
_lang_ ] [--rule
_path_ ] [--resource
_name_ ]
    [--fileset
_pattern_ ] [--ignore-case] [--list-access] [--var-access]
    [--var-prefix
_prefix_ ] [--no-type-declaration] [--type-name
_name_ ]
    [--function-name
_name_ ] [--member-content
_name_ ]
    [--member-length
_name_ ] [--member-modtime
_name_ ]
    [--member-format
_name_ ] [--preelaborate]
    [--content-only] directory...


### DESCRIPTION

_are_ is a tool to generate C, Ada or Go source allowing to embed files
in a binary program by compiling and linking with the compiled generated sources.

The process to use _are_ is simple and composed of three steps:

* First, you describe the resources that you want to embed.
The description is either made on command line arguments or by writing an XML file.
The XML description gives more flexibility as it allows to define a transformation rule that
must be executed on the original file before being embedded.  This allows to minify a Javascript
or CSS file, compress some files and even encrypt a file before its integration.

* You run the _are_ command with the your target language and rule description and you give the tool
a list of directories that must be scanned to identify the files that must be collected.
The _are_ tool scan the directories according to the patterns that you have given either on
the command line or in the XML rule description.  After identifying the files, the tool applies
the rules and execute the transformations.
The _are_ tool then invokes the target language generator that writes one or several files depending
on the list of resources.

* Once the files are generated, you use them in your program and add them in your build process
as they are now part of your sources.  After building your program, it now embeds the
resource files that were collected and optionally transformed.

The identification of files is made by using fileset patterns similar to the
_ant_(1) tool.  The patterns are applied to the directories that are passed to the _are_ tool.
Files that match the pattern are selected and taken into account.
The pattern can be an exact relative path definition or it may contain wildcards.
Below are some examples:


*.txt
This pattern matches all files with a
_.txt_ extension in the directories passed to the command.  Only the root directories are taken
into account (the
_.txt_ files in sub-directories are ignored).


**/*.txt
The
_**/_ pattern indicates that the pattern is applied on directories recursively.
The files must then match the
_*.txt_ pattern to be taken into account.  Therefore, the
_**/*.txt_ pattern will match all
_.txt_ files in any directory.


config/*.conf
This pattern will match the
_.conf_ files in the
_config_ directory.


web/index.html
This pattern matches a fixed path.

### OPTIONS

The following options are recognized by _are_:


-V
Prints the
_are_ version.


-v
Enable the verbose mode.


-vv
Enable debugging output.


--tmp _directory_
Use the directory to build the resource files.  The default directory is
_are-generator_ and it is created in the current working directory.  This option allows to
choose another path.


--keep
Keep the directory used to prepare the resource files.  By default the
_are-generator_ directory (which can be overriden by the
_--tmp_ option) is removed when the code generation is finished.  By keeping the
directory, you can have a look at the files and their transformations.


--output _directory_
Set the output directory path where generators writes the code.


--lang _language_
Select the target generator language.  The supported languages are
_Ada_, _C_, and
_Go_. The default language is
_Ada_. 

--rule _path_
Read the XML file that describes the resources to generate.  The use of a XML resource
file allows to use the advance features of the tool such as doing some transformations
on the input files.  The XML resource file can describe several resources and
provides mechanisms to control the generation for each of them.


--resource _name_
Define the name of the resource collection.  This option is used to create a resource
with the given name.


--fileset _pattern_
Define the pattern to match files for the resource collection.
After the
_--resource_ option, this indicates the pattern to match the files for that resource.


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


--var-prefix _prefix_
Defines the prefix to be used for the variable declarations that give
access access to each content.  This option implies the
_--var-access_ option.


--no-type-declaration
Do not declare any type in the package specification.  It is assumed that the
types used by the generated code is declared somewhere else and is visible during the
compilation.


--type-name _name_
Define the name of the type used to hold the information.  This is the name
of the C, Ada or Go type that is generated.  It must be a valid name
of the target language.


--member-content _name_
Define the name data structure member holding the content.


--member-length _name_
Define the name data structure member holding the length.


--member-modtime _name_
Define the name data structure member holding the modification time.


--member-format _name_
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
_package_ root element.

A resource is described by the
_resource_ XML element with a mandatory
_name_ attribute that indicates the name of the resource.
It then contains an
_install_ XML element which describes the installation rule with the patterns
that identify the files.

```
 <package>
  <resource name='help' format='string'>
   <install mode='copy'>
     <fileset dir='help'>
      <include name='**/*.txt'/>
     </fileset>
   </install>
  </resource>
 </package>
```

### INSTALL MODES

The _are_ tool provides several installation modes:


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

_ant(1)_, _gprbuild(1)_, _gzip(1)_, _closure(1)_,
_yui-compressor(1)_

### AUTHOR

Written by Stephane Carrez.

