
## NAME

are - Resource embedder to include files in Ada, C/C++, Go binaries

## SYNOPSIS

*are* [ -v ] [-vv] [-vv] [-V] [--tmp
_directory_ ] [-k] [--keep]

    [-o
_directory_ ] [-l
_lang_ ] [--rule
_path_ ] [--resource
_name_ ]

    [--fileset
_pattern_ ] [--ignore-case] [--list-access] [--var-access]

    [--no-type-declaration] [--type-name
_name_ ] [--function-name
_name_ ]

    [--member-content
_name_ ] [--member-length
_name_ ]

    [--member-modtime
_name_ ] [--member-format
_name_ ] [--preelaborate]

    [--content-only] directory...


## DESCRIPTION

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

## OPTIONS

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

--list-access
Generate support to list the content names.

--var-access
Declare a variable to give access to each content.  When this option is given,
the code generator will emit a global variable declaration with the name of the
file.  By using the global variable, the program can access the resource
directly.

--no-type-declaration
Do not declare any type in the package specification.  It is assumed that the
types used by the generated code is declared elsewere and is visible during the
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

## RULE DESCRIPTION

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
 <resource name='help'>
  <install mode='copy'>
    <fileset dir='help'>
      <include name='**/*.txt'/>
    </fileset>
  </install>
 </resource>
```

## INSTALL MODES

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

## SEE ALSO

_ant(1)_, _gprbuild(1)_, _gzip(1)_, _closure(1)_,
_yui-compressor(1)_

## AUTHOR

Written by Stephane Carrez.

