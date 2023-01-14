
## NAME

are - Resource embedder to include files in Ada, C/C++, Go binaries

## SYNOPSIS

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


## DESCRIPTION

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

## OPTIONS

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

## RULE DESCRIPTION

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

## INSTALL MODES

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

## SEE ALSO

**ant(1)**, **gprbuild(1)**, **gzip(1)**, **closure(1)**,
**yui-compressor(1)**

## AUTHOR

Written by Stephane Carrez.

