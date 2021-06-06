# Generator
The code generators are invoked when the installer has scanned the directories,
selected the files and applied the installation rules to produce the content
that must be embedded.

## Ada Generator
The Ada code generator produces for each resource description an Ada
package with the name of that resource.  Sometimes, the Ada package
specification is enough and it contains all necessary definitions including
the content of files.  In other cases, an Ada package body is also generated
and it contains the generated files with a function that allows to query
and retrieve the file content.  The Ada code generator is driven by the
resource description and also by the tool options.

When the `--content-only` option is used, the code generator uses the
following type to describe a file content:

```Ada
type Content_Access is
   access constant Ada.Streams.Stream_Element_Array;
```

This type definition gives access to a readonly binary content and provides
enough information to also indicate the size of that content.  Then when
the `--name-access` option is passed, the code generator declares and
implements the following function:

```Ada
function Get_Content (Name : String) return Content_Access;
```

That function will return either a content access or null if it was not found.

By default, when the `--content-only` option is not passed, the code generator
provides more information about the embedded content such as the file name,
the modification time of the file and the target file format.
In that case, the following Ada record is declared in the Ada specification:

```Ada
type Name_Access is access constant String;
type Format_Type is (FILE_RAW, FILE_GZIP);
type Content_Type is record
  Name    : Name_Access;
  Content : Content_Access;
  Modtime : Interfaces.C.long = 0;
  Format  : Format_Type := FILE_RAW;
end record;
```

The generated `Get_Content` function will return a `Content_Type`.  You must
compare the result with the `Null_Content` constant to check if the embedded
file was found.

When the `--list-access` option is passed, the code generator emits a code
that gives access to the list of file names embedded in the resource.
The list of names is a simple Ada constant array.  The array is sorted
on the name.  It is declared as follows:

```Ada
type Name_Array is array (Natural range <>) of Name_Access;
Names : constant Name_Array;
```
## C Generator
The C code generator produces for each resource description a C
header and a C source file with the name of that resource.  The header
contains the public declaration and the C source file contains the generated
files with an optional function that allows to query
and retrieve the file content.  The C code generator is driven by the
resource description and also by the tool options.

The header file declares a C structure that describes the content information.
The C structure gives access to the content, its size,
the modification time of the file and the target file format.
The structure name is prefixed by the resource name.

```Ada
struct <resource>_content {
  const unsigned char* content;
  size_t size;
  time_t modtime;
  int format;
}
```

This type definition gives access to a readonly binary content and provides
enough information to also indicate the size of that content.  Then when
the `--name-access` option is passed, the code generator declares and
implements the following function:

```Ada
extern const struct <resource>_content *
     <resource>_get_content(const char* name);
```

That function will return either a pointer to the resource description
or null if the name was not found.

When the `--list-access` option is passed, the C code generator also
declares two global constant variables:

```Ada
extern const char* const <resource>_names[];
static const int <resource>_names_length = NNN;
```

The generated array gives access to the list of file names embedded in
the resource.  That list is sorted on the name so that a dichotomic
search can be used to find an entry.

## Go Generator
The Go code generator produces for each resource description a Go
source file with the name of that resource.  The header
contains the public declaration and the C source file contains the generated
files with an optional function that allows to query
and retrieve the file content.  The C code generator is driven by the
resource description and also by the tool options.

The Go source file declares a structure that describes the content information.
The structure is declared public so that it is visible outside the Go package.
It gives access to the content, its size,
the modification time of the file and the target file format.

```Ada
type Content struct {
  Content []byte
  Size    int64
  Modtime int64
  Format  int
}
```

This type definition gives access to a binary content and provides
enough information to also indicate the size of that content.  Then when
the `--name-access` option is passed, the code generator declares and
implements the following function:

```Ada
func Get_content(name string) (*Content)
```

That function will return either a pointer to the resource description
or null if the name was not found.

When the `--list-access` option is passed, the Go code generator
makes available the list of names by making the `Names` variable public:

```Ada
var Names= []string {
...
}
```

The generated array gives access to the list of file names embedded in
the resource.  That list is sorted on the name so that a dichotomic
search can be used to find an entry.


