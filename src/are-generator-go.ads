-----------------------------------------------------------------------
--  are-generator-go -- Generator for Go
--  Copyright (C) 2021 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--
--  Licensed under the Apache License, Version 2.0 (the "License");
--  you may not use this file except in compliance with the License.
--  You may obtain a copy of the License at
--
--      http://www.apache.org/licenses/LICENSE-2.0
--
--  Unless required by applicable law or agreed to in writing, software
--  distributed under the License is distributed on an "AS IS" BASIS,
--  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
--  See the License for the specific language governing permissions and
--  limitations under the License.
-----------------------------------------------------------------------
private with Util.Strings.Vectors;

--  == Go Generator ==
--  The Go code generator produces for each resource description a Go
--  source file with the name of that resource.  The header
--  contains the public declaration and the C source file contains the generated
--  files with an optional function that allows to query
--  and retrieve the file content.  The C code generator is driven by the
--  resource description and also by the tool options.
--
--  The Go source file declares a structure that describes the content information.
--  The structure is declared public so that it is visible outside the Go package.
--  It gives access to the content, its size,
--  the modification time of the file and the target file format.
--
--    type Content struct {
--      Content []byte
--      Size    int64
--      Modtime int64
--      Format  int
--    }
--
--  This type definition gives access to a binary content and provides
--  enough information to also indicate the size of that content.  Then when
--  the `--name-access` option is passed, the code generator declares and
--  implements the following function:
--
--    func Get_content(name string) (*Content)
--
--  That function will return either a pointer to the resource description
--  or null if the name was not found.
--
--  When the `--list-access` option is passed, the Go code generator
--  makes available the list of names by making the `Names` variable public:
--
--    var Names= []string {
--    ...
--    }
--
--  The generated array gives access to the list of file names embedded in
--  the resource.  That list is sorted on the name so that a dichotomic
--  search can be used to find an entry.
--
private package Are.Generator.Go is

   type Generator_Type is new Are.Generator.Generator_Type with private;

   --  Generate the Go code for the resources that have been collected.
   overriding
   procedure Generate (Generator : in out Generator_Type;
                       Resources : in Resource_List;
                       Context   : in out Are.Context_Type'Class);

   --  Setup the command line configuration to accept specific generation options.
   overriding
   procedure Setup (Generator : in out Generator_Type;
                    Config    : in out GC.Command_Line_Configuration);

private

   type Generator_Type is new Are.Generator.Generator_Type with record
      Names               : Util.Strings.Vectors.Vector;
   end record;

   --  Generate the source file.
   procedure Generate_Source (Generator : in out Generator_Type;
                              Resource  : in Are.Resource_Type;
                              Context   : in out Are.Context_Type'Class);

end Are.Generator.Go;
