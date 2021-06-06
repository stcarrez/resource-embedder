-----------------------------------------------------------------------
--  are-generator-c -- Generator for C/C++
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

--  == C Generator ==
--  The C code generator produces for each resource description a C
--  header and a C source file with the name of that resource.  The header
--  contains the public declaration and the C source file contains the generated
--  files with an optional function that allows to query
--  and retrieve the file content.  The C code generator is driven by the
--  resource description and also by the tool options.
--
--  The header file declares a C structure that describes the content information.
--  The C structure gives access to the content, its size,
--  the modification time of the file and the target file format.
--  The structure name is prefixed by the resource name.
--
--    struct <resource>_content {
--      const unsigned char* content;
--      size_t size;
--      time_t modtime;
--      int format;
--    }
--
--  This type definition gives access to a readonly binary content and provides
--  enough information to also indicate the size of that content.  Then when
--  the `--name-access` option is passed, the code generator declares and
--  implements the following function:
--
--    extern const struct <resource>_content *
--         <resource>_get_content(const char* name);
--
--  That function will return either a pointer to the resource description
--  or null if the name was not found.
--
--  When the `--list-access` option is passed, the C code generator also
--  declares two global constant variables:
--
--    extern const char* const <resource>_names[];
--    static const int <resource>_names_length = NNN;
--
--  The generated array gives access to the list of file names embedded in
--  the resource.  That list is sorted on the name so that a dichotomic
--  search can be used to find an entry.
--
private package Are.Generator.C is

   type Generator_Type is new Are.Generator.Generator_Type with private;

   --  Generate the C/C++ code for the resources that have been collected.
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

   --  Generate the header declaration file.
   procedure Generate_Header (Generator : in out Generator_Type;
                              Resource  : in Are.Resource_Type;
                              Context   : in out Are.Context_Type'Class);

   --  Generate the source file.
   procedure Generate_Source (Generator : in out Generator_Type;
                              Resource  : in Are.Resource_Type;
                              Context   : in out Are.Context_Type'Class);

end Are.Generator.C;
