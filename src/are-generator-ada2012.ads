-----------------------------------------------------------------------
--  are-generator-ada2012 -- Generator for Ada
--  Copyright (C) 2021, 2023, 2024 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
private with Ada.Text_IO;
private with Util.Strings.Vectors;
private with Util.Strings.Maps;

--  == Ada Generator ==
--  The Ada code generator produces for each resource description an Ada
--  package with the name of that resource.  Sometimes, the Ada package
--  specification is enough and it contains all necessary definitions including
--  the content of files.  In other cases, an Ada package body is also generated
--  and it contains the generated files with a function that allows to query
--  and retrieve the file content.  The Ada code generator is driven by the
--  resource description and also by the tool options.
--
--  The code generator supports several data format to access the file content.
--
--  | Format | Ada type                                         |
--  |--------|--------------------------------------------------|
--  | binary | access constant Ada.Streams.Stream_Element_Array |
--  | string | access constant String                           |
--  | lines  | array of access constant String                  |
--  | map    | access constant String                           |
--
--  When the `--content-only` option is used, the code generator uses the
--  following type to describe a file content in the `binary` format:
--
--    type Content_Access is
--       access constant Ada.Streams.Stream_Element_Array;
--
--  for the `string` format it defines:
--
--    type Content_Access is access constant String;
--
--  and for the `lines` format it defines:
--
--    type Content_Array is
--       array (Natural range <>) of access constant String;
--    type Content_Access is access constant Content_Array;
--
--  These type definitions give access to a readonly binary or string content
--  and provides enough information to also indicate the size of that content.
--  Then when the `--name-access` option is passed, the code generator declares
--  and implements the following function:
--
--    function Get_Content (Name : String) return Content_Access;
--
--  That function will return either a content access or null if it was not found.
--
--  By default, when the `--content-only` option is not passed, the code generator
--  provides more information about the embedded content such as the file name,
--  the modification time of the file and the target file format.
--  In that case, the following Ada record is declared in the Ada specification:
--
--    type Name_Access is access constant String;
--    type Format_Type is (FILE_RAW, FILE_GZIP);
--    type Content_Type is record
--      Name    : Name_Access;
--      Content : Content_Access;
--      Modtime : Interfaces.C.long = 0;
--      Format  : Format_Type := FILE_RAW;
--    end record;
--
--  The generated `Get_Content` function will return a `Content_Type`.  You must
--  compare the result with the `Null_Content` constant to check if the embedded
--  file was found.
--
--  When the `--list-access` option is passed, the code generator emits a code
--  that gives access to the list of file names embedded in the resource.
--  The list of names is a simple Ada constant array.  The array is sorted
--  on the name.  It is declared as follows:
--
--    type Name_Array is array (Natural range <>) of Name_Access;
--    Names : constant Name_Array;
--
--  The `map` format is special as it produces a mapping table defined by
--  reading a JSON or XML content.  Files matched by the rules are read and
--  parsed to populate the internal map table.  Content of these files is not
--  available directly but the mapping is queried by using the generated
--  `Get_Content` function with the name.
private package Are.Generator.Ada2012 is

   type Generator_Type is new Are.Generator.Generator_Type with private;

   --  Generate the Ada code for the resources that have been collected.
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
      Pragma_Preelaborate : aliased Boolean := False;
      Content_Only        : aliased Boolean := False;
      Names               : Util.Strings.Vectors.Vector;
      Map                 : Util.Strings.Maps.Map;
   end record;

   --  Generate the package specification.
   procedure Generate_Specs (Generator : in out Generator_Type;
                             Resource  : in Are.Resource_Type;
                             Context   : in out Are.Context_Type'Class);

   --  Generate the package body.
   procedure Generate_Body (Generator : in out Generator_Type;
                            Resource  : in Are.Resource_Type;
                            Context   : in out Are.Context_Type'Class);

   --  Generate the perfect hash implementation used by the name indexer.
   procedure Generate_Perfect_Hash (Generator : in out Generator_Type;
                                    Resource  : in Are.Resource_Type;
                                    Context   : in out Are.Context_Type'Class);

   --  Generate the keyword table.
   procedure Generate_Keyword_Table (Generator : in out Generator_Type;
                                     Into      : in out Ada.Text_IO.File_Type);

   --  Generate the mapping table for the resource (when format = R_MAP).
   procedure Generate_Resource_Mapping (Generator    : in out Generator_Type;
                                        Resource     : in Are.Resource_Type;
                                        Context      : in out Are.Context_Type'Class);

   procedure Generate_Mapping_Table (Generator : in out Generator_Type;
                                     Into      : in out Ada.Text_IO.File_Type);

   function To_File_Name (Name : in String) return String;

   function To_Ada_Name (Prefix : in String;
                         Name   : in String) return String;

end Are.Generator.Ada2012;
