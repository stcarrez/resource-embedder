-----------------------------------------------------------------------
--  are-generator -- Advanced Resource Embedder Generator
--  Copyright (C) 2021, 2023 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with GNAT.Command_Line;
private with Ada.Text_IO;

--  = Generator =
--  The code generators are invoked when the installer has scanned the directories,
--  selected the files and applied the installation rules to produce the content
--  that must be embedded.
--
--  @include are-generator-ada2012.ads
--  @include are-generator-c.ads
--  @include are-generator-go.ads
package Are.Generator is

   package GC renames GNAT.Command_Line;

   --  Main entry point to parse command line arguments.
   procedure Main;

   procedure Usage;

private

   type Generator_Type is limited interface;

   --  Generate the code for the resources that have been collected.
   procedure Generate (Generator : in out Generator_Type;
                       Resources : in Resource_List;
                       Context   : in out Are.Context_Type'Class) is abstract;

   --  Setup the command line configuration to accept specific generation options.
   procedure Setup (Generator : in out Generator_Type;
                    Config    : in out GC.Command_Line_Configuration) is abstract;

   --  Return a string that identifies the program.
   function Get_Title return String;

   procedure Add_Option (Switch, Value : in String);

   --  Portability hack to support old GNAT.Command_Line before gcc 7.3
   procedure Specific_Options (Config  : in out GC.Command_Line_Configuration;
                               Context : in out Are.Context_Type'Class);

   --  Write a list of lines in the output file.
   procedure Put_Lines (File : in out Ada.Text_IO.File_Type;
                        Lines : in Util.Strings.Vectors.Vector);

end Are.Generator;
