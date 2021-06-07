-----------------------------------------------------------------------
--  are-installer -- Resource selector, preparer and installer
--  Copyright (C) 2012, 2017, 2021 Stephane Carrez
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
with Ada.Finalization;
private with Ada.Strings.Unbounded;
private with Ada.Containers.Vectors;
private with Ada.Containers.Indefinite_Vectors;
private with Ada.Containers.Indefinite_Ordered_Maps;
private with GNAT.Regpat;
private with DOM.Core;
private with Util.Log;

--  = Rules =
--  The `Advanced Resource Embedder` provides several mechanisms to integrate
--  files in the generated code.
--
--  An XML file file contains a set of rules which describe how to select the
--  files to include in the generator.  The XML file is read and resource rules
--  introduced by the `resource` XML element are collected.
--
--  The source paths are then scanned and a complete tree of source files is created.
--  Because several source paths are given, we have several source trees with possibly
--  duplicate files and names in them.
--
--  The source paths are matched against the resource rules and each installation rule
--  is filled with the source files that they match.
--
--  The resource installation rules are executed in the order defined
--  in the `package.xml` file.  Each resource rule can have its own way to make
--  the installation for the set of files that matched the rule definition.
--  A resource rule can copy the file, another can concatenate the source files,
--  another can do some transformation on the source files and prepare it before being
--  embedded and used by the generator.
--
--  @include are-installer-copies.ads
--  @include are-installer-concat.ads
--  @include are-installer-exec.ads
--  @include are-installer-bundles.ads
--  @include are-installer-merges.ads
package Are.Installer is

   type Installer_Type is limited new Ada.Finalization.Limited_Controlled with private;

   --  Read the XML package file that describes the resources and build the list
   --  of rules to collect and build those resources.
   procedure Read_Package (Installer : in out Installer_Type;
                           File      : in String;
                           Context   : in out Context_Type'Class);

   --  Scan the directory collecting the files that must be taken into account and
   --  processed by the distribution rules.
   procedure Scan_Directory (Installer : in out Installer_Type;
                             Path      : in String;
                             Context   : in out Context_Type'Class);

   --  Add a simple rule to copy the files matching the pattern on the resource.
   procedure Add_Rule (Installer : in out Installer_Type;
                       Resource  : in Are.Resource_Access;
                       Pattern   : in String);

   --  Execute the installation rules and collect the resources to be written
   --  in the context.
   procedure Execute (Installer : in out Installer_Type;
                      Context   : in out Context_Type'Class);

   --  Clear the rules and files that have been loaded.
   procedure Clear (Installer : in out Installer_Type);

private

   type Directory_List;
   type Directory_List_Access is access all Directory_List;

   --  A `File_Record` refers to a source file that must be processed by a resource
   --  rule.  It is linked to the directory which contains it through the `Dir` member.
   --  The `Name` refers to the file name part.
   type File_Record (Length : Natural) is record
      Dir  : Directory_List_Access;
      Name : String (1 .. Length);
   end record;

   package File_Record_Vectors is
     new Ada.Containers.Indefinite_Vectors (Index_Type   => Positive,
                                            Element_Type => File_Record);

   subtype File_Vector is File_Record_Vectors.Vector;
   subtype File_Cursor is File_Record_Vectors.Cursor;

   --  Get the first source path from the list.
   function Get_Source_Path (From           : in File_Vector;
                             Use_First_File : in Boolean := False) return String;

   --  The file tree represents the target distribution tree that must be built.
   --  Each key represent a target file and it is associated with a `File_Vector` which
   --  represents the list of source files that must be used to build the target.
   package File_Tree is
     new Ada.Containers.Indefinite_Ordered_Maps (Key_Type     => String,
                                                 Element_Type => File_Vector,
                                                 "<"          => "<",
                                                 "="          => File_Record_Vectors."=");

   package Directory_List_Vector is
      new Ada.Containers.Vectors (Index_Type   => Positive,
                                  Element_Type => Directory_List_Access);

   --  The `Directory_List` describes the content of a source directory.
   type Directory_List (Length      : Positive;
                        Path_Length : Natural)
   is record
      Files       : File_Record_Vectors.Vector;
      Directories : Directory_List_Vector.Vector;
      Rel_Pos     : Positive := 1;
      Name        : String (1 .. Length);
      Path        : String (1 .. Path_Length);
   end record;

   --  Get the relative path of the directory.
   function Get_Relative_Path (Dir : in Directory_List) return String;

   --  Strip the base part of the path
   function Get_Strip_Path (Base : in String;
                            Path : in String) return String;

   --  Build a regular expression pattern from a pattern string.
   function Make_Regexp (Pattern : in String) return String;

   --  Build a regular expression pattern from a pattern string.
   function Make_Regexp (Pattern : in String) return GNAT.Regpat.Pattern_Matcher;

   --  Scan the directory whose root path is `Path` and with the relative path
   --  `Rel_Path` and build in `Dir` the list of files and directories.
   procedure Scan (Path     : in String;
                   Rel_Path : in String;
                   Dir      : in Directory_List_Access);

   type Match_Rule is record
      Base_Dir : Ada.Strings.Unbounded.Unbounded_String;
      Match    : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   package Match_Rule_Vector is
      new Ada.Containers.Vectors (Index_Type   => Positive,
                                  Element_Type => Match_Rule);

   --  ------------------------------
   --  Distribution rule
   --  ------------------------------
   --  The <b>Distrib_Rule</b> represents a distribution rule that must be executed on
   --  a given file or set of files.
   type Distrib_Rule is abstract tagged record
      Dir      : Ada.Strings.Unbounded.Unbounded_String;
      Resource : Are.Resource_Access;
      Matches  : Match_Rule_Vector.Vector;
      Excludes : Match_Rule_Vector.Vector;
      Files    : File_Tree.Map;
      Level    : Util.Log.Level_Type := Util.Log.DEBUG_LEVEL;
      Strip_Extension  : Boolean := False;
      Source_Timestamp : Boolean := False;
   end record;
   type Distrib_Rule_Access is access all Distrib_Rule'Class;

   --  Get a name to qualify the installation rule (used for logs).
   function Get_Install_Name (Rule    : in Distrib_Rule) return String is abstract;

   --  Install the file <b>File</b> according to the distribution rule.
   procedure Install (Rule    : in Distrib_Rule;
                      Target  : in String;
                      File    : in File_Vector;
                      Context : in out Context_Type'Class) is abstract;

   --  Scan the directory tree whose root is defined by <b>Dir</b> and find the files
   --  that match the current rule.
   procedure Scan (Rule : in out Distrib_Rule;
                   Dir  : in Directory_List);

   procedure Scan (Rule     : in out Distrib_Rule;
                   Dir      : in Directory_List;
                   Base_Dir : in String;
                   Pattern  : in String;
                   Exclude  : in Boolean);

   procedure Execute (Rule    : in out Distrib_Rule;
                      Context : in out Context_Type'Class);

   --  Get the target path associate with the given source file for the distribution rule.
   function Get_Target_Path (Rule : in Distrib_Rule;
                             Base : in String;
                             File : in File_Record) return String;

   --  Get the source path of the file.
   function Get_Source_Path (Rule : in Distrib_Rule;
                             File : in File_Record) return String;

   --  Get the path that must be exported by the rule.
   function Get_Export_Path (Rule : in Distrib_Rule;
                             Path : in String) return String;

   --  Add the file to be processed by the distribution rule.  The file has a relative
   --  path represented by <b>Path</b>.  The path is relative from the base directory
   --  specified in <b>Base_Dir</b>.
   procedure Add_Source_File (Rule     : in out Distrib_Rule;
                              Path     : in String;
                              File     : in File_Record);

   --  Remove the file to be processed by the distribution rule.  This is the opposite of
   --  <tt>Add_Source_File</tt> and used for the <exclude name="xxx"/> rules.
   procedure Remove_Source_File (Rule     : in out Distrib_Rule;
                                 Path     : in String;
                                 File     : in File_Record);

   --  Load and add the file in the resource library.
   procedure Add_File (Rule : in Distrib_Rule;
                       Name : in String;
                       Path : in String;
                       Modtime  : in Ada.Calendar.Time;
                       Override : in Boolean := False);

   --  Create a resource rule identified by `Kind`.
   --  The resource rule is configured according to the DOM tree whose node is `Node`.
   function Create_Rule (Kind     : in String;
                         Node     : in DOM.Core.Node) return Distrib_Rule_Access;

   --  A list of rules that define how to build the distribution.
   package Distrib_Rule_Vectors is
      new Ada.Containers.Vectors (Index_Type   => Positive,
                                  Element_Type => Distrib_Rule_Access);

   type Installer_Type is limited new Ada.Finalization.Limited_Controlled with record
      Rules : Distrib_Rule_Vectors.Vector;
      Trees : Directory_List_Vector.Vector;
   end record;

   procedure Delete (Directory : in out Directory_List_Access);

   overriding
   procedure Finalize (Installer : in out Installer_Type);

end Are.Installer;
