-----------------------------------------------------------------------
--  are -- Advanced Resource Embedder
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
private with Ada.Strings.Unbounded;
private with Ada.Finalization;
private with Ada.Streams;
private with Ada.Containers.Indefinite_Ordered_Maps;
private with Ada.Directories;
private with Ada.Calendar;
private with Ada.Command_Line;
private with GNAT.Strings;
package Are is

   function "-" (Message : in String) return String is (Message);

   type Context_Type is tagged limited private;

   type Resource_Type is tagged limited private;

   type Resource_Access is access all Resource_Type;

   type Resource_List is limited private;

private

   subtype UString is Ada.Strings.Unbounded.Unbounded_String;

   type Stream_Element_Access is access all Ada.Streams.Stream_Element_Array;

   type File_Format is (FORMAT_RAW, FORMAT_GZIP);

   --  The information about a file being embedded.
   type File_Info is record
      Content : Stream_Element_Access;
      Length  : Ada.Directories.File_Size;
      Modtime : Ada.Calendar.Time;
      Format  : File_Format := FORMAT_RAW;
   end record;

   --  An ordered map of files being embedded.
   package File_Maps is
     new Ada.Containers.Indefinite_Ordered_Maps (Key_Type     => String,
                                                 Element_Type => File_Info,
                                                 "<"          => "<",
                                                 "="          => "=");

   --  A resource is composed of a set of files.
   type Resource_Type is limited new Ada.Finalization.Limited_Controlled with record
      Next                : Resource_Access;
      Name                : UString;
      Files               : File_Maps.Map;
      Type_Name           : UString;
      Function_Name       : UString;
      Member_Content_Name : UString;
      Member_Length_Name  : UString;
      Member_Modtime_Name : UString;
      Member_Format_Name  : UString;
   end record;

   --  Get the name of type and struct/record members for the generator:
   --  - get the value from the resource library definition,
   --  - otherwise use the global context,
   --  - otherwise use the pre-defined value.
   function Get_Type_Name (Resource : in Resource_Type;
                           Context  : in Context_Type'Class;
                           Default  : in String) return String;
   function Get_Function_Name (Resource : in Resource_Type;
                               Context  : in Context_Type'Class;
                               Default  : in String) return String;
   function Get_Member_Content_Name (Resource : in Resource_Type;
                                     Context  : in Context_Type'Class;
                                     Default  : in String) return String;
   function Get_Member_Length_Name (Resource : in Resource_Type;
                                    Context  : in Context_Type'Class;
                                    Default  : in String) return String;
   function Get_Member_Modtime_Name (Resource : in Resource_Type;
                                     Context  : in Context_Type'Class;
                                     Default  : in String) return String;
   function Get_Member_Format_Name (Resource : in Resource_Type;
                                    Context  : in Context_Type'Class;
                                    Default  : in String) return String;

   --  Release the resources.
   overriding
   procedure Finalize (Context : in out Resource_Type);

   --  Load and add the file in the resource library.
   procedure Add_File (Resource : in Resource_Access;
                       Name     : in String;
                       Path     : in String;
                       Override : in Boolean := False) with
     Pre  => Resource /= null and Name'Length > 0 and Path'Length > 0,
     Post => Resource.Files.Contains (Name);

   --  Load and add the file in the resource library.
   procedure Add_File (Resource : in Resource_Access;
                       Name     : in String;
                       Path     : in String;
                       Modtime  : in Ada.Calendar.Time;
                       Override : in Boolean := False) with
     Pre  => Resource /= null and Name'Length > 0 and Path'Length > 0,
     Post => Resource.Files.Contains (Name);

   --  List of resources.
   type Resource_List is limited record
      Head   : Resource_Access;
   end record;

   --  Create a new resource with the given name.
   procedure Create_Resource (List     : in out Resource_List;
                              Name     : in String;
                              Resource : out Resource_Access) with
     Pre  => Name'Length > 0,
     Post => Resource /= null;

   --  Get the number of resources in the list.
   function Length (List : in Resource_List) return Natural;

   --  The context holding and describing information to embed.
   type Context_Type is limited new Ada.Finalization.Limited_Controlled with record
      Status      : Ada.Command_Line.Exit_Status := Ada.Command_Line.Success;
      Resources   : Resource_List;
      Verbose     : aliased Boolean := False;
      Debug       : aliased Boolean := False;
      Dump        : aliased Boolean := False;
      Ignore_Case : aliased Boolean := False;
      Name_Index  : aliased Boolean := False;
      Declare_Var : aliased Boolean := False;
      No_Type_Declaration : aliased Boolean := False;
      List_Content        : aliased Boolean := False;
      Keep_Temporary      : aliased Boolean := False;
      Rule_File           : aliased GNAT.Strings.String_Access;
      Language            : aliased GNAT.Strings.String_Access;
      Output              : aliased GNAT.Strings.String_Access;
      Tmp_Dir             : aliased GNAT.Strings.String_Access;
      Type_Name           : aliased GNAT.Strings.String_Access;
      Function_Name       : aliased GNAT.Strings.String_Access;
      Member_Content_Name : aliased GNAT.Strings.String_Access;
      Member_Length_Name  : aliased GNAT.Strings.String_Access;
      Member_Modtime_Name : aliased GNAT.Strings.String_Access;
      Member_Format_Name  : aliased GNAT.Strings.String_Access;
      Resource_Name       : aliased GNAT.Strings.String_Access;
      Fileset_Pattern     : aliased GNAT.Strings.String_Access;
   end record;

   --  Release the context information.
   overriding
   procedure Finalize (Context : in out Context_Type);

   --  Report an error and set the exit status accordingly
   procedure Error (Context : in out Context_Type;
                    Message : in String;
                    Arg1    : in String;
                    Arg2    : in String := "");
   procedure Error (Context : in out Context_Type;
                    Message : in String;
                    Arg1    : in UString;
                    Arg2    : in String := "");

   --  Get a path for the resource generation directory.
   function Get_Generation_Path (Context : in Context_Type;
                                 Name    : in String) return String;

   --  Get the path to write a file taking into account the output directory.
   function Get_Output_Path (Context : in Context_Type;
                             Name    : in String) return String;

   --  Configure the logs.
   procedure Configure_Logs (Debug   : in Boolean;
                             Dump    : in Boolean;
                             Verbose : in Boolean);

end Are;
