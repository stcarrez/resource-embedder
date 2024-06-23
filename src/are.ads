-----------------------------------------------------------------------
--  are -- Advanced Resource Embedder
--  Copyright (C) 2021, 2023, 2024 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
private with Ada.Strings.Unbounded;
private with Ada.Finalization;
private with Ada.Streams;
private with Ada.Strings.Maps;
private with Ada.Containers.Indefinite_Ordered_Maps;
private with Ada.Containers.Indefinite_Vectors;
private with Ada.Directories;
private with Ada.Calendar;
private with Ada.Command_Line;
private with GNAT.Strings;
private with GNAT.Regpat;
private with Util.Strings.Vectors;
private with Util.Strings.Maps;
package Are is

   function "-" (Message : in String) return String is (Message);

   type Context_Type is tagged limited private;

   type Resource_Type is tagged limited private;

   type Resource_Access is access all Resource_Type;

   type Resource_List is limited private;

private

   subtype UString is Ada.Strings.Unbounded.Unbounded_String;

   subtype Character_Set is Ada.Strings.Maps.Character_Set;

   type Stream_Element_Access is access all Ada.Streams.Stream_Element_Array;

   type File_Format is (FORMAT_RAW, FORMAT_GZIP);

   --  The information about a file being embedded.
   type File_Info is record
      Content    : Stream_Element_Access;
      Length     : Ada.Directories.File_Size;
      Modtime    : Ada.Calendar.Time;
      Format     : File_Format := FORMAT_RAW;
      Line_Count : Natural := 0;
   end record;

   --  An ordered map of files being embedded.
   package File_Maps is
     new Ada.Containers.Indefinite_Ordered_Maps (Key_Type     => String,
                                                 Element_Type => File_Info,
                                                 "<"          => "<",
                                                 "="          => "=");

   type Format_Type is (R_BINARY, R_STRING, R_LINES, R_MAP);
   type Mapper_Type is (M_NONE, M_TEXT, M_JSON);

   type Line_Filter_Type (Size : GNAT.Regpat.Program_Size;
                          Replace_Length : Natural) is
      record
         Pattern : GNAT.Regpat.Pattern_Matcher (Size);
         Replace : String (1 .. Replace_Length);
      end record;

   package Filter_Vectors is
     new Ada.Containers.Indefinite_Vectors (Index_Type   => Positive,
                                            Element_Type => Line_Filter_Type);

   --  A resource is composed of a set of files.
   type Resource_Type is limited new Ada.Finalization.Limited_Controlled with record
      Next                : Resource_Access;
      Name                : UString;
      Format              : Format_Type := R_BINARY;
      Mapper              : Mapper_Type := M_NONE;
      Keep_Empty_Lines    : Boolean := False;
      Var_Access          : Boolean := False;
      Name_Access         : Boolean := False;
      List_Access         : Boolean := False;
      Content_Only        : Boolean := False;
      No_Type_Declaration : Boolean := False;
      Files               : File_Maps.Map;
      Separators          : Character_Set := Ada.Strings.Maps.Null_Set;
      Filters             : Filter_Vectors.Vector;
      Type_Name           : UString;
      Index_Type_Name     : UString;
      Content_Type_Name   : UString;
      Function_Name       : UString;
      Member_Content_Name : UString;
      Member_Length_Name  : UString;
      Member_Modtime_Name : UString;
      Member_Format_Name  : UString;
      Var_Prefix          : UString;
      Headers_Spec        : Util.Strings.Vectors.Vector;
      Headers_Impl        : Util.Strings.Vectors.Vector;
   end record;

   --  Get the name of type and struct/record members for the generator:
   --  - get the value from the resource library definition,
   --  - otherwise use the global context,
   --  - otherwise use the pre-defined value.
   function Get_Type_Name (Resource : in Resource_Type;
                           Context  : in Context_Type'Class;
                           Default  : in String) return String;
   function Get_Index_Type_Name (Resource : in Resource_Type;
                                 Context  : in Context_Type'Class;
                                 Default  : in String) return String;
   function Get_Content_Type_Name (Resource : in Resource_Type;
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
   procedure Add_File (Resource : in out Resource_Type;
                       Name     : in String;
                       Path     : in String;
                       Override : in Boolean := False) with
     Pre  => Name'Length > 0 and then Path'Length > 0,
     Post => Resource.Files.Contains (Name);

   --  Load and add the file in the resource library.
   procedure Add_File (Resource : in out Resource_Type;
                       Name     : in String;
                       Path     : in String;
                       Modtime  : in Ada.Calendar.Time;
                       Override : in Boolean := False) with
     Pre  => Name'Length > 0 and then Path'Length > 0,
     Post => Resource.Files.Contains (Name);

   --  Add a line filter that will replace contents matching the pattern
   --  by the replacement string.
   procedure Add_Line_Filter (Resource    : in out Resource_Type;
                              Pattern     : in String;
                              Replacement : in String);

   --  Convert the file content to a list of string lines.
   procedure Convert_To_Lines (Resource : in Resource_Type;
                               File     : in File_Info;
                               Lines    : in out Util.Strings.Vectors.Vector);

   --  Convert the file content to a mapping table.
   procedure Convert_To_Map (Resource : in Resource_Type;
                             Path     : in String;
                             File     : in File_Info;
                             Context  : in out Context_Type'Class;
                             Map      : in out Util.Strings.Maps.Map);

   --  Collect the list of files names for the resource (list is sorted).
   procedure Collect_Names (Resource    : in Resource_Type;
                            Ignore_Case : in Boolean;
                            Names       : in out Util.Strings.Vectors.Vector);

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
      Version     : aliased Boolean := False;
      Ignore_Case : aliased Boolean := False;
      Name_Access : aliased Boolean := False;
      Var_Access  : aliased Boolean := False;
      No_Type_Declaration : aliased Boolean := False;
      List_Access         : aliased Boolean := False;
      Keep_Temporary      : aliased Boolean := False;
      Rule_File           : aliased GNAT.Strings.String_Access;
      Language            : aliased GNAT.Strings.String_Access;
      Output              : aliased GNAT.Strings.String_Access;
      Tmp_Dir             : aliased GNAT.Strings.String_Access;
      Type_Name           : aliased GNAT.Strings.String_Access;
      Index_Type_Name     : aliased GNAT.Strings.String_Access;
      Function_Name       : aliased GNAT.Strings.String_Access;
      Member_Content_Name : aliased GNAT.Strings.String_Access;
      Member_Length_Name  : aliased GNAT.Strings.String_Access;
      Member_Modtime_Name : aliased GNAT.Strings.String_Access;
      Member_Format_Name  : aliased GNAT.Strings.String_Access;
      Resource_Name       : aliased GNAT.Strings.String_Access;
      Fileset_Pattern     : aliased GNAT.Strings.String_Access;
      Var_Prefix          : aliased GNAT.Strings.String_Access;
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
                             Verbose : in Boolean);

end Are;
