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
with Ada.Unchecked_Deallocation;
with Ada.Streams.Stream_IO;
with Ada.Characters.Handling;
with Util.Log.Loggers;
with Util.Properties;
with Util.Files;
package body Are is

   use Ada.Strings.Unbounded;

   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("Are");

   function Get_Type_Name (Resource : in Resource_Type;
                           Context  : in Context_Type'Class;
                           Default  : in String) return String is
   begin
      if Length (Resource.Type_Name) > 0 then
         return To_String (Resource.Type_Name);
      elsif Context.Type_Name'Length > 0 then
         return Context.Type_Name.all;
      else
         return Default;
      end if;
   end Get_Type_Name;

   function Get_Content_Type_Name (Resource : in Resource_Type;
                                   Context  : in Context_Type'Class;
                                   Default  : in String) return String is
      pragma Unreferenced (Context);
   begin
      if Length (Resource.Content_Type_Name) > 0 then
         return To_String (Resource.Content_Type_Name);
      else
         return Default;
      end if;
   end Get_Content_Type_Name;

   function Get_Function_Name (Resource : in Resource_Type;
                               Context  : in Context_Type'Class;
                               Default  : in String) return String is
   begin
      if Length (Resource.Function_Name) > 0 then
         return To_String (Resource.Function_Name);
      elsif Context.Function_Name'Length > 0 then
         return Context.Function_Name.all;
      else
         return Default;
      end if;
   end Get_Function_Name;

   function Get_Member_Content_Name (Resource : in Resource_Type;
                                     Context  : in Context_Type'Class;
                                     Default  : in String) return String is
   begin
      if Length (Resource.Member_Content_Name) > 0 then
         return To_String (Resource.Member_Content_Name);
      elsif Context.Member_Content_Name'Length > 0 then
         return Context.Member_Content_Name.all;
      else
         return Default;
      end if;
   end Get_Member_Content_Name;

   function Get_Member_Length_Name (Resource : in Resource_Type;
                                    Context  : in Context_Type'Class;
                                    Default  : in String) return String is
   begin
      if Length (Resource.Member_Length_Name) > 0 then
         return To_String (Resource.Member_Length_Name);
      elsif Context.Member_Length_Name'Length > 0 then
         return Context.Member_Length_Name.all;
      else
         return Default;
      end if;
   end Get_Member_Length_Name;

   function Get_Member_Modtime_Name (Resource : in Resource_Type;
                                     Context  : in Context_Type'Class;
                                     Default  : in String) return String is
   begin
      if Length (Resource.Member_Modtime_Name) > 0 then
         return To_String (Resource.Member_Modtime_Name);
      elsif Context.Member_Modtime_Name'Length > 0 then
         return Context.Member_Modtime_Name.all;
      else
         return Default;
      end if;
   end Get_Member_Modtime_Name;

   function Get_Member_Format_Name (Resource : in Resource_Type;
                                    Context  : in Context_Type'Class;
                                    Default  : in String) return String is
   begin
      if Length (Resource.Member_Format_Name) > 0 then
         return To_String (Resource.Member_Format_Name);
      elsif Context.Member_Format_Name'Length > 0 then
         return Context.Member_Format_Name.all;
      else
         return Default;
      end if;
   end Get_Member_Format_Name;

   --  ------------------------------
   --  Release the resources.
   --  ------------------------------
   overriding
   procedure Finalize (Context : in out Resource_Type) is
      procedure Free is
        new Ada.Unchecked_Deallocation (Object => Ada.Streams.Stream_Element_Array,
                                        Name   => Stream_Element_Access);
      procedure Process (Key  : in String;
                         Item : in out File_Info);

      procedure Process (Key  : in String;
                         Item : in out File_Info) is
         pragma Unreferenced (Key);
      begin
         Free (Item.Content);
      end Process;
      Iter : File_Maps.Cursor := Context.Files.First;
   begin
      while File_Maps.Has_Element (Iter) loop
         Context.Files.Update_Element (Iter, Process'Access);
         File_Maps.Next (Iter);
      end loop;
   end Finalize;

   --  ------------------------------
   --  Load and add the file in the resource library.
   --  ------------------------------
   procedure Add_File (Resource : in out Resource_Type;
                       Name     : in String;
                       Path     : in String;
                       Override : in Boolean := False) is
   begin
      Resource.Add_File (Name, Path, Ada.Directories.Modification_Time (Path), Override);
   end Add_File;

   --  ------------------------------
   --  Load and add the file in the resource library.
   --  ------------------------------
   procedure Add_File (Resource : in out Resource_Type;
                       Name     : in String;
                       Path     : in String;
                       Modtime  : in Ada.Calendar.Time;
                       Override : in Boolean := False) is
      use Ada.Streams;

      File    : Ada.Streams.Stream_IO.File_Type;
      Length  : constant Ada.Directories.File_Size := Ada.Directories.Size (Path);
      Info    : File_Info;
      Last    : Ada.Streams.Stream_Element_Offset;
   begin
      Info.Length := Length;
      Info.Modtime := Modtime;
      Info.Content := new Ada.Streams.Stream_Element_Array (1 .. Stream_Element_Offset (Length));
      Ada.Streams.Stream_IO.Open (File, Ada.Streams.Stream_IO.In_File, Path);
      Ada.Streams.Stream_IO.Read (File, Info.Content.all, Last);
      Ada.Streams.Stream_IO.Close (File);

      if not Override then
         Resource.Files.Insert (Name, Info);
      else
         Resource.Files.Include (Name, Info);
      end if;
   end Add_File;

   --  ------------------------------
   --  Add a line filter that will replace contents matching the pattern
   --  by the replacement string.
   --  ------------------------------
   procedure Add_Line_Filter (Resource    : in out Resource_Type;
                              Pattern     : in String;
                              Replacement : in String) is
      Matcher : constant GNAT.Regpat.Pattern_Matcher
        := GNAT.Regpat.Compile (Pattern, GNAT.Regpat.Single_Line);
   begin
      Resource.Filters.Append ((Size => Matcher.Size,
                                Replace_Length => Replacement'Length,
                                Pattern => Matcher,
                                Replace => Replacement));
   end Add_Line_Filter;

   procedure Convert_To_Lines (Resource : in Resource_Type;
                               File     : in File_Info;
                               Lines    : in out Util.Strings.Vectors.Vector) is
      use Ada.Strings.Maps;
      function Find_End (Content : in String;
                         Start   : in Natural) return Natural;
      function Skip_End (Content : in String;
                         Start   : in Natural) return Natural;
      procedure Apply_Filter (Filter  : in Line_Filter_Type;
                              Content : in String;
                              Result  : in out UString);
      procedure Apply_Filters (Content : in String);

      function Find_End (Content : in String;
                         Start   : in Natural) return Natural is
         Pos : Natural := Start;
      begin
         while Pos <= Content'Last loop
            if Is_In (Content (Pos), Resource.Separators) then
               return Pos - 1;
            end if;
            Pos := Pos + 1;
         end loop;
         return Content'Last;
      end Find_End;

      function Skip_End (Content : in String;
                         Start   : in Natural) return Natural is
         Pos : Natural := Start;
      begin
         while Pos <= Content'Last loop
            if not Is_In (Content (Pos), Resource.Separators) then
               return Pos;
            end if;
            Pos := Pos + 1;
         end loop;
         return Pos;
      end Skip_End;

      procedure Apply_Filter (Filter  : in Line_Filter_Type;
                              Content : in String;
                              Result  : in out UString) is
         use type GNAT.Regpat.Match_Location;

         Last    : constant Natural := Content'Last;
         Matches : GNAT.Regpat.Match_Array (0 .. 10);
         Pos     : Positive := Content'First;
      begin
         Result := To_Unbounded_String ("");
         while Pos <= Last loop
            GNAT.Regpat.Match (Filter.Pattern, Content, Matches, Pos, Last);
            exit when Matches (0) = GNAT.Regpat.No_Match;
            if Matches (0).First > Pos then
               Append (Result, Content (Pos .. Matches (0).First - 1));
            end if;
            if Filter.Replace_Length > 0 then
               Append (Result, Filter.Replace);
            end if;
            Pos := Matches (0).Last + 1;
         end loop;
         if Pos <= Last then
            Append (Result, Content (Pos .. Last));
         end if;
      end Apply_Filter;

      procedure Apply_Filters (Content : in String) is
         Result : UString := To_Unbounded_String (Content);
      begin
         for Filter of Resource.Filters loop
            declare
               Content : constant String := To_String (Result);
            begin
               Apply_Filter (Filter, Content, Result);
            end;
         end loop;
         if Length (Result) > 0 then
            Lines.Append (To_String (Result));
         end if;
      end Apply_Filters;

   begin
      Lines.Clear;
      if File.Content = null or else File.Content'Length = 0 then
         return;
      end if;
      declare
         First   : constant Natural := Natural (File.Content'First);
         Last    : constant Natural := Natural (File.Content'Last);
         Content : String (First .. Last);
         for Content'Address use File.Content.all'Address;

         Pos     : Natural;
         Start   : Natural;
         End_Pos : Natural;
      begin
         Pos := First;
         Start := Pos;
         while Pos <= Last loop
            End_Pos := Find_End (Content, Pos);
            if Start < End_Pos then
               Apply_Filters (Content (Start .. End_Pos));
            end if;
            Pos := Skip_End (Content, End_Pos + 1);
            Start := Pos;
         end loop;
      end;
   end Convert_To_Lines;

   --  ------------------------------
   --  Collect the list of files names for the resource (list is sorted).
   --  ------------------------------
   procedure Collect_Names (Resource    : in Resource_Type;
                            Ignore_Case : in Boolean;
                            Names       : in out Util.Strings.Vectors.Vector) is
   begin
      Names.Clear;

      for File in Resource.Files.Iterate loop
         declare
            Name : constant String := File_Maps.Key (File);
         begin
            if Ignore_Case then
               Names.Append (Ada.Characters.Handling.To_Upper (Name));
            else
               Names.Append (Name);
            end if;
         end;
      end loop;
   end Collect_Names;

   --  ------------------------------
   --  Create a new resource with the given name.
   --  ------------------------------
   procedure Create_Resource (List     : in out Resource_List;
                              Name     : in String;
                              Resource : out Resource_Access) is
   begin
      Resource := new Resource_Type;
      Resource.Name := Ada.Strings.Unbounded.To_Unbounded_String (Name);
      Resource.Next := List.Head;
      List.Head := Resource;
   end Create_Resource;

   --  ------------------------------
   --  Get the number of resources in the list.
   --  ------------------------------
   function Length (List : in Resource_List) return Natural is
      Count    : Natural := 0;
      Resource : Resource_Access := List.Head;
   begin
      while Resource /= null loop
         Count := Count + 1;
         Resource := Resource.Next;
      end loop;
      return Count;
   end Length;

   --  ------------------------------
   --  Get the path to write a file taking into account the output directory.
   --  ------------------------------
   function Get_Output_Path (Context : in Context_Type;
                             Name    : in String) return String is
   begin
      if Context.Output'Length = 0 or Context.Output.all = "." then
         return Name;
      else
         return Ada.Directories.Compose (Context.Output.all, Name);
      end if;
   end Get_Output_Path;

   --  ------------------------------
   --  Release the context information.
   --  ------------------------------
   overriding
   procedure Finalize (Context : in out Context_Type) is
      procedure Free is
        new Ada.Unchecked_Deallocation (Object => Resource_Type,
                                        Name   => Resource_Access);

      Resource : Resource_Access := Context.Resources.Head;
   begin
      while Resource /= null loop
         Context.Resources.Head := Resource.Next;
         Free (Resource);
         Resource := Context.Resources.Head;
      end loop;
   end Finalize;

   --  Report an error and set the exit status accordingly
   procedure Error (Context : in out Context_Type;
                    Message : in String;
                    Arg1    : in String;
                    Arg2    : in String := "") is
   begin
      Log.Error ("error: " & Message, Arg1, Arg2);
      Context.Status := Ada.Command_Line.Failure;
   end Error;

   procedure Error (Context : in out Context_Type;
                    Message : in String;
                    Arg1    : in UString;
                    Arg2    : in String := "") is
   begin
      Log.Error ("error: " & Message, To_String (Arg1), Arg2);
      Context.Status := Ada.Command_Line.Failure;
   end Error;

   --  ------------------------------
   --  Get a path to build/generate some content in a temporary file.
   --  ------------------------------
   function Get_Generation_Path (Context : in Context_Type;
                                 Name    : in String) return String is
   begin
      if Context.Tmp_Dir'Length > 0 then
         return Util.Files.Compose (Context.Tmp_Dir.all, Name);
      else
         return Util.Files.Compose ("are-generator", Name);
      end if;
   end Get_Generation_Path;

   --  ------------------------------
   --  Configure the logs.
   --  ------------------------------
   procedure Configure_Logs (Debug   : in Boolean;
                             Verbose : in Boolean) is
      Log_Config  : Util.Properties.Manager;
   begin
      Log_Config.Set ("log4j.rootCategory", "ERROR,errorConsole");
      Log_Config.Set ("log4j.appender.errorConsole", "Console");
      Log_Config.Set ("log4j.appender.errorConsole.level", "ERROR");
      Log_Config.Set ("log4j.appender.errorConsole.layout", "message");
      Log_Config.Set ("log4j.appender.errorConsole.stderr", "true");
      Log_Config.Set ("log4j.appender.errorConsole.prefix", "are: ");
      Log_Config.Set ("log4j.logger.Util", "FATAL");
      Log_Config.Set ("log4j.logger.Util.Events", "ERROR");
      Log_Config.Set ("log4j.logger.Are", "ERROR");
      if Verbose or Debug then
         Log_Config.Set ("log4j.logger.Util", "WARN");
         Log_Config.Set ("log4j.logger.Are", "INFO");
         Log_Config.Set ("log4j.rootCategory", "INFO,errorConsole,verbose");
         Log_Config.Set ("log4j.appender.verbose", "Console");
         Log_Config.Set ("log4j.appender.verbose.level", "INFO");
         Log_Config.Set ("log4j.appender.verbose.layout", "level-message");
      end if;
      if Debug then
         Log_Config.Set ("log4j.logger.Util.Processes", "INFO");
         Log_Config.Set ("log4j.logger.Are", "DEBUG");
         Log_Config.Set ("log4j.rootCategory", "DEBUG,errorConsole,debug");
         Log_Config.Set ("log4j.appender.debug", "Console");
         Log_Config.Set ("log4j.appender.debug.level", "DEBUG");
         Log_Config.Set ("log4j.appender.debug.layout", "full");
      end if;

      Util.Log.Loggers.Initialize (Log_Config);

   end Configure_Logs;

end Are;
