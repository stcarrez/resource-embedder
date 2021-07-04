-----------------------------------------------------------------------
--  are-installer-merges -- Web file merge
--  Copyright (C) 2020, 2021 Stephane Carrez
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
with Ada.Directories;
with Ada.Strings.Fixed;
with Ada.Streams.Stream_IO;
with Ada.Exceptions;
with Ada.IO_Exceptions;
with Ada.Unchecked_Deallocation;

with Util.Beans.Objects;
with Util.Log.Loggers;
with Util.Files;
with Util.Strings;
with Util.Streams.Files;
with Util.Streams.Texts;
with Util.Streams.Buffered;

with EL.Expressions;

with Are.Utils;
package body Are.Installer.Merges is

   use Util.Log;
   use Ada.Strings.Fixed;
   use Util.Beans.Objects;
   use Ada.Strings.Unbounded;
   use type Ada.Calendar.Time;

   procedure Process_Property (Rule : in out Merge_Rule;
                               Node : in DOM.Core.Node);
   procedure Process_Replace (Rule : in out Merge_Rule;
                              Node : in DOM.Core.Node);

   Log : constant Loggers.Logger := Loggers.Create ("Are.Installer.Merges");

   --  ------------------------------
   --  Extract the <property name='{name}'>{value}</property> to setup the
   --  EL context to evaluate the source and target links for the merge.
   --  ------------------------------
   procedure Process_Property (Rule : in out Merge_Rule;
                               Node : in DOM.Core.Node) is
      use Util.Beans.Objects.Maps;

      Name  : constant String := Are.Utils.Get_Attribute (Node, "name");
      Value : constant String := Are.Utils.Get_Data_Content (Node);
      Pos   : constant Natural := Util.Strings.Index (Name, '.');
   begin
      if Pos = 0 then
         Rule.Variables.Bind (Name, To_Object (Value));
         return;
      end if;

      --  A composed name such as 'jquery.path' must be split so that we store
      --  the 'path' value within a 'jquery' Map_Bean object.  We handle only
      --  one such composition.
      declare
         Param : constant String := Name (Name'First .. Pos - 1);
         Tag   : constant Unbounded_String := To_Unbounded_String (Param);
         Var   : constant EL.Expressions.Expression := Rule.Variables.Get_Variable (Tag);
         Val   : Object := Rule.Params.Get_Value (Param);
         Child : Map_Bean_Access;
      begin
         if Is_Null (Val) then
            Child := new Map_Bean;
            Val := To_Object (Child);
            Rule.Params.Set_Value (Param, Val);
         else
            Child := Map_Bean_Access (To_Bean (Val));
         end if;
         Child.Set_Value (Name (Pos + 1 .. Name'Last), To_Object (Value));
         if Var.Is_Null then
            Rule.Variables.Bind (Param, Val);
         end if;
      end;
   end Process_Property;

   procedure Process_Replace (Rule : in out Merge_Rule;
                              Node : in DOM.Core.Node) is
      From : constant String := Are.Utils.Get_Data_Content (Node, "from");
      To   : constant String := Are.Utils.Get_Data_Content (Node, "to");
   begin
      Rule.Replace.Include (From, To);
   end Process_Replace;

   procedure Iterate_Properties is
      new Are.Utils.Iterate_Nodes (Merge_Rule, Process_Property);

   procedure Iterate_Replace is
      new Are.Utils.Iterate_Nodes (Merge_Rule, Process_Replace);

   --  ------------------------------
   --  Create a distribution rule to copy a set of files or directories.
   --  ------------------------------
   function Create_Rule (Node : in DOM.Core.Node) return Distrib_Rule_Access is
      Result : constant Merge_Rule_Access := new Merge_Rule;
   begin
      Iterate_Properties (Result.all, Node, "property", False);
      Iterate_Replace (Result.all, Node, "replace", False);
      Result.Context.Set_Variable_Mapper (Result.Variables'Access);
      Result.Start_Mark := Are.Utils.Get_Attribute (Node, "merge-start", DEFAULT_MERGE_START);
      Result.End_Mark := Are.Utils.Get_Attribute (Node, "merge-end", DEFAULT_MERGE_END);
      return Result.all'Access;
   end Create_Rule;

   --  ------------------------------
   --  Get a name to qualify the installation rule (used for logs).
   --  ------------------------------
   overriding
   function Get_Install_Name (Rule : in Merge_Rule) return String is
      pragma Unreferenced (Rule);
   begin
      return "merge";
   end Get_Install_Name;

   overriding
   procedure Install (Rule    : in Merge_Rule;
                      Path    : in String;
                      Files   : in File_Vector;
                      Context : in out Context_Type'Class) is

      use Ada.Streams;

      type Mode_Type is (MERGE_NONE, MERGE_LINK, MERGE_SCRIPT);

      procedure Error (Message : in String;
                       Arg1    : in String := "");
      function Get_Target_Name (Link : in String) return String;
      function Get_Target_Path (Link : in String) return String;
      function Get_Filename (Line : in String) return String;
      function Get_Source (Line : in String;
                           Tag  : in String) return String;
      function Find_Match (Buffer : in Stream_Element_Array) return Stream_Element_Offset;
      procedure Prepare_Merge (Line : in String);
      procedure Include (Line    : in String;
                         Pattern : in String);
      procedure Process (Line : in String);

      Root_Dir   : constant String := Context.Get_Generation_Path (To_String (Rule.Dir));
      Source     : constant String := Get_Source_Path (Files, False);
      Target     : constant String := Context.Get_Generation_Path (Path);
      Dir        : constant String := Ada.Directories.Containing_Directory (Target);
      Build      : constant String := "23"; --  Context.Get_Parameter ("dynamo.build");
      Start_Mark : constant String := "<!-- " & To_String (Rule.Start_Mark) & " ";
      End_Mark   : constant String := "<!-- " & To_String (Rule.End_Mark) & " ";
      File_Time  : constant Ada.Calendar.Time := Ada.Directories.Modification_Time (Source);
      Modtime    : Ada.Calendar.Time := File_Time;
      Output     : aliased Util.Streams.Files.File_Stream;
      Merge      : aliased Util.Streams.Files.File_Stream;
      Merge_Path : UString;
      Merge_Name : UString;
      Text       : Util.Streams.Texts.Print_Stream;
      Mode       : Mode_Type := MERGE_NONE;
      Line_Num   : Natural := 0;
      Inc_Error  : Natural := 0;

      procedure Error (Message : in String;
                       Arg1    : in String := "") is
         Line : constant String := Util.Strings.Image (Line_Num);
      begin
         Context.Error (Source & ":" & Line & ": " & Message, Arg1);
      end Error;

      function Get_Target_Name (Link : in String) return String is
         Expr : EL.Expressions.Expression;
         File : Util.Beans.Objects.Object;
      begin
         Expr := EL.Expressions.Create_Expression (Link, Rule.Context);
         File := Expr.Get_Value (Rule.Context);
         declare
            Name : constant String := To_String (File);
         begin
            --  Drop the first '/' if there is one.
            if Name'Length > 0 and then Name (Name'First) = '/' then
               return Name (Name'First + 1 .. Name'Last);
            else
               return Name;
            end if;
         end;
      end Get_Target_Name;

      function Get_Target_Path (Link : in String) return String is
         Name : constant String := Get_Target_Name (Link);
      begin
         return Util.Files.Compose (Root_Dir, Name);
      end Get_Target_Path;

      function Get_Filename (Line : in String) return String is
         Pos  : Natural := Index (Line, "link=");
         Last : Natural;
      begin
         if Pos > 0 then
            Mode := MERGE_LINK;
         else
            Pos := Index (Line, "script=");
            if Pos > 0 then
               Mode := MERGE_SCRIPT;
            end if;
            if Pos = 0 then
               return "";
            end if;
         end if;
         Pos := Index (Line, "=");

         Last := Util.Strings.Index (Line, ' ', Pos + 1);
         if Last = 0 then
            return "";
         end if;
         return Line (Pos + 1 .. Last - 1);
      end Get_Filename;

      function Get_Source (Line : in String;
                           Tag  : in String) return String is
         Pos  : Natural := Index (Line, Tag);
         Last : Natural;
      begin
         if Pos = 0 then
            return "";
         end if;
         Pos := Pos + Tag'Length;
         if Pos > Line'Last or else (Line (Pos) /= '"' and Line (Pos) /= ''') then
            return "";
         end if;
         Last := Util.Strings.Index (Line, Line (Pos), Pos + 1);
         if Last = 0 then
            return "";
         end if;
         return Line (Pos + 1 .. Last - 1);
      end Get_Source;

      procedure Prepare_Merge (Line : in String) is
         Link        : constant String := Get_Filename (Line);
         Path        : constant String := Get_Target_Name (Link);
         Target_Path : constant String := Util.Files.Compose (Root_Dir, Path);
         Parent_Dir  : constant String := Ada.Directories.Containing_Directory (Target_Path);
      begin
         if Link'Length = 0 then
            Error ("invalid empty file name");
            return;
         end if;
         case Mode is
            when MERGE_LINK =>
               Text.Write ("<link media='screen' type='text/css' rel='stylesheet'"
                           & " href='");
               Text.Write (Link);
               Text.Write ("?build=");
               Text.Write (Build);
               Text.Write ("'/>" & ASCII.LF);

            when MERGE_SCRIPT =>
               Text.Write ("<script type='text/javascript' src='");
               Text.Write (Link);
               Text.Write ("?build=");
               Text.Write (Build);
               Text.Write ("'></script>" & ASCII.LF);

            when MERGE_NONE =>
               null;

         end case;
         if Rule.Level >= Util.Log.INFO_LEVEL then
            Log.Info ("  create {0}", Path);
         end if;
         if not Ada.Directories.Exists (Parent_Dir) then
            Ada.Directories.Create_Path (Parent_Dir);
         end if;
         Modtime := File_Time;
         Merge_Path := To_Unbounded_String (Target_Path);
         Merge_Name := To_Unbounded_String (Path);
         Merge.Create (Mode => Ada.Streams.Stream_IO.Out_File,
                       Name => Target_Path);
      end Prepare_Merge;

      Current_Match : Util.Strings.Maps.Cursor;

      function Find_Match (Buffer : in Stream_Element_Array) return Stream_Element_Offset is
         Iter  : Util.Strings.Maps.Cursor := Rule.Replace.First;
         First : Stream_Element_Offset := Buffer'Last + 1;
      begin
         while Util.Strings.Maps.Has_Element (Iter) loop
            declare
               Value : constant String := Util.Strings.Maps.Key (Iter);
               Match : Stream_Element_Array (1 .. Value'Length);
               for Match'Address use Value'Address;
               Pos   : Stream_Element_Offset;
            begin
               Pos := Buffer'First;
               while Pos + Match'Length < Buffer'Last loop
                  if Buffer (Pos .. Pos + Match'Length - 1) = Match then
                     if First > Pos then
                        First := Pos;
                        Current_Match := Iter;
                     end if;
                     exit;
                  end if;
                  Pos := Pos + 1;
               end loop;
            end;
            Util.Strings.Maps.Next (Iter);
         end loop;
         return First;
      end Find_Match;

      procedure Free is
        new Ada.Unchecked_Deallocation (Object => Ada.Streams.Stream_Element_Array,
                                        Name   => Util.Streams.Buffered.Buffer_Access);

      procedure Include (Line    : in String;
                         Pattern : in String) is
         Source : constant String := Get_Source (Line, Pattern);
         Target : constant String := Get_Target_Path (Source);
         Input  : Util.Streams.Files.File_Stream;
         Size   : Stream_Element_Offset;
         Last   : Stream_Element_Offset;
         Pos    : Stream_Element_Offset;
         Next   : Stream_Element_Offset;
         Buffer : Util.Streams.Buffered.Buffer_Access;
         Time   : Ada.Calendar.Time;
      begin
         if Source'Length = 0 then
            Inc_Error := Inc_Error + 1;
            if Inc_Error > 5 then
               return;
            end if;
            Error ("expecting a '{0}' in the line to identify the file to include", Pattern);
            if Inc_Error = 5 then
               Error ("may be the end marker '{0}' was not correct?", To_String (Rule.End_Mark));
            end if;
            return;
         end if;

         if Rule.Level >= Util.Log.INFO_LEVEL then
            Log.Info ("    include {0}", Target);
         end if;

         Time := Ada.Directories.Modification_Time (Target);
         if Time > Modtime then
            Modtime := Time;
         end if;
         Input.Open (Name => Target, Mode => Ada.Streams.Stream_IO.In_File);

         Size := Stream_Element_Offset (Ada.Directories.Size (Target));
         Buffer := new Stream_Element_Array (1 .. Size);
         Input.Read (Buffer.all, Last);
         Input.Close;

         Pos := 1;
         while Pos < Last loop
            Next := Find_Match (Buffer (Pos .. Last));
            if Next > Pos then
               Merge.Write (Buffer (Pos .. Next - 1));
            end if;
            exit when Next >= Last;
            declare
               Value   : constant String := Util.Strings.Maps.Key (Current_Match);
               Replace : constant String := Util.Strings.Maps.Element (Current_Match);
               Content : Stream_Element_Array (1 .. Replace'Length);
               for Content'Address use Replace'Address;
            begin
               Merge.Write (Content);
               Pos := Next + Value'Length;
            end;
         end loop;
         Free (Buffer);

      exception
         when Ex : Ada.IO_Exceptions.Name_Error =>
            Error ("cannot read {0}", Ada.Exceptions.Exception_Message (Ex));
      end Include;

      procedure Process (Line : in String) is
         Pos : Natural;
      begin
         Line_Num := Line_Num + 1;
         case Mode is
            when MERGE_NONE =>
               Pos := Index (Line, Start_Mark);
               if Pos = 0 then
                  Text.Write (Line);
                  Text.Write (ASCII.LF);
                  return;
               end if;
               Text.Write (Line (Line'First .. Pos - 1));
               Prepare_Merge (Line (Pos + 10 .. Line'Last));

            when MERGE_LINK =>
               Pos := Index (Line, End_Mark);
               if Pos > 0 then
                  Merge.Close;
                  if not Rule.Source_Timestamp then
                     Modtime := Ada.Directories.Modification_Time (To_String (Merge_Path));
                  end if;
                  Rule.Add_File (Name => To_String (Merge_Name),
                                 Path => To_String (Merge_Path),
                                 Modtime => Modtime,
                                 Override => True);
                  Mode := MERGE_NONE;
                  return;
               end if;
               Include (Line, "href=");

            when MERGE_SCRIPT =>
               Pos := Index (Line, End_Mark);
               if Pos > 0 then
                  Merge.Close;
                  if not Rule.Source_Timestamp then
                     Modtime := Ada.Directories.Modification_Time (To_String (Merge_Path));
                  end if;
                  Rule.Add_File (Name => To_String (Merge_Name),
                                 Path => To_String (Merge_Path),
                                 Modtime => Modtime,
                                 Override => True);
                  Mode := MERGE_NONE;
                  return;
               end if;
               Include (Line, "src=");

         end case;
      end Process;

   begin
      if Rule.Level >= Util.Log.INFO_LEVEL then
         Log.Info ("webmerge {0}", Path);
      end if;

      Ada.Directories.Create_Path (Dir);
      Output.Create (Name => Target, Mode => Ada.Streams.Stream_IO.Out_File);
      Text.Initialize (Output'Unchecked_Access, 16 * 1024);
      Util.Files.Read_File (Source, Process'Access);
      Text.Flush;
      Output.Close;

      Rule.Add_File (Path, Target, File_Time);
   end Install;

end Are.Installer.Merges;
