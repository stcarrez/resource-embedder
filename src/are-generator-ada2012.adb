-----------------------------------------------------------------------
--  are-generator-ada2012 -- Generator for Ada
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
with Ada.Calendar.Conversions;
with Interfaces.C;

with Util.Files;
with Util.Log.Loggers;
with GNAT.Perfect_Hash_Generators;

package body Are.Generator.Ada2012 is

   use Ada.Text_IO;
   use Ada.Strings.Unbounded;
   use type Ada.Containers.Count_Type;

   function Get_Function_Type (Generator : in Generator_Type;
                               Resource  : in Are.Resource_Type;
                               Context   : in Are.Context_Type'Class) return String;
   function Get_Content_Type (Generator : in Generator_Type;
                              Resource  : in Are.Resource_Type;
                              Context   : in Are.Context_Type'Class) return String;

   --  Generate the resource declaration list.
   procedure Generate_Resource_Declarations (Resource     : in Are.Resource_Type;
                                             Into         : in out Ada.Text_IO.File_Type;
                                             Content_Type : in String;
                                             Var_Prefix   : in String);

   --  Generate the resource content definition.
   procedure Generate_Resource_Contents (Resource     : in Are.Resource_Type;
                                         Into         : in out Ada.Text_IO.File_Type;
                                         Declare_Var  : in Boolean;
                                         Content_Type : in String;
                                         Var_Prefix   : in String);

   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("Are.Generator.Ada2012");

   --  ------------------------------
   --  Generate the Ada code for the resources that have been collected.
   --  ------------------------------
   overriding
   procedure Generate (Generator : in out Generator_Type;
                       Resources : in Resource_List;
                       Context   : in out Are.Context_Type'Class) is
      Resource : Resource_Access := Resources.Head;
   begin
      while Resource /= null loop
         if Context.Name_Index then
            Resource.Collect_Names (Context.Ignore_Case, Generator.Names);
            if Generator.Names.Length > 1 then
               Generator.Generate_Perfect_Hash (Resource.all, Context);
            end if;
         end if;

         Generator.Generate_Specs (Resource.all, Context);
         Generator.Generate_Body (Resource.all, Context);

         Generator.Names.Clear;
         Resource := Resource.Next;
      end loop;
   end Generate;

   --  ------------------------------
   --  Setup the command line configuration to accept specific generation options.
   --  ------------------------------
   overriding
   procedure Setup (Generator : in out Generator_Type;
                    Config    : in out GC.Command_Line_Configuration) is
   begin
      GC.Define_Switch (Config => Config,
                        Output => Generator.Pragma_Preelaborate'Access,
                        Long_Switch => "--preelaborate",
                        Help   => -("[Ada] Generate a pragma Preelaborate in the specification"));
      GC.Define_Switch (Config => Config,
                        Output => Generator.Content_Only'Access,
                        Long_Switch => "--content-only",
                        Help   => -("[Ada] Give access only to the file content"));
   end Setup;

   function Get_Function_Type (Generator : in Generator_Type;
                               Resource  : in Are.Resource_Type;
                               Context   : in Are.Context_Type'Class) return String is
      Def_Type    : constant String := (if Generator.Content_Only then
                                           "Content_Access" else "Content_Type");
   begin
      return Resource.Get_Type_Name (Context, Def_Type);
   end Get_Function_Type;

   function Get_Content_Type (Generator : in Generator_Type;
                              Resource  : in Are.Resource_Type;
                              Context   : in Are.Context_Type'Class) return String is
      Func_Type   : constant String := Get_Function_Type (Generator, Resource, Context);
   begin
      if Resource.Format = R_LINES then
         return Resource.Get_Content_Type_Name (Context, "Content_Array");
      end if;

      if Func_Type = "access constant String" or Resource.Format = R_STRING then
         return Resource.Get_Content_Type_Name (Context, "String");
      end if;

      return Resource.Get_Content_Type_Name (Context, "Ada.Streams.Stream_Element_Array");
   end Get_Content_Type;

   --  ------------------------------
   --  Given a package name, return the file name that correspond.
   --  ------------------------------
   function To_File_Name (Name : in String) return String is
      Result : String (Name'Range);
   begin
      for J in Name'Range loop
         if Name (J) in 'A' .. 'Z' then
            Result (J) := Character'Val (Character'Pos (Name (J))
              - Character'Pos ('A')
              + Character'Pos ('a'));

         elsif Name (J) = '.' then
            Result (J) := '-';

         else
            Result (J) := Name (J);
         end if;
      end loop;
      return Result;
   end To_File_Name;

   function To_Ada_Name (Prefix : in String;
                         Name   : in String) return String is
      Result : Unbounded_String;
   begin
      Append (Result, Prefix);
      for C of Name loop
         if C = '-' or C = '.' then
            Append (Result, '_');

         elsif C >= 'a' and C <= 'z' then
            Append (Result, C);

         elsif C >= 'A' and C <= 'Z' then
            Append (Result, C);

         elsif C >= '0' and C <= '9' then
            Append (Result, C);
         end if;
      end loop;
      return To_String (Result);
   end To_Ada_Name;

   --  ------------------------------
   --  Generate the perfect hash implementation used by the name indexer.
   --  ------------------------------
   procedure Generate_Perfect_Hash (Generator : in out Generator_Type;
                                    Resource  : in Are.Resource_Type;
                                    Context   : in out Are.Context_Type'Class) is
      Count    : constant Positive := Positive (Generator.Names.Length);
      Pkg      : constant String := To_String (Resource.Name);
      Seed     : constant Natural := 4321; --  Needed by the hash algorithm
      K_2_V    : Float;
      V        : Natural;
   begin
      --  Collect the keywords for the hash.
      for Name of Generator.Names loop
         declare
            --  SCz 2021-07-03: s-pehage.adb:1900 index check failed is raised
            --  if we give a string that has a first index > 1.  Make a copy
            --  with new bounds.
            Word : constant String (1 .. Name'Length) := Name;
         begin
            GNAT.Perfect_Hash_Generators.Insert (Word);
         end;
      end loop;

      --  Generate the perfect hash package.
      V := 2 * Count + 1;
      loop
         K_2_V := Float (V) / Float (Count);
         GNAT.Perfect_Hash_Generators.Initialize (Seed, K_2_V);
         begin
            GNAT.Perfect_Hash_Generators.Compute;
            exit;
         exception
            when GNAT.Perfect_Hash_Generators.Too_Many_Tries =>
               V := V + 1;
         end;
      end loop;

      GNAT.Perfect_Hash_Generators.Produce (Pkg_Name => Pkg);

      GNAT.Perfect_Hash_Generators.Finalize;

      --  The perfect hash generator can only write files in the current directory.
      --  Move them to the target directory.
      if Context.Output'Length > 0 and Context.Output.all /= "." then
         declare
            Filename : String := To_File_Name (Pkg) & ".ads";
            Path     : String := Context.Get_Output_Path (Filename);
         begin
            if Ada.Directories.Exists (Path) then
               Ada.Directories.Delete_File (Path);
            end if;
            Ada.Directories.Rename (Filename, Path);

            Filename (Filename'Last) := 'b';
            Path (Path'Last) := 'b';
            if Ada.Directories.Exists (Path) then
               Ada.Directories.Delete_File (Path);
            end if;
            Ada.Directories.Rename (Filename, Path);
         end;
      end if;

   end Generate_Perfect_Hash;

   --  ------------------------------
   --  Generate the resource declaration list.
   --  ------------------------------
   procedure Generate_Resource_Declarations (Resource     : in Are.Resource_Type;
                                             Into         : in out Ada.Text_IO.File_Type;
                                             Content_Type : in String;
                                             Var_Prefix   : in String) is
   begin
      for File in Resource.Files.Iterate loop
         Put (Into, "   ");
         Put (Into, To_Ada_Name (Var_Prefix, File_Maps.Key (File)));
         Put (Into, " : aliased constant ");
         Put (Into, Content_Type);
         Put_Line (Into, ";");
      end loop;
      New_Line (Into);
   end Generate_Resource_Declarations;

   --  ------------------------------
   --  Generate the resource content definition.
   --  ------------------------------
   procedure Generate_Resource_Contents (Resource     : in Are.Resource_Type;
                                         Into         : in out Ada.Text_IO.File_Type;
                                         Declare_Var  : in Boolean;
                                         Content_Type : in String;
                                         Var_Prefix   : in String) is
      function Get_Variable_Name (Key : in String) return String;
      procedure Write_Binary (Name    : in String;
                              Content : in Are.File_Info);
      procedure Write_String (Content : in String);
      procedure Write_String (Name    : in String;
                              Content : in Are.File_Info);
      procedure Write_Lines (Name    : in String;
                             Content : in Are.File_Info);

      procedure Write_Binary (Name    : in String;
                              Content : in Are.File_Info) is
         Need_Sep : Boolean := False;
         Column   : Natural := 0;
      begin
         Put (Into, "   ");
         Put (Into, Name);
         Put (Into, " : aliased constant ");
         Put (Into, Content_Type);
         if Content.Content = null or else Content.Content'Length = 0 then
            Put_Line (Into, "(1 .. 0) := (others => <>);");
         elsif Content.Content'Length = 1 then
            Put (Into, " := (0 => ");
            Put (Into, Util.Strings.Image (Natural (Content.Content (Content.Content'First))));
            Put_Line (Into, ");");
         else
            Put_Line (Into, " :=");
            Put (Into, "     (");
            for C of Content.Content.all loop
               if Need_Sep then
                  Put (Into, ",");
                  Need_Sep := False;
               end if;
               if Column > 20 then
                  New_Line (Into);
                  Put (Into, "      ");
                  Column := 1;
               elsif Column > 0 then
                  Put (Into, " ");
               end if;
               Put (Into, Util.Strings.Image (Natural (C)));
               Column := Column + 1;
               Need_Sep := True;
            end loop;
            Put_Line (Into, ");");
         end if;
      end Write_Binary;

      procedure Write_String (Content : in String) is
         Need_Sep : Boolean := False;
         Column   : Natural := 0;
      begin
         Column := 40;
         Put (Into, """");
         for C of Content loop
            if Column > 80 then
               if not Need_Sep then
                  Put (Into, """");
               end if;
               New_Line (Into);
               Put (Into, "      ");
               Column := 6;
               Need_Sep := True;
            end if;
            case C is
               when ASCII.CR =>
                  if not Need_Sep then
                     Put (Into, """");
                     Need_Sep := True;
                  end if;
                  Put (Into, " & ASCII.CR");
                  Column := Column + 11;

               when ASCII.LF =>
                  if not Need_Sep then
                     Put (Into, """");
                     Need_Sep := True;
                  end if;
                  Put (Into, " & ASCII.LF");
                  Column := Column + 11;

               when '"' =>
                  if Need_Sep then
                     Put (Into, " & """);
                     Need_Sep := False;
                  end if;
                  Put (Into, """""");
                  Column := Column + 1;

               when others =>
                  if Need_Sep then
                     Put (Into, " & """);
                     Need_Sep := False;
                  end if;
                  Put (Into, C);

            end case;
            Column := Column + 1;
         end loop;
         if not Need_Sep then
            Put (Into, """");
         end if;
      end Write_String;

      procedure Write_String (Name    : in String;
                              Content : in Are.File_Info) is
      begin
         Put (Into, "   ");
         Put (Into, Name);
         Put (Into, " : aliased constant ");
         Put (Into, Content_Type);
         Put (Into, " := ");
         if Content.Content /= null and then Content.Content'Length > 0 then
            declare
               First   : constant Natural := Natural (Content.Content'First);
               Last    : constant Natural := Natural (Content.Content'Last);

               File_Content : String (First .. Last);
               for File_Content'Address use Content.Content.all'Address;
            begin
               Write_String (File_Content);
            end;
         end if;
         Put_Line (Into, ";");
      end Write_String;

      --  Line index is global for the resource.
      Line_Index : Natural := 0;

      procedure Write_Lines (Name    : in String;
                             Content : in Are.File_Info) is
         Lines : Util.Strings.Vectors.Vector;
         First : Natural := Line_Index;
      begin
         Are.Convert_To_Lines (Resource, Content, Lines);
         for Line of Lines loop
            Line_Index := Line_Index + 1;
            Put (Into, "   L_");
            Put (Into, Util.Strings.Image (Line_Index));
            Set_Col (Into, 10);
            Put (Into, ": aliased constant String := ");
            Write_String (Line);
            Put_Line (Into, ";");
         end loop;
         Put (Into, "   ");
         Put (Into, Name);
         Put (Into, " : aliased constant ");
         Put (Into, Content_Type);
         if Lines.Is_Empty then
            Put_Line (Into, "(1 .. 0) := (others => <>);");
         elsif Lines.Length = 1 then
            Put (Into, " := (1 => L_");
            Put (Into, Util.Strings.Image (First));
            Put_Line (Into, "'Access);");
         else
            Put_Line (Into, " :=");
            Put (Into, "     (");
            for I in 1 .. Lines.Length loop
               if I > 1 then
                  Put_Line (Into, ",");
                  Set_Col (Into, 7);
               end if;
               First := First + 1;
               Put (Into, "L_");
               Put (Into, Util.Strings.Image (First));
               Put (Into, "'Access");
            end loop;
            Put_Line (Into, ");");
         end if;
      end Write_Lines;

      Index : Natural := 0;

      function Get_Variable_Name (Key : in String) return String is
      begin
         if Declare_Var then
            return To_Ada_Name (Var_Prefix, Key);
         else
            return "C_" & Util.Strings.Image (Index);
         end if;
      end Get_Variable_Name;

   begin
      for File in Resource.Files.Iterate loop
         declare
            Name    : constant String := Get_Variable_Name (File_Maps.Key (File));
            Content : constant Are.File_Info := File_Maps.Element (File);
         begin
            Index := Index + 1;

            if Resource.Format = R_LINES then
               Write_Lines (Name, Content);
            elsif Resource.Format = R_STRING then
               Write_String (Name, Content);
            else
               Write_Binary (Name, Content);
            end if;
         end;
         New_Line (Into);
      end loop;
   end Generate_Resource_Contents;

   --  ------------------------------
   --  Generate the keyword table.
   --  ------------------------------
   procedure Generate_Keyword_Table (Generator : in out Generator_Type;
                                     Into      : in out Ada.Text_IO.File_Type) is

      Index : Integer := 0;

      procedure Print_Keyword (Pos : in Util.Strings.Vectors.Cursor);
      procedure Print_Table (Pos : in Util.Strings.Vectors.Cursor);

      --  ------------------------------
      --  Print a keyword as an Ada constant string.
      --  ------------------------------
      procedure Print_Keyword (Pos : in Util.Strings.Vectors.Cursor) is
         Name : constant String := Util.Strings.Vectors.Element (Pos);
      begin
         Put (Into, "   K_");
         Put (Into, Util.Strings.Image (Index));
         Set_Col (Into, 20);
         Put (Into, ": aliased constant String := """);
         Put (Into, Name);
         Put_Line (Into, """;");
         Index := Index + 1;
      end Print_Keyword;

      --  ------------------------------
      --  Build the keyword table.
      --  ------------------------------
      procedure Print_Table (Pos : in Util.Strings.Vectors.Cursor) is
         pragma Unreferenced (Pos);
      begin
         if Index > 0 then
            if Index mod 4 = 0 then
               Put_Line (Into, ",");
               Put (Into, "      ");
            else
               Put (Into, ", ");
            end if;
         end if;
         Put (Into, "K_");
         Put (Into, Util.Strings.Image (Index));
         Put (Into, "'Access");
         Index := Index + 1;
      end Print_Table;

   begin
      New_Line (Into);

      Generator.Names.Iterate (Print_Keyword'Access);
      New_Line (Into);

      Index := 0;
      Put_Line (Into, "   Names : constant Name_Array := (");
      Put (Into, "      ");
      if Generator.Names.Length = 1 then
         Put (Into, "0 => ");
      end if;
      Generator.Names.Iterate (Print_Table'Access);
      Put_Line (Into, ");");
   end Generate_Keyword_Table;

   --  ------------------------------
   --  Generate the package specification.
   --  ------------------------------
   procedure Generate_Specs (Generator   : in out Generator_Type;
                             Resource    : in Are.Resource_Type;
                             Context   : in out Are.Context_Type'Class) is
      Name         : constant String := To_String (Resource.Name);
      Filename     : constant String := To_File_Name (Name) & ".ads";
      Path         : constant String := Context.Get_Output_Path (Filename);
      Def_Type     : constant String := (if Generator.Content_Only then
                                            "Content_Access" else "Content_Type");
      Type_Name    : constant String := Resource.Get_Type_Name (Context, Def_Type);
      Content_Type : constant String := Get_Content_Type (Generator, Resource, Context);
      File         : Ada.Text_IO.File_Type;
      Has_Private  : Boolean := False;
   begin
      Log.Info ("Writing {0}", Path);

      Ada.Text_IO.Create (File => File,
                          Mode => Ada.Text_IO.Out_File,
                          Name => Path);
      Put (File, "--  ");
      Put_Line (File, Get_Title);
      if not Context.No_Type_Declaration then
         if Resource.Format = R_BINARY then
            Put_Line (File, "with Ada.Streams;");
         end if;
         if not Generator.Content_Only then
            Put_Line (File, "with Interfaces.C;");
         end if;
      end if;
      Put (File, "package ");
      Put (File, Name);
      Put_Line (File, " is");
      if Generator.Pragma_Preelaborate then
         New_Line (File);
         Put_Line (File, "   pragma Preelaborate;");
      end if;
      New_Line (File);
      if not Context.No_Type_Declaration then
         if Resource.Format = R_BINARY then
            Put_Line (File, "   type Content_Access is access constant"
                      & " Ada.Streams.Stream_Element_Array;");
         elsif Resource.Format = R_LINES then
            Put_Line (File, "   type Content_Array is array (Positive range <>)"
                      & " of access constant String;");
            Put_Line (File, "   type Content_Access is access constant Content_Array;");
         elsif Resource.Format = R_STRING then
            Put_Line (File, "   type Content_Access is access constant String;");
         end if;
         New_Line (File);
         if Context.List_Content or not Generator.Content_Only then
            Put_Line (File, "   type Name_Access is access constant String;");
            New_Line (File);
         end if;
         if not Generator.Content_Only then
            Put_Line (File, "   type Format_Type is (FILE_RAW, FILE_GZIP);");
            New_Line (File);
            Put (File, "   type ");
            Put (File, Type_Name);
            Put_Line (File, " is record");
            Put_Line (File, "      Name    : Name_Access;");
            Put_Line (File, "      Content : Content_Access;");
            Put_Line (File, "      Modtime : Interfaces.C.long := 0;");
            Put_Line (File, "      Format  : Format_Type := FILE_RAW;");
            Put_Line (File, "   end record;");
            New_Line (File);
            Put (File, "   Null_Content : constant ");
            Put (File, Type_Name);
            Put_Line (File, ";");
            New_Line (File);
         end if;
      end if;
      if Context.Declare_Var then
         Generate_Resource_Declarations (Resource, File, Content_Type, Context.Var_Prefix.all);
      end if;
      if Context.List_Content then
         if not Context.No_Type_Declaration then
            Put_Line (File, "   type Name_Array is array (Natural range <>) of Name_Access;");
            New_Line (File);
         end if;
         Put_Line (File, "   Names : constant Name_Array;");
         New_Line (File);
      end if;
      if Context.Name_Index then
         Put_Line (File, "   --  Returns the data stream with the given name or null.");
         Put (File, "   function Get_Content (Name : String) return ");
         Put (File, Type_Name);
         Put_Line (File, ";");
         New_Line (File);
      end if;
      if Context.Declare_Var then
         Put_Line (File, "private");
         New_Line (File);
         Has_Private := True;
         Generate_Resource_Contents (Resource, File, Context.Declare_Var,
                                     Content_Type, Context.Var_Prefix.all);
      end if;
      if not Context.No_Type_Declaration and not Generator.Content_Only then
         if not Has_Private then
            Put_Line (File, "private");
            New_Line (File);
            Has_Private := True;
         end if;
         Put (File, "   Null_Content : constant ");
         Put (File, Type_Name);
         Put_Line (File, " := (others => <>);");
         New_Line (File);
      end if;
      if Context.List_Content then
         if not Has_Private then
            Put_Line (File, "private");
            New_Line (File);
            Has_Private := True;
         end if;
         Generate_Keyword_Table (Generator, File);
      end if;
      Put      (File, "end ");
      Put      (File, Name);
      Put      (File, ";");
      New_Line (File);
      Close    (File);
   end Generate_Specs;

   --  ------------------------------
   --  Generate the package body.
   --  ------------------------------
   procedure Generate_Body (Generator   : in out Generator_Type;
                            Resource    : in Are.Resource_Type;
                            Context     : in out Are.Context_Type'Class) is

      procedure Read_Body (Line : in String);
      procedure Generate_Contents_Array;

      Name         : constant String := To_String (Resource.Name);
      Filename     : constant String := To_File_Name (Name) & ".adb";
      Path         : constant String := Context.Get_Output_Path (Filename);
      Def_Type     : constant String := (if Generator.Content_Only then
                                            "Content_Access" else "Content_Type");
      Type_Name    : constant String := Resource.Get_Type_Name (Context, Def_Type);
      Content_Type : constant String := Get_Content_Type (Generator, Resource, Context);
      Use_Hash     : constant Boolean := Context.Name_Index and Generator.Names.Length > 1;
      File         : Ada.Text_IO.File_Type;
      Count        : Natural;
      Lines        : Util.Strings.Vectors.Vector;

      --  ------------------------------
      --  Read the generated body file.
      --  ------------------------------
      procedure Read_Body (Line : in String) is
      begin
         Lines.Append (Line);
      end Read_Body;

      procedure Generate_Contents_Array is
         Need_Sep : Boolean := False;
         Col      : Natural := 0;
         Index    : Natural := 0;
      begin
         Put_Line (File, "   Contents : constant Content_List_Array := (");
         Put (File, "     ");
         if Resource.Files.Length = 1 then
            Put (File, "0 => ");
         end if;
         for Content in Resource.Files.Iterate loop
            if Need_Sep then
               Put (File, ",");
            end if;
            if Col > 5 then
               New_Line (File);
               Put (File, "      ");
               Col := 0;
            else
               Put (File, " ");
            end if;
            if not Generator.Content_Only then
               Put (File, "(K_");
               Put (File, Util.Strings.Image (Index));
               Put (File, "'Access, ");
            end if;
            if Context.Declare_Var then
               Put (File, To_Ada_Name (Context.Var_Prefix.all, File_Maps.Key (Content)));
            else
               Put (File, "C_");
               Put (File, Util.Strings.Image (Index));
            end if;
            Index := Index + 1;
            Put (File, "'Access");
            if not Generator.Content_Only then
               declare
                  use Ada.Calendar.Conversions;

                  Data : constant File_Info := File_Maps.Element (Content);
               begin
                  Put (File, ",");
                  Put (File, Interfaces.C.long'Image (To_Unix_Time (Data.Modtime)));
                  Put (File, ", FILE_RAW");
               end;
               Put (File, ")");
            end if;
            Col := Col + 1;
            Need_Sep := True;
         end loop;
         Put_Line (File, ");");
      end Generate_Contents_Array;

   begin
      if not Context.Name_Index or else Generator.Names.Is_Empty then
         Log.Debug ("Skipping body generation for {0}", Filename);
         return;
      end if;

      Log.Info ("Writing {0}", Path);

      if Use_Hash then
         Util.Files.Read_File (Path => Path, Process => Read_Body'Access);
      end if;

      Ada.Text_IO.Create (File => File,
                          Mode => Ada.Text_IO.Out_File,
                          Name => Path);
      Put (File, "--  ");
      Put_Line (File, Get_Title);
      if Context.Ignore_Case then
         Put_Line (File, "with Ada.Characters.Handling;");
      end if;
      if Use_Hash then
         Count := Natural (Lines.Length);
         for I in 1 .. Count - 1 loop
            declare
               L : constant String := Lines.Element (I);
            begin
               Put_Line (File, L);

               if Util.Strings.Starts_With (L, "package ") then
                  Put_Line (File, "   function Hash (S : String) return Natural;");
               end if;
            end;
         end loop;
      else
         Put (File, "package body ");
         Put (File, Name);
         Put_Line (File, " is");
      end if;

      if not Context.Declare_Var then
         Generate_Resource_Contents (Resource, File, Context.Declare_Var,
                                     Content_Type, Context.Var_Prefix.all);
      end if;

      if Context.Name_Index and not Context.List_Content then
         if Generator.Content_Only then
            Put_Line (File, "   type Name_Access is access constant String;");
         end if;
         Put_Line (File, "   type Name_Array is array "
                   & "(Natural range <>) of Name_Access;");
         New_Line (File);
         Generate_Keyword_Table (Generator, File);
      end if;

      New_Line (File);
      Put (File, "   type Content_List_Array is array (Natural range <>) of ");
      Put (File, Type_Name);
      Put_Line (File, ";");

      Generate_Contents_Array;

      if Context.Name_Index then
         New_Line (File);
         Put (File, "   function Get_Content (Name : String) return ");
         Put (File, Type_Name);
         Put_Line (File, " is");
         if Use_Hash then
            if Context.Ignore_Case then
               Put_Line (File, "      K : constant String := "
                         & "Ada.Characters.Handling.To_Upper (Name);");
               Put_Line (File, "      H : constant Natural := Hash (K);");
            else
               Put_Line (File, "      H : constant Natural := Hash (Name);");
            end if;
            Put_Line (File, "   begin");
            if Context.Ignore_Case then
               Put (File, "      return (if Names (H).all = K then Contents (H) else ");
            else
               Put (File, "      return (if Names (H).all = Name then Contents (H) else ");
            end if;
            if Generator.Content_Only then
               Put_Line (File, "null);");
            else
               Put_Line (File, "Null_Content);");
            end if;
         else
            if Context.Ignore_Case then
               Put_Line (File, "      K : constant String := "
                         & "Ada.Characters.Handling.To_Upper (Name);");
            end if;
            Put_Line (File, "   begin");
            if Context.Ignore_Case then
               Put (File, "      return (if Names (0).all = K then Contents (0) else ");
            else
               Put (File, "      return (if Names (0).all = Name then Contents (0) else ");
            end if;
            if Generator.Content_Only then
               Put_Line (File, "null);");
            else
               Put_Line (File, "Null_Content);");
            end if;
         end if;
         Put_Line (File, "   end Get_Content;");
         New_Line (File);
      end if;

      Put (File, "end ");
      Put (File, Name);
      Put_Line (File, ";");
      Close (File);
   end Generate_Body;

end Are.Generator.Ada2012;
