-----------------------------------------------------------------------
--  are-generator-ada2012 -- Generator for Ada
--  Copyright (C) 2021, 2023, 2024 Stephane Carrez
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
   function Get_Index_Type (Generator : in Generator_Type;
                            Resource  : in Are.Resource_Type;
                            Context   : in Are.Context_Type'Class) return String;
   function Get_Content_Type (Generator : in Generator_Type;
                              Resource  : in Are.Resource_Type;
                              Context   : in Are.Context_Type'Class) return String;
   function Get_Import_Type (Cur_Pkg   : in String;
                             Type_Name : in String) return String;
   procedure Write_String (Into    : in out Ada.Text_IO.File_Type;
                           Content : in String);
   function Should_Break (Column  : in Natural;
                          Content : in String) return Boolean;

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
                                         Var_Prefix   : in String;
                                         Context      : in out Are.Context_Type'Class);

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
         if Resource.Format = R_MAP then
            Generator.Generate_Resource_Mapping (Resource.all, Context);
         elsif Context.Name_Access or else Resource.Name_Access then
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
      Content_Only : constant Boolean := Generator.Content_Only or else Resource.Content_Only;
      Def_Type     : constant String := (if Content_Only then
                                            "Content_Access" else "Content_Type");
   begin
      return Resource.Get_Type_Name (Context, Def_Type);
   end Get_Function_Type;

   function Get_Index_Type (Generator : in Generator_Type;
                            Resource  : in Are.Resource_Type;
                            Context   : in Are.Context_Type'Class) return String is
      pragma Unreferenced (Generator);
   begin
      return Resource.Get_Index_Type_Name (Context, "Natural");
   end Get_Index_Type;

   function Get_Content_Type (Generator : in Generator_Type;
                              Resource  : in Are.Resource_Type;
                              Context   : in Are.Context_Type'Class) return String is
      Func_Type   : constant String := Get_Function_Type (Generator, Resource, Context);
   begin
      if Resource.Format = R_LINES then
         return Resource.Get_Content_Type_Name (Context, "Content_Array");
      end if;

      if Util.Strings.Starts_With (Func_Type, "access constant ") then
         return Resource.Get_Content_Type_Name
           (Context, Func_Type (Func_Type'First + 16 .. Func_Type'Last));
      end if;

      if Resource.Format = R_STRING then
         return Resource.Get_Content_Type_Name (Context, "String");
      end if;

      return Resource.Get_Content_Type_Name (Context, "Ada.Streams.Stream_Element_Array");
   end Get_Content_Type;

   --  ------------------------------
   --  Get the import package name for the given type.  We try to guess if
   --  `Type_Name` is declared in a parent package of the current unit.
   --  ------------------------------
   function Get_Import_Type (Cur_Pkg   : in String;
                             Type_Name : in String) return String is
      Pos : constant Natural := Util.Strings.Rindex (Type_Name, '.');
   begin
      if Pos = 0 then
         return "";
      end if;
      declare
         Pkg_Name : constant String := Type_Name (Type_Name'First .. Pos - 1);
      begin
         if Util.Strings.Starts_With (Cur_Pkg, Pkg_Name) then
            return "";
         end if;
         return Pkg_Name;
      end;
   end Get_Import_Type;

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
         if C in '-' | '.' then
            Append (Result, '_');

         elsif C in 'a' .. 'z' then
            Append (Result, C);

         elsif C in 'A' .. 'Z' then
            Append (Result, C);

         elsif C in '0' .. '9' then
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
      if Context.Output'Length > 0 and then Context.Output.all /= "." then
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

   function Should_Break (Column  : in Natural;
                          Content : in String) return Boolean is
      Count : Natural := 0;
   begin
      if Column >= 70 then
         for C of Content loop
            if C in ' ' | '!' | '#' .. '~' then
               return Count + Column > 80;
            end if;
            if C < ' ' then
               return True;
            end if;
            Count := Count + 1;
            if Count > 15 then
               return True;
            end if;
         end loop;
      end if;
      return False;
   end Should_Break;

   procedure Write_String (Into    : in out Ada.Text_IO.File_Type;
                           Content : in String) is
      Need_Sep : Boolean := False;
      Column   : Natural := 0;
      C        : Character;
      Pos      : Natural := Content'First;
   begin
      Column := 40;
      Put (Into, """");
      while Pos <= Content'Last loop
         C := Content (Pos);
         if Should_Break (Column, Content (Pos .. Content'Last)) then
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
                  Column := Column + 1;
               end if;
               Put (Into, " & ASCII.CR");
               Column := Column + 11;

            when ASCII.LF =>
               if not Need_Sep then
                  Put (Into, """");
                  Need_Sep := True;
                  Column := Column + 1;
               end if;
               Put (Into, " & ASCII.LF");
               Column := Column + 11;

            when ASCII.HT =>
               if not Need_Sep then
                  Put (Into, """");
                  Need_Sep := True;
                  Column := Column + 1;
               end if;
               Put (Into, " & ASCII.HT");
               Column := Column + 11;

            when '"' =>
               if Need_Sep then
                  Put (Into, " & """);
                  Need_Sep := False;
                  Column := Column + 5;
               end if;
               Put (Into, """""");
               Column := Column + 1;

            when ' ' | '!' | '#' .. '~' =>
               if Need_Sep then
                  Put (Into, " & """);
                  Need_Sep := False;
                  Column := Column + 5;
               end if;
               Put (Into, C);

            when Character'Val (192) .. Character'Val (255) =>
               if Need_Sep then
                  Put (Into, " & """);
                  Need_Sep := False;
                  Column := Column + 5;
               end if;
               Put (Into, C);
               while Pos + 1 <= Content'Last loop
                  C := Content (Pos + 1);
                  exit when Character'Pos (C) < 128;
                  exit when Character'Pos (C) >= 192;
                  Pos := Pos + 1;
                  Put (Into, C);
               end loop;

            when others =>
               if not Need_Sep then
                  Put (Into, """");
                  Need_Sep := True;
                  Column := Column + 1;
               end if;
               Put (Into, " & Character'Val (");
               Put (Into, Util.Strings.Image (Integer (Character'Pos (C))));
               Put (Into, ")");
               Column := Column + 22;

         end case;
         Column := Column + 1;
         Pos := Pos + 1;
      end loop;
      if not Need_Sep then
         Put (Into, """");
      end if;
   end Write_String;

   --  ------------------------------
   --  Generate the resource content definition.
   --  ------------------------------
   procedure Generate_Resource_Contents (Resource     : in Are.Resource_Type;
                                         Into         : in out Ada.Text_IO.File_Type;
                                         Declare_Var  : in Boolean;
                                         Content_Type : in String;
                                         Var_Prefix   : in String;
                                         Context      : in out Are.Context_Type'Class) is
      pragma Unreferenced (Context);
      function Get_Variable_Name (Key : in String) return String;
      procedure Write_Binary (Name    : in String;
                              Content : in Are.File_Info);
      procedure Write_String (Name    : in String;
                              Content : in Are.File_Info);
      procedure Write_Lines (Name    : in String;
                             Content : in Are.File_Info);

      procedure Write_Binary (Name    : in String;
                              Content : in Are.File_Info) is
         Need_Sep : Boolean := False;
         Column   : Natural := 0;
         Index    : Positive := 1;
      begin
         Put (Into, "   ");
         Put (Into, Name);
         Put (Into, " : aliased constant ");
         Put (Into, Content_Type);
         if Content.Content = null or else Content.Content'Length = 0 then
            Put_Line (Into, "(1 .. 0) := (others => <>);");
         elsif Content.Content'Length = 1 then
            Put (Into, " := (1 => ");
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
               if Column > 60 then
                  New_Line (Into);
                  Put (Into, "      ");
                  Column := 6;
               elsif Column > 0 then
                  Put (Into, " ");
               end if;
               declare
                  Idx : constant String := Util.Strings.Image (Index);
                  S   : constant String := Util.Strings.Image (Natural (C));
               begin
                  Put (Into, Idx);
                  Put (Into, " => ");
                  Put (Into, S);
                  Column := Column + S'Length + Idx'Length + 4 + 2;
               end;
               Index := Index + 1;
               Need_Sep := True;
            end loop;
            Put_Line (Into, ");");
         end if;
      end Write_Binary;

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
               Write_String (Into, File_Content);
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
            Write_String (Into, Line);
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
            Path    : constant String := File_Maps.Key (File);
            Name    : constant String := Get_Variable_Name (Path);
            Content : constant Are.File_Info := File_Maps.Element (File);
         begin
            Index := Index + 1;

            if Resource.Format = R_LINES then
               Write_Lines (Name, Content);
            elsif Resource.Format = R_STRING then
               Write_String (Name, Content);
            elsif Resource.Format = R_BINARY then
               Write_Binary (Name, Content);
            end if;
         end;
         New_Line (Into);
      end loop;
   end Generate_Resource_Contents;

   --  ------------------------------
   --  Generate the mapping table for the resource (when format = R_MAP).
   --  ------------------------------
   procedure Generate_Resource_Mapping (Generator    : in out Generator_Type;
                                        Resource     : in Are.Resource_Type;
                                        Context      : in out Are.Context_Type'Class) is
   begin
      Generator.Map.Clear;
      Generator.Names.Clear;

      --  Build the map by reading files.
      for File in Resource.Files.Iterate loop
         declare
            Path    : constant String := File_Maps.Key (File);
            Content : constant Are.File_Info := File_Maps.Element (File);
         begin
            Convert_To_Map (Resource, Path, Content, Context, Generator.Map);
         end;
      end loop;

      --  Collect the keys for the generation of the perfect hash function.
      for Item in Generator.Map.Iterate loop
         declare
            Name : constant String := Util.Strings.Maps.Key (Item);
         begin
            Generator.Names.Append (Name);
         end;
      end loop;

      if Generator.Names.Length > 1 then
         Generator.Generate_Perfect_Hash (Resource, Context);
      end if;
   end Generate_Resource_Mapping;

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
   --  Generate the mapping table.
   --  ------------------------------
   procedure Generate_Mapping_Table (Generator : in out Generator_Type;
                                     Into      : in out Ada.Text_IO.File_Type) is

      Index    : Integer := 0;
      Key_Mode : Boolean := True;

      procedure Print_Mapping (Pos : in Util.Strings.Maps.Cursor);
      procedure Print_Table (Pos : in Util.Strings.Maps.Cursor);

      --  ------------------------------
      --  Print a value associated with the key as an Ada constant string.
      --  ------------------------------
      procedure Print_Mapping (Pos : in Util.Strings.Maps.Cursor) is
         Key   : constant String := Util.Strings.Maps.Key (Pos);
         Value : constant String := Util.Strings.Maps.Element (Pos);
      begin
         Put (Into, "   K_");
         Put (Into, Util.Strings.Image (Index));
         Set_Col (Into, 20);
         Put (Into, ": aliased constant String := ");
         Write_String (Into, Key);
         Put_Line (Into, ";");
         Put (Into, "   M_");
         Put (Into, Util.Strings.Image (Index));
         Set_Col (Into, 20);
         Put (Into, ": aliased constant String := ");
         Write_String (Into, Value);
         Put_Line (Into, ";");
         Index := Index + 1;
      end Print_Mapping;

      --  ------------------------------
      --  Build the mapping table.
      --  ------------------------------
      procedure Print_Table (Pos : in Util.Strings.Maps.Cursor) is
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
         Put (Into, (if Key_Mode then "K_" else "M_"));
         Put (Into, Util.Strings.Image (Index));
         Put (Into, "'Access");
         Index := Index + 1;
      end Print_Table;

   begin
      Generator.Map.Iterate (Print_Mapping'Access);
      New_Line (Into);

      Index := 0;
      Put_Line (Into, "   Names : constant Name_Array := (");
      Put (Into, "      ");
      if Generator.Map.Length = 1 then
         Put (Into, "0 => ");
      end if;
      Generator.Map.Iterate (Print_Table'Access);
      Put_Line (Into, ");");
      New_Line (Into);

      Index := 0;
      Key_Mode := False;
      Put_Line (Into, "   Contents : constant Name_Array := (");
      Put (Into, "      ");
      if Generator.Map.Length = 1 then
         Put (Into, "0 => ");
      end if;
      Generator.Map.Iterate (Print_Table'Access);
      Put_Line (Into, ");");
   end Generate_Mapping_Table;

   --  ------------------------------
   --  Generate the package specification.
   --  ------------------------------
   procedure Generate_Specs (Generator   : in out Generator_Type;
                             Resource    : in Are.Resource_Type;
                             Context   : in out Are.Context_Type'Class) is
      Name         : constant String := To_String (Resource.Name);
      Filename     : constant String := To_File_Name (Name) & ".ads";
      Path         : constant String := Context.Get_Output_Path (Filename);
      Content_Only : constant Boolean := Generator.Content_Only or else Resource.Content_Only;
      Def_Type     : constant String := (if Content_Only then
                                            "Content_Access" else "Content_Type");
      Type_Name    : constant String := Resource.Get_Type_Name (Context, Def_Type);
      Content_Type : constant String := Get_Content_Type (Generator, Resource, Context);
      Var_Access   : constant Boolean := Context.Var_Access or else Resource.Var_Access;
      No_Type_Declaration : constant Boolean
        := Context.No_Type_Declaration or else Resource.No_Type_Declaration;
      File         : Ada.Text_IO.File_Type;
      Has_Private  : Boolean := False;
      Name_Access  : constant Boolean := Context.Name_Access or else Resource.Name_Access;
      List_Access  : constant Boolean := Context.List_Access or else Resource.List_Access;
   begin
      Log.Info ("Writing {0}", Path);

      Ada.Text_IO.Create (File => File,
                          Mode => Ada.Text_IO.Out_File,
                          Name => Path);
      Put (File, "--  ");
      Put_Line (File, Get_Title);
      Put_Lines (File, Resource.Headers_Spec);
      if not No_Type_Declaration then
         if Resource.Format = R_BINARY then
            declare
               Pkg_Import : constant String := Get_Import_Type (Name, Content_Type);
            begin
               if Pkg_Import'Length > 0 then
                  Put (File, "with ");
                  Put (File, Pkg_Import);
                  Put_Line (File, ";");
               end if;
            end;
         end if;
         if not Content_Only and then Resource.Format /= R_MAP then
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
      if not No_Type_Declaration then
         if Resource.Format = R_BINARY then
            Put (File, "   type Content_Access is access constant ");
            Put (File, Content_Type);
            Put_Line (File, ";");
         elsif Resource.Format = R_LINES then
            Put_Line (File, "   type Content_Array is array (Positive range <>)"
                      & " of access constant String;");
            Put_Line (File, "   type Content_Access is access constant Content_Array;");
         elsif Resource.Format = R_STRING then
            Put_Line (File, "   type Content_Access is access constant String;");
         elsif Resource.Format = R_MAP then
            Put_Line (File, "   type Content_Access is access constant String;");
         end if;
         New_Line (File);
         if List_Access
           or else not Content_Only
           or else Resource.Format = R_MAP
         then
            Put_Line (File, "   type Name_Access is access constant String;");
            New_Line (File);
         end if;
         if not Content_Only and then Resource.Format /= R_MAP then
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
      if Var_Access then
         Generate_Resource_Declarations (Resource, File, Content_Type,
                                         (if Length (Resource.Var_Prefix) > 0 then
                                             To_String (Resource.Var_Prefix)
                                          else Context.Var_Prefix.all));
      end if;
      if List_Access then
         Put (File, "   Names_Count : constant :=");
         Put (File, Generator.Names.Length'Image);
         Put_Line (File, ";");
         if not No_Type_Declaration then
            Put (File, "   type Name_Array is array (");
            Put (File, Get_Index_Type (Generator, Resource, Context));
            Put_Line (File, " range <>) of Name_Access;");
            New_Line (File);
         end if;
         Put_Line (File, "   Names : constant Name_Array;");
         New_Line (File);
      end if;
      if Resource.Format = R_MAP then
         if Name_Access then
            Put_Line (File, "   Contents : constant Name_Array;");
            New_Line (File);
         end if;
         Put_Line (File, "   --  Returns the mapping that corresponds to the name or null.");
         Put_Line (File, "   function Get_Mapping (Name : String) return");
         Put (File, "      ");
         Put (File, Type_Name);
         Put_Line (File, ";");
         New_Line (File);
      else
         if Name_Access then
            Put_Line (File, "   --  Returns the data stream with the given name or null.");
            Put_Line (File, "   function Get_Content (Name : String) return");
            Put (File, "      ");
            Put (File, Type_Name);
            Put_Line (File, ";");
            New_Line (File);
         end if;
      end if;
      if Var_Access then
         Put_Line (File, "private");
         New_Line (File);
         Has_Private := True;
         Generate_Resource_Contents (Resource, File, Var_Access,
                                     Content_Type, Context.Var_Prefix.all, Context);
      end if;
      if not No_Type_Declaration
        and then not Content_Only
        and then Resource.Format /= R_MAP
      then
         if not Has_Private then
            Put_Line (File, "private");
            Has_Private := True;
         end if;
         Put (File, "   Null_Content : constant ");
         Put (File, Type_Name);
         Put_Line (File, " := (others => <>);");
         New_Line (File);
      end if;
      if List_Access and then (Resource.Format /= R_MAP or else not Name_Access) then
         if not Has_Private then
            Put_Line (File, "private");
            New_Line (File);
            Has_Private := True;
         end if;
         Generate_Keyword_Table (Generator, File);
      end if;
      if Resource.Format = R_MAP and then List_Access and then Name_Access then
         if not Has_Private then
            Put_Line (File, "private");
            New_Line (File);
            Has_Private := True;
         end if;
         Generate_Mapping_Table (Generator, File);
         New_Line (File);
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
      Content_Only : constant Boolean := Generator.Content_Only or else Resource.Content_Only;
      Def_Type     : constant String := (if Content_Only then
                                            "Content_Access" else "Content_Type");
      Type_Name    : constant String := Resource.Get_Type_Name (Context, Def_Type);
      Content_Type : constant String := Get_Content_Type (Generator, Resource, Context);
      Index_Type   : constant String := Get_Index_Type (Generator, Resource, Context);
      Name_Access  : constant Boolean := Context.Name_Access or else Resource.Name_Access;
      Use_Mapping  : constant Boolean := Resource.Format = R_MAP;
      Use_Hash     : constant Boolean :=
        (Use_Mapping or else Name_Access) and then Generator.Names.Length > 1;
      Var_Access   : constant Boolean := Context.Var_Access or else Resource.Var_Access;
      List_Access  : constant Boolean := Context.List_Access or else Resource.List_Access;
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
            if Col > 4 then
               New_Line (File);
               Put (File, "      ");
               Col := 0;
            else
               Put (File, " ");
            end if;
            if not Content_Only then
               Put (File, "(K_");
               Put (File, Util.Strings.Image (Index));
               Put (File, "'Access, ");
            end if;
            if Var_Access then
               Put (File, To_Ada_Name (Context.Var_Prefix.all, File_Maps.Key (Content)));
            else
               Put (File, "C_");
               Put (File, Util.Strings.Image (Index));
            end if;
            Index := Index + 1;
            Put (File, "'Access");
            if not Content_Only then
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
      if (not Use_Mapping and then not Name_Access)
        or else Generator.Names.Is_Empty
      then
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
      Put_Lines (File, Resource.Headers_Impl);
      if Context.Ignore_Case then
         Put_Line (File, "with Ada.Characters.Handling;");
      end if;
      if Use_Hash then
         Count := Natural (Lines.Length);
         for I in 1 .. Count - 1 loop
            declare
               L : constant String := Lines.Element (I);
            begin
               exit when I = Count - 1 and then L = "";
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

      if not Var_Access then
         New_Line (File);
         Generate_Resource_Contents (Resource, File, False,
                                     Content_Type, "", Context);
      end if;

      if Name_Access and then not List_Access then
         if Content_Only then
            Put_Line (File, "   type Name_Access is access constant String;");
         end if;
         Put_Line (File, "   type Name_Array is array "
                   & "(Natural range <>) of Name_Access;");
         Generate_Keyword_Table (Generator, File);
      end if;

      if Resource.Format = R_MAP then
         if not Name_Access and then not List_Access then
            Put_Line (File, "   type Name_Array is array "
                      & "(Natural range <>) of Content_Access;");
            New_Line (File);
            Generate_Mapping_Table (Generator, File);
         end if;
      end if;

      if Resource.Format /= R_MAP then
         New_Line (File);
         Put (File, "   type Content_List_Array is array (Natural range <>) of ");
         Put (File, Type_Name);
         Put_Line (File, ";");

         Generate_Contents_Array;
      end if;

      if Name_Access or else Resource.Format = R_MAP then
         New_Line (File);
         if Resource.Format = R_MAP then
            Put (File, "   function Get_Mapping (Name : String) return ");
         else
            Put (File, "   function Get_Content (Name : String) return ");
         end if;
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
            Put (File, "      return (if Names (");
            if Index_Type /= "Natural" then
               Put (File, Index_Type);
               Put (File, " (H)).all = ");
            else
               Put (File, "H).all = ");
            end if;
            if Context.Ignore_Case then
               Put (File, "K then Contents (H) else ");
            else
               Put (File, "Name then Contents (H) else ");
            end if;
            if Content_Only or else Use_Mapping then
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
            Put (File, "      return (if Names (Names'First).all = ");
            if Context.Ignore_Case then
               Put (File, "K then Contents (0) else ");
            else
               Put (File, "Name then Contents (0) else ");
            end if;
            if Content_Only or else Use_Mapping then
               Put_Line (File, "null);");
            else
               Put_Line (File, "Null_Content);");
            end if;
         end if;
         if Resource.Format = R_MAP then
            Put_Line (File, "   end Get_Mapping;");
         else
            Put_Line (File, "   end Get_Content;");
         end if;
         New_Line (File);
      end if;

      Put (File, "end ");
      Put (File, Name);
      Put_Line (File, ";");
      Close (File);
   end Generate_Body;

end Are.Generator.Ada2012;
