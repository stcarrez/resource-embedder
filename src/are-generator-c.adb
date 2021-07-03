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
with Ada.Text_IO;
with Ada.Calendar.Conversions;

with Interfaces.C;

with Util.Log.Loggers;

package body Are.Generator.C is

   use Ada.Text_IO;
   use Ada.Strings.Unbounded;

   function Get_Content_Type (Resource  : in Are.Resource_Type;
                              Context   : in Are.Context_Type'Class) return String;
   function Get_Type_Name (Prefix   : in String;
                           Resource : in Are.Resource_Type;
                           Context  : in Are.Context_Type'Class) return String;
   function To_File_Name (Name : in String) return String;
   function To_C_Name (Name : in String) return String;
   function To_Define_Name (Name    : in String;
                            Postfix : in String := "_H_") return String;
   function To_Prefix_Name (Name : in String) return String;

   --  Generate the resource declaration list.
   procedure Generate_Resource_Declarations (Resource     : in Are.Resource_Type;
                                             Content_Name : in String;
                                             Content_Type : in String;
                                             Into         : in out Ada.Text_IO.File_Type);

   --  Generate the resource content definition.
   procedure Generate_Resource_Contents (Resource     : in out Are.Resource_Type;
                                         Into         : in out Ada.Text_IO.File_Type);

   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("Are.Generator.C");

   --  ------------------------------
   --  Generate the C/C++ code for the resources that have been collected.
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
         end if;

         Generator.Generate_Header (Resource.all, Context);
         Generator.Generate_Source (Resource.all, Context);

         Generator.Names.Clear;
         Resource := Resource.Next;
      end loop;
   end Generate;

   function Get_Content_Type (Resource  : in Are.Resource_Type;
                              Context   : in Are.Context_Type'Class) return String is
   begin
      if Resource.Format = R_LINES then
         return Resource.Get_Content_Type_Name (Context, "const char* const*");
      end if;

      if Resource.Format = R_STRING then
         return Resource.Get_Content_Type_Name (Context, "const char*");
      end if;

      return Resource.Get_Content_Type_Name (Context, "const unsigned char *");
   end Get_Content_Type;

   function Get_Type_Name (Prefix   : in String;
                           Resource : in Are.Resource_Type;
                           Context  : in Are.Context_Type'Class) return String is
      Type_Name    : constant String := Resource.Get_Type_Name (Context, Prefix & "_content");
   begin
      if Util.Strings.Index (Type_Name, ' ') > 0 then
         return Type_Name;
      else
         return "struct " & Type_Name;
      end if;
   end Get_Type_Name;

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

   --  ------------------------------
   --  Given a package name, return the file name that correspond.
   --  ------------------------------
   function To_Define_Name (Name    : in String;
                            Postfix : in String := "_H_") return String is
      Result : String (Name'Range);
   begin
      for J in Name'Range loop
         if Name (J) in 'a' .. 'z' then
            Result (J) := Character'Val (Character'Pos (Name (J))
              - Character'Pos ('a')
              + Character'Pos ('A'));

         elsif Name (J) = '.' or Name (J) = ' ' then
            Result (J) := '_';

         else
            Result (J) := Name (J);
         end if;
      end loop;
      return "_" & Result & Postfix;
   end To_Define_Name;

   --  ------------------------------
   --  Given a package name, return a prefix to use for global variables.
   --  ------------------------------
   function To_Prefix_Name (Name : in String) return String is
      Result : String (Name'Range);
   begin
      for J in Name'Range loop
         if Name (J) in 'A' .. 'Z' then
            Result (J) := Character'Val (Character'Pos (Name (J))
              - Character'Pos ('A')
              + Character'Pos ('a'));

         elsif Name (J) = '.' or Name (J) = ' ' then
            Result (J) := '_';

         else
            Result (J) := Name (J);
         end if;
      end loop;
      return Result;
   end To_Prefix_Name;

   function To_C_Name (Name : in String) return String is
      Result : Unbounded_String;
   begin
      Append (Result, "Id_");
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
   end To_C_Name;

   --  ------------------------------
   --  Generate the resource declaration list.
   --  ------------------------------
   procedure Generate_Resource_Declarations (Resource     : in Are.Resource_Type;
                                             Content_Name : in String;
                                             Content_Type : in String;
                                             Into         : in out Ada.Text_IO.File_Type) is
      Remain : Natural := Natural (Resource.Files.Length);
   begin
      Put (Into, "enum");
      Put_Line (Into, " {");
      for File in Resource.Files.Iterate loop
         Put (Into, "   ");
         Put (Into, To_C_Name (File_Maps.Key (File)));
         Remain := Remain - 1;
         if Remain /= 0 then
            Put_Line (Into, ",");
         else
            New_Line (Into);
         end if;
      end loop;
      Put_Line (Into, "};");
      New_Line (Into);

      Put (Into, "extern const ");
      Put (Into, Content_Type);
      Put (Into, " ");
      Put (Into, Content_Name);
      Put_Line (Into, "[];");
      New_Line (Into);
   end Generate_Resource_Declarations;

   --  ------------------------------
   --  Generate the resource content definition.
   --  ------------------------------
   procedure Generate_Resource_Contents (Resource     : in out Are.Resource_Type;
                                         Into         : in out Ada.Text_IO.File_Type) is
      procedure Write_Binary (Name    : in String;
                              Content : in Are.File_Info);
      procedure Write_String (Content : in String);
      procedure Write_String (Name    : in String;
                              Content : in Are.File_Info);
      procedure Write_Lines (Name    : in String;
                             Content : in out Are.File_Info);
      procedure Write (File_Name : in String;
                       Content   : in out Are.File_Info);

      procedure Write_Binary (Name    : in String;
                              Content : in Are.File_Info) is
         Need_Sep : Boolean := False;
         Column   : Natural := 0;
      begin
         Put (Into, "static const unsigned char ");
         Put (Into, Name);
         Put (Into, "[] = {");
         if Content.Content = null or else Content.Content'Length = 0 then
            Put_Line (Into, "};");
         elsif Content.Content'Length = 1 then
            Put (Into, Util.Strings.Image (Natural (Content.Content (Content.Content'First))));
            Put_Line (Into, "};");
         else
            New_Line (Into);
            Put (Into, "  ");
            for C of Content.Content.all loop
               if Need_Sep then
                  Put (Into, ",");
                  Need_Sep := False;
               end if;
               if Column > 20 then
                  New_Line (Into);
                  Put (Into, "  ");
                  Column := 1;
               elsif Column > 0 then
                  Put (Into, " ");
               end if;
               Put (Into, Util.Strings.Image (Natural (C)));
               Column := Column + 1;
               Need_Sep := True;
            end loop;
            New_Line (Into);
            Put_Line (Into, "};");
         end if;
         New_Line (Into);
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
               Put (Into, "   ");
               Column := 3;
               Need_Sep := True;
            end if;
            case C is
               when ASCII.CR =>
                  Put (Into, "\r");
                  Column := Column + 2;

               when ASCII.LF =>
                  Put (Into, "\n");
                  Column := Column + 2;

               when ASCII.HT =>
                  Put (Into, "\t");
                  Column := Column + 2;

               when '"' =>
                  Put (Into, "\""");
                  Column := Column + 2;

               when others =>
                  if Need_Sep then
                     Put (Into, "  """);
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
         Put (Into, "static const char ");
         Put (Into, Name);
         Put (Into, "[] = ");
         if Content.Content /= null and then Content.Content'Length > 0 then
            declare
               First   : constant Natural := Natural (Content.Content'First);
               Last    : constant Natural := Natural (Content.Content'Last);

               File_Content : String (First .. Last);
               for File_Content'Address use Content.Content.all'Address;
            begin
               Write_String (File_Content);
            end;
         else
            Put (Into, """""");
         end if;
         Put_Line (Into, ";");
      end Write_String;

      procedure Write_Lines (Name    : in String;
                             Content : in out Are.File_Info) is
         Lines : Util.Strings.Vectors.Vector;
         Count : Natural := 0;
      begin
         Are.Convert_To_Lines (Resource, Content, Lines);
         Put (Into, "static const char *const ");
         Put (Into, Name);
         Put (Into, "[] = {");
         if Lines.Is_Empty then
            Put_Line (Into, "};");
         else
            New_Line (Into);
            for Line of Lines loop
               if Count >= 1 then
                  Put_Line (Into, ",");
               end if;
               Set_Col (Into, 3);
               Write_String (Line);
               Count := Count + 1;
            end loop;
            New_Line (Into);
            Put_Line (Into, "};");
         end if;
         Content.Line_Count := Count;
      end Write_Lines;

      Index : Natural := 0;

      procedure Write (File_Name : in String;
                       Content   : in out Are.File_Info) is
         pragma Unreferenced (File_Name);

         Name : constant String := "C_" & Util.Strings.Image (Index);
      begin
         if Resource.Format = R_LINES then
            Write_Lines (Name, Content);
         elsif Resource.Format = R_STRING then
            Write_String (Name, Content);
         else
            Write_Binary (Name, Content);
         end if;
      end Write;

   begin
      for File in Resource.Files.Iterate loop
         Index := Index + 1;

         Resource.Files.Update_Element (File, Write'Access);
         New_Line (Into);
      end loop;
   end Generate_Resource_Contents;

   --  ------------------------------
   --  Generate the package specification.
   --  ------------------------------
   procedure Generate_Header (Generator : in out Generator_Type;
                              Resource  : in out Are.Resource_Type;
                              Context   : in out Are.Context_Type'Class) is
      Name         : constant String := To_String (Resource.Name);
      Define       : constant String := To_Define_Name (Name);
      Filename     : constant String := To_File_Name (Name) & ".h";
      Path         : constant String := Context.Get_Output_Path (Filename);
      Prefix       : constant String := To_Prefix_Name (Name);
      Def_Func     : constant String := To_Prefix_Name (Name) & "_get_content";
      Content_Name : constant String := Prefix & "_contents";
      List_Names   : constant String := Prefix & "_names";
      Type_Name    : constant String := Get_Type_Name (Prefix, Resource, Context);
      Content_Type : constant String := Get_Content_Type (Resource, Context);
      Func_Name    : constant String := Resource.Get_Function_Name (Context, Def_Func);
      Type_Define  : constant String := To_Define_Name (Type_Name, "_TYPE_");
      File         : Ada.Text_IO.File_Type;
   begin
      Log.Info ("Writing resource {0} in {1}", Name, Path);

      Ada.Text_IO.Create (File => File,
                          Mode => Ada.Text_IO.Out_File,
                          Name => Path);
      Put (File, "// ");
      Put_Line (File, Get_Title);
      Put (File, "#ifndef ");
      Put_Line (File, Define);
      Put (File, "#define ");
      Put_Line (File, Define);
      New_Line (File);
      Put_Line (File, "#include <time.h>");
      New_Line (File);
      Put_Line (File, "#ifdef __cplusplus");
      Put_Line (File, "extern ""C"" {");
      Put_Line (File, "#endif");

      New_Line (File);

      --  Declare the struct holding the data.
      if not Context.No_Type_Declaration then
         Log.Debug ("Writing struct {0} declaration", Type_Name);

         Put (File, "#ifndef ");
         Put_Line (File, Type_Define);
         Put (File, "#define ");
         Put_Line (File, Type_Define);
         New_Line (File);
         Put (File, Type_Name);
         Put_Line (File, " {");
         Put (File, "  ");
         Put (File, Content_Type);
         Put (File, " ");
         Put (File, Resource.Get_Member_Content_Name (Context, "content"));
         Put_Line (File, ";");
         if Resource.Format = R_LINES then
            Put (File, "  size_t ");
            Put (File, Resource.Get_Member_Length_Name (Context, "length"));
            Put_Line (File, ";");
         else
            Put (File, "  size_t ");
            Put (File, Resource.Get_Member_Length_Name (Context, "size"));
            Put_Line (File, ";");
            Put (File, "  time_t ");
            Put (File, Resource.Get_Member_Modtime_Name (Context, "modtime"));
            Put_Line (File, ";");
            Put (File, "  int ");
            Put (File, Resource.Get_Member_Format_Name (Context, "format"));
            Put_Line (File, ";");
         end if;
         Put_Line (File, "};");
         New_Line (File);
         Put_Line (File, "#endif");
         New_Line (File);
      end if;

      if Context.List_Content then
         Put_Line (File, "// Sorted array of names composing the resource.");
         Put (File, "extern const char* const ");
         Put (File, List_Names);
         Put_Line (File, "[];");
         Put (File, "static const int ");
         Put (File, List_Names);
         Put (File, "_length = ");
         Put (File, Util.Strings.Image (Natural (Resource.Files.Length)));
         Put_Line (File, ";");
         New_Line (File);
      end if;

      if Context.Declare_Var then
         Generate_Resource_Declarations (Resource, Content_Name, Type_Name, File);
      end if;
      if Context.Name_Index then
         Log.Debug ("Writing {0} declaration", Func_Name);

         Put_Line (File, "// Returns the data stream with the given name or null.");
         Put (File, "extern const ");
         Put (File, Type_Name);
         Put (File, "* ");
         Put (File, Func_Name);
         Put (File, "(const char* name);");
         New_Line (File);
         New_Line (File);
      end if;

      Put_Line (File, "#ifdef __cplusplus");
      Put_Line (File, "}");
      Put_Line (File, "#endif");
      New_Line (File);

      Put (File, "#endif /* ");
      Put (File, Define);
      Put_Line (File, " */");
      Close (File);
   end Generate_Header;

   --  ------------------------------
   --  Generate the package body.
   --  ------------------------------
   procedure Generate_Source (Generator   : in out Generator_Type;
                              Resource    : in out Are.Resource_Type;
                              Context     : in out Are.Context_Type'Class) is

      procedure Generate_Keyword_Table (Into     : in out Ada.Text_IO.File_Type;
                                        Names    : in Util.Strings.Vectors.Vector);

      Name         : constant String := To_String (Resource.Name);
      Filename     : constant String := To_File_Name (Name) & ".c";
      Path         : constant String := Context.Get_Output_Path (Filename);
      Prefix       : constant String := To_Prefix_Name (Name);
      List_Names   : constant String := Prefix & "_names";
      Def_Func     : constant String := Prefix & "_get_content";
      Content_Name : constant String := Prefix & "_contents";
      Type_Name    : constant String := Get_Type_Name (Prefix, Resource, Context);
      Func_Name    : constant String := Resource.Get_Function_Name (Context, Def_Func);
      Count        : constant Natural := Natural (Resource.Files.Length);
      File         : Ada.Text_IO.File_Type;

      --  ------------------------------
      --  Generate the keyword table.
      --  ------------------------------
      procedure Generate_Keyword_Table (Into     : in out Ada.Text_IO.File_Type;
                                        Names    : in Util.Strings.Vectors.Vector) is
         Index : Integer := 0;
      begin
         New_Line (Into);
         if not Context.List_Content then
            Put (Into, "static ");
         end if;
         Put (Into, "const char* const ");
         Put (Into, List_Names);
         Put_Line (Into, "[] = {");
         for Name of Names loop
            if Index > 0 then
               Put_Line (Into, ",");
            end if;
            Put (Into, "  """);
            Put (Into, Name);
            Put (Into, """");
            Index := Index + 1;
         end loop;
         New_Line (Into);
         Put_Line (Into, "};");
         New_Line (Into);
      end Generate_Keyword_Table;

   begin
      Log.Info ("Writing resource {0} in {1}", Name, Path);

      Ada.Text_IO.Create (File => File,
                          Mode => Ada.Text_IO.Out_File,
                          Name => Path);
      Put (File, "// ");
      Put_Line (File, Get_Title);
      Put_Line (File, "#include <string.h>");
      Put (File, "#include """);
      Put (File, Filename (Filename'First .. Filename'Last - 1));
      Put_Line (File, "h""");
      New_Line (File);

      Generate_Resource_Contents (Resource, File);

      if Context.Name_Index then
         Generate_Keyword_Table (File, Generator.Names);
      end if;

      if Count >= 1 then

         Log.Debug ("Writing struct {0} contents[] with {1} entries",
                    Type_Name, Util.Strings.Image (Count));

         New_Line (File);
         if not Context.Declare_Var then
            Put (File, "static ");
         end if;
         Put (File, "const ");
         Put (File, Type_Name);
         Put (File, " ");
         Put (File, Content_Name);
         Put_Line (File, "[] = {");
         declare
            Need_Sep : Boolean := False;
            Col      : Natural := 0;
            Index    : Natural := 1;
         begin
            for Content in Resource.Files.Iterate loop
               if Need_Sep then
                  Put (File, ",");
                  New_Line (File);
               end if;
               Put (File, " { ");
               Put (File, "C_");
               Put (File, Util.Strings.Image (Index));
               Index := Index + 1;
               Put (File, ",");
               declare
                  use Ada.Calendar.Conversions;

                  Data : constant File_Info := File_Maps.Element (Content);
               begin
                  if Resource.Format = R_LINES then
                     Put (File, Natural'Image (Data.Line_Count));
                  else
                     Put (File, Ada.Directories.File_Size'Image (Data.Length));
                     Put (File, ",");
                     Put (File, Interfaces.C.long'Image (To_Unix_Time (Data.Modtime)));
                  end if;
               end;
               Put (File, " }");
               Col := Col + 1;
               Need_Sep := True;
            end loop;
         end;
         New_Line (File);
         Put_Line (File, "};");
      end if;

      if Context.Name_Index then
         Log.Debug ("Writing {0} implementation", Func_Name);

         Put_Line (File, "// Returns the data stream with the given name or null.");
         Put (File, "const ");
         Put (File, Type_Name);
         Put (File, "* ");
         Put (File, Func_Name);
         Put (File, "(const char* name)");
         New_Line (File);
         Put_Line (File, "{");
         if Count > 1 then
            Put_Line (File, "  int low = 0;");
            Put (File, "  int high = ");
            Put (File, Util.Strings.Image (Count - 1));
            Put_Line (File, ";");
            Put_Line (File, "  while (low <= high)");
            Put_Line (File, "    {");
            Put_Line (File, "      int mid = (low + high) / 2;");
            Put_Line (File, "      int cmp = strcmp(" & List_Names & "[mid], name);");
            Put_Line (File, "      if (cmp == 0)");
            Put (File, "        return &");
            Put (File, Content_Name);
            Put_Line (File, "[mid];");
            Put_Line (File, "      else if (cmp < 0)");
            Put_Line (File, "        low = mid + 1;");
            Put_Line (File, "      else");
            Put_Line (File, "        high = mid - 1;");
            Put_Line (File, "    }");
            Put_Line (File, "  return 0;");
         else
            Put (File, "  return (strcmp(");
            Put (File, List_Names);
            Put (File, "[0], name) == 0 ? &");
            Put (File, Content_Name);
            Put_Line (File, "[0] : 0);");
         end if;
         Put_Line (File, "}");
         New_Line (File);
      end if;

      Close (File);
   end Generate_Source;

end Are.Generator.C;
