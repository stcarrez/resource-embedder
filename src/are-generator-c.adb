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
with Util.Strings.Transforms;

package body Are.Generator.C is

   use Ada.Text_IO;
   use Ada.Strings.Unbounded;

   function To_File_Name (Name : in String) return String;
   function To_C_Name (Name : in String) return String;
   function To_Define_Name (Name    : in String;
                            Postfix : in String := "_H_") return String;
   function To_Prefix_Name (Name : in String) return String;

   --  Generate the resource declaration list.
   procedure Generate_Resource_Declarations (Resource    : in Are.Resource_Type;
                                             Into        : in out Ada.Text_IO.File_Type);

   --  Generate the resource content definition.
   procedure Generate_Resource_Contents (Resource    : in Are.Resource_Type;
                                         Into        : in out Ada.Text_IO.File_Type;
                                         Declare_Var : in Boolean);

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
            Generator.Names.Clear;

            for File in Resource.Files.Iterate loop
               declare
                  Name : constant String := File_Maps.Key (File);
               begin
                  if Context.Ignore_Case then
                     Generator.Names.Append (Util.Strings.Transforms.To_Upper_Case (Name));
                  else
                     Generator.Names.Append (Name);
                  end if;
               end;
            end loop;
         end if;

         Generator.Generate_Header (Resource.all, Context);
         Generator.Generate_Source (Resource.all, Context);

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
      null;
   end Setup;

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

         elsif Name (J) = '.' then
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

         elsif Name (J) = '.' then
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
   procedure Generate_Resource_Declarations (Resource    : in Are.Resource_Type;
                                             Into        : in out Ada.Text_IO.File_Type) is
   begin
      for File in Resource.Files.Iterate loop
         Put (Into, "extern const unsigned char ");
         Put (Into, To_C_Name (File_Maps.Key (File)));
         Put_Line (Into, "[];");
      end loop;
      New_Line (Into);
   end Generate_Resource_Declarations;

   --  ------------------------------
   --  Generate the resource content definition.
   --  ------------------------------
   procedure Generate_Resource_Contents (Resource    : in Are.Resource_Type;
                                         Into        : in out Ada.Text_IO.File_Type;
                                         Declare_Var : in Boolean) is
      Index : Natural := 0;
   begin
      for File in Resource.Files.Iterate loop
         if Declare_Var then
            Put (Into, "const unsigned char ");
            Put (Into, To_C_Name (File_Maps.Key (File)));
         else
            Put (Into, "static const unsigned char ");
            Put (Into, "C_");
            Put (Into, Util.Strings.Image (Index));
            Index := Index + 1;
         end if;
         declare
            Content  : constant Are.File_Info := File_Maps.Element (File);
            Need_Sep : Boolean := False;
            Column   : Natural := 0;
         begin
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
         end;
         New_Line (Into);
      end loop;
   end Generate_Resource_Contents;

   --  ------------------------------
   --  Generate the package specification.
   --  ------------------------------
   procedure Generate_Header (Generator   : in out Generator_Type;
                              Resource    : in Are.Resource_Type;
                              Context   : in out Are.Context_Type'Class) is
      pragma Unreferenced (Generator);

      Name        : constant String := To_String (Resource.Name);
      Define      : constant String := To_Define_Name (Name);
      Filename    : constant String := To_File_Name (Name) & ".h";
      Path        : constant String := Context.Get_Output_Path (Filename);
      Prefix      : constant String := To_Prefix_Name (Name);
      Def_Func    : constant String := To_Prefix_Name (Name) & "_get_content";
      List_Names  : constant String := Prefix & "_names";
      Type_Name   : constant String := Resource.Get_Type_Name (Context, Prefix & "_content");
      Func_Name   : constant String := Resource.Get_Function_Name (Context, Def_Func);
      Type_Define : constant String := To_Define_Name (Type_Name, "_TYPE_");
      File        : Ada.Text_IO.File_Type;
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
         Put (File, "struct ");
         Put (File, Type_Name);
         Put_Line (File, " {");
         Put (File, "  const unsigned char *");
         Put (File, Resource.Get_Member_Content_Name (Context, "content"));
         Put_Line (File, ";");
         Put (File, "  size_t ");
         Put (File, Resource.Get_Member_Length_Name (Context, "size"));
         Put_Line (File, ";");
         Put (File, "  time_t ");
         Put (File, Resource.Get_Member_Modtime_Name (Context, "modtime"));
         Put_Line (File, ";");
         Put (File, "  int ");
         Put (File, Resource.Get_Member_Format_Name (Context, "format"));
         Put_Line (File, ";");
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
         Generate_Resource_Declarations (Resource, File);
      end if;
      if Context.Name_Index then
         Log.Debug ("Writing {0} declaration", Func_Name);

         Put_Line (File, "// Returns the data stream with the given name or null.");
         Put (File, "extern const struct ");
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
                              Resource    : in Are.Resource_Type;
                              Context     : in out Are.Context_Type'Class) is

      procedure Generate_Keyword_Table (Into     : in out Ada.Text_IO.File_Type;
                                        Names    : in Util.Strings.Vectors.Vector);

      Name        : constant String := To_String (Resource.Name);
      Filename    : constant String := To_File_Name (Name) & ".c";
      Path        : constant String := Context.Get_Output_Path (Filename);
      Prefix      : constant String := To_Prefix_Name (Name);
      List_Names  : constant String := Prefix & "_names";
      Def_Func    : constant String := Prefix & "_get_content";
      Type_Name   : constant String := Resource.Get_Type_Name (Context, Prefix & "_content");
      Func_Name   : constant String := Resource.Get_Function_Name (Context, Def_Func);
      Count       : constant Natural := Natural (Resource.Files.Length);
      File        : Ada.Text_IO.File_Type;

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
      if not Context.Name_Index or else Generator.Names.Is_Empty then
         Log.Info ("Skipping body generation for {0}", Filename);
         return;
      end if;

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

      Generate_Resource_Contents (Resource, File, Context.Declare_Var);

      if Context.Name_Index then
         Generate_Keyword_Table (File, Generator.Names);
      end if;

      if Count >= 1 then

         Log.Debug ("Writing struct {0} contents[] with {1} entries",
                    Type_Name, Util.Strings.Image (Count));

         New_Line (File);
         Put (File, "static const struct ");
         Put (File, Type_Name);
         Put_Line (File, " contents[] = {");
         declare
            Need_Sep : Boolean := False;
            Col      : Natural := 0;
            Index    : Natural := 0;
         begin
            for Content in Resource.Files.Iterate loop
               if Need_Sep then
                  Put (File, ",");
                  New_Line (File);
               end if;
               Put (File, " { ");
               if Context.Declare_Var then
                  Put (File, To_C_Name (File_Maps.Key (Content)));
               else
                  Put (File, "C_");
                  Put (File, Util.Strings.Image (Index));
                  Index := Index + 1;
               end if;
               Put (File, ",");
               declare
                  use Ada.Calendar.Conversions;

                  Data : constant File_Info := File_Maps.Element (Content);
               begin
                  Put (File, Ada.Directories.File_Size'Image (Data.Length));
                  Put (File, ",");
                  Put (File, Interfaces.C.long'Image (To_Unix_Time (Data.Modtime)));
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
         Put (File, "const struct ");
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
            Put_Line (File, "        return &contents[mid];");
            Put_Line (File, "      else if (cmp < 0)");
            Put_Line (File, "        low = mid + 1;");
            Put_Line (File, "      else");
            Put_Line (File, "        high = mid - 1;");
            Put_Line (File, "    }");
            Put_Line (File, "  return 0;");
         else
            Put_Line (File, "  return (strcmp(" & List_Names
                      & "[0], name) == 0 ? &contents[0] : 0);");
         end if;
         Put_Line (File, "}");
         New_Line (File);
      end if;

      Close (File);
   end Generate_Source;

end Are.Generator.C;
