-----------------------------------------------------------------------
--  are-generator-go -- Generator for Go
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

package body Are.Generator.Go is

   use Ada.Text_IO;
   use Ada.Strings.Unbounded;

   function To_File_Name (Name : in String) return String;

   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("Are.Generator.Go");

   --  ------------------------------
   --  Generate the Go code for the resources that have been collected.
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
   --  Generate the package body.
   --  ------------------------------
   procedure Generate_Source (Generator   : in out Generator_Type;
                              Resource    : in Are.Resource_Type;
                              Context     : in out Are.Context_Type'Class) is

      use Util.Strings.Transforms;

      procedure Generate_Keyword_Table (Into     : in out Ada.Text_IO.File_Type;
                                        Names    : in Util.Strings.Vectors.Vector);
      procedure Write_Content (Into : in out Ada.Text_IO.File_Type;
                               Content : in Are.Stream_Element_Access);

      Name        : constant String := To_String (Resource.Name);
      Basename    : constant String := To_File_Name (Name);
      Filename    : constant String := Basename & ".go";
      Path        : constant String := Context.Get_Output_Path (Basename);
      Def_Func    : constant String := "Get_content";
      Type_Name   : constant String := Resource.Get_Type_Name (Context, "content");
      Func_Name   : constant String := Resource.Get_Function_Name (Context, Def_Func);
      Count       : constant Natural := Natural (Resource.Files.Length);
      File        : Ada.Text_IO.File_Type;

      function List_Names return String is
         (if Context.List_Content then "Names" else "names");

      --  ------------------------------
      --  Generate the keyword table.
      --  ------------------------------
      procedure Generate_Keyword_Table (Into     : in out Ada.Text_IO.File_Type;
                                        Names    : in Util.Strings.Vectors.Vector) is
         Index : Integer := 0;
      begin
         New_Line (Into);
         Put (Into, "var ");
         Put (Into, List_Names);
         Put_Line (Into, "= []string {");
         for Name of Names loop
            Put (Into, "  """);
            Put (Into, Name);
            Put_Line (Into, """,");
            Index := Index + 1;
         end loop;
         New_Line (Into);
         Put_Line (Into, "}");
         New_Line (Into);
      end Generate_Keyword_Table;

      procedure Write_Content (Into : in out Ada.Text_IO.File_Type;
                               Content : in Are.Stream_Element_Access) is
         use type Ada.Streams.Stream_Element;

         Conversion : constant String (1 .. 16) := "0123456789abcdef";

         Col : Natural := 0;
         Ch  : Character;
      begin
         for C of Content.all loop
            if Col > 50 then
               Put_Line (Into, """ +");
               Put (Into, """");
               Col := 0;
            end if;
            Ch := Character'Val (C);
            case Ch is
               when 'a' .. 'z' | 'A' .. 'Z' | ' ' | '0' .. '9' |
                  '!' | '#' | '$' | '%' | '&' | '(' | ')' | ''' |
                  '*' | '+' | ',' |
                  '-' | '.' | '/' | ':' | ';' | '<' | '=' | '>' |
                  '?' | '@' | '[' | ']' | '^' | '_' | '{' | '|' |
                  '}' | '~' =>
                  Put (Into, Ch);

               when ASCII.LF =>
                  Put (Into, "\n");

               when others =>
                  Put (Into, "\x");
                  Put (Into, Conversion (1 + Natural (C / 16)));
                  Put (Into, Conversion (1 + Natural (C mod 16)));
            end case;
            Col := Col + 1;
         end loop;
      end Write_Content;

   begin
      Log.Info ("Writing resource {0} in {1}", Name, Path);

      if not Ada.Directories.Exists (Path) then
         Ada.Directories.Create_Path (Path);
      end if;

      Ada.Text_IO.Create (File => File,
                          Mode => Ada.Text_IO.Out_File,
                          Name => Ada.Directories.Compose (Path, Filename));
      Put (File, "// ");
      Put_Line (File, Get_Title);
      Put (File, "package ");
      Put_Line (File, Name);
      New_Line (File);

      if Context.Name_Index then
         Put_Line (File, "import (");
         Put_Line (File, "  ""strings""");
         if Count > 1 then
            Put_Line (File, "  ""sort""");
         end if;
         Put_Line (File, ")");
      end if;

      New_Line (File);
      Put (File, "type ");
      Put (File, Capitalize (Type_Name));
      Put_Line (File, " struct {");
      Put (File, "    ");
      Put (File, Capitalize (Resource.Get_Member_Content_Name (Context, "content")));
      Put_Line (File, "  []byte");
      Put (File, "    ");
      Put (File, Capitalize (Resource.Get_Member_Length_Name (Context, "size")));
      Put_Line (File, "  int64");
      Put (File, "    ");
      Put (File, Capitalize (Resource.Get_Member_Modtime_Name (Context, "modtime")));
      Put_Line (File, "  int64");
      Put (File, "    ");
      Put (File, Capitalize (Resource.Get_Member_Format_Name (Context, "format")));
      Put_Line (File, "   int");
      Put_Line (File, "}");
      New_Line (File);

      --  Generate_Resource_Contents (Resource, File, Context.Declare_Var);

      if Context.Name_Index then
         Generate_Keyword_Table (File, Generator.Names);
      end if;

      if Count >= 1 then

         Log.Debug ("Writing struct {0} contents[] with {1} entries",
                    Type_Name, Util.Strings.Image (Count));

         New_Line (File);
         Put (File, "var contents = []");
         Put (File, Capitalize (Type_Name));
         Put_Line (File, " {");

         for Content in Resource.Files.Iterate loop
            Put (File, " { []byte(""");
            declare
               use Ada.Calendar.Conversions;

               Data : constant File_Info := File_Maps.Element (Content);
            begin
               Write_Content (File, Data.Content);
               Put_Line (File, """),");
               Put (File, "   ");
               Put (File, Ada.Directories.File_Size'Image (Data.Length));
               Put (File, ", ");
               Put (File, Interfaces.C.long'Image (To_Unix_Time (Data.Modtime)));
            end;
            Put_Line (File, ", 0,");
            Put_Line (File, " }, ");
         end loop;
         Put_Line (File, "}");
         New_Line (File);
      end if;

      if Context.Name_Index then
         Log.Debug ("Writing {0} implementation", Func_Name);

         Put_Line (File, "// Returns the data stream with the given name or null.");
         Put (File, "func ");
         Put (File, Func_Name);
         Put (File, "(name string) (*Content) {");
         New_Line (File);
         if Count > 1 then
            Put (File, "    i := sort.Search(");
            Put (File, Util.Strings.Image (Count));
            Put_Line (File, ", func(i int) bool {");
            Put (File, "        return strings.Compare(");
            Put (File, List_Names);
            Put_Line (File, "[i], name) >= 0");
            Put_Line (File, "    })");
            Put (File, "    if i < ");
            Put (File, Util.Strings.Image (Count));
            Put (File, " && strings.Compare(");
            Put (File, List_Names);
            Put_Line (File, "[i], name) == 0 {");
            Put (File, "        return ");
            Put_Line (File, "&contents[i]");
            Put_Line (File, "    }");
            Put_Line (File, "    return nil");
         else
            Put (File, "  r := strings.Compare(");
            Put (File, List_Names);
            Put (File, "[0], name)");
            Put_Line (File, "  if r == 0 {");
            Put_Line (File, "    return &contents[0]");
            Put_Line (File, "  }");
            Put_Line (File, "  return nil");
         end if;
         Put_Line (File, "}");
         New_Line (File);
      end if;

      Close (File);
   end Generate_Source;

end Are.Generator.Go;
