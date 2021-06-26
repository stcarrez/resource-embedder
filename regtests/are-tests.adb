-----------------------------------------------------------------------
--  are-tests -- Various tests for the are tool (based on examples)
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
with Ada.Directories;
with Ada.Strings.Maps;

with Util.Test_Caller;
package body Are.Tests is

   package Caller is new Util.Test_Caller (Test, "Are.Examples");

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test examples c-config",
                       Test_Example_C_Config'Access);
      Caller.Add_Test (Suite, "Test examples c-help",
                       Test_Example_C_Help'Access);
      Caller.Add_Test (Suite, "Test examples ada-config",
                       Test_Example_Ada_Config'Access);
      Caller.Add_Test (Suite, "Test examples ada-help",
                       Test_Example_Ada_Help'Access);
      Caller.Add_Test (Suite, "Test examples ada-lines",
                       Test_Example_Ada_Lines'Access);
      if Ada.Directories.Exists ("/usr/bin/go") then
         Caller.Add_Test (Suite, "Test examples go-config",
                          Test_Example_Go_Config'Access);
         Caller.Add_Test (Suite, "Test examples go-help",
                          Test_Example_Go_Help'Access);
      end if;
      Caller.Add_Test (Suite, "Test Are.Convert_To_Lines_1",
                       Test_Convert_Lines_1'Access);
      Caller.Add_Test (Suite, "Test Are.Convert_To_Lines_2",
                       Test_Convert_Lines_2'Access);
   end Add_Tests;

   procedure Test_Example_C_Config (T : in out Test) is
      Result : Ada.Strings.Unbounded.Unbounded_String;
   begin
      T.Execute ("make -C examples/c-config clean", Result, Status => 0);
      T.Execute ("make -C examples/c-config ARE=../../bin/are", Result, Status => 0);
      T.Execute ("examples/c-config/show-config" & Are.Testsuite.EXE, Result, Status => 0);
      Util.Tests.Assert_Matches (T, ".*Example of embedded configuration.*",
                                 Result,
                                 "Invalid C show-config");
   end Test_Example_C_Config;

   procedure Test_Example_Ada_Config (T : in out Test) is
      Result : Ada.Strings.Unbounded.Unbounded_String;
   begin
      T.Execute ("make -C examples/ada-config clean", Result, Status => 0);
      T.Execute ("make -C examples/ada-config ARE=../../bin/are", Result, Status => 0);
      T.Execute ("examples/ada-config/show_config" & Are.Testsuite.EXE, Result, Status => 0);
      Util.Tests.Assert_Matches (T, ".*Example of embedded configuration.*",
                                 Result,
                                 "Invalid Ada show_config");
   end Test_Example_Ada_Config;

   procedure Test_Example_Go_Config (T : in out Test) is
      Result : Ada.Strings.Unbounded.Unbounded_String;
   begin
      T.Execute ("make -C examples/go-config clean", Result, Status => 0);
      T.Execute ("make -C examples/go-config ARE=../../bin/are", Result, Status => 0);
      T.Execute ("examples/go-config/show-config", Result, Status => 0);
      Util.Tests.Assert_Matches (T, ".*Example of embedded configuration.*",
                                 Result,
                                 "Invalid Go show-config");
   end Test_Example_Go_Config;

   procedure Test_Example_C_Help (T : in out Test) is
      Result : Ada.Strings.Unbounded.Unbounded_String;
   begin
      T.Execute ("make -C examples/c-help clean", Result, Status => 0);
      T.Execute ("make -C examples/c-help ARE=../../bin/are", Result, Status => 0);
      T.Execute ("examples/c-help/show-help" & Are.Testsuite.EXE, Result, Status => 0);
      Util.Tests.Assert_Matches (T, ".*sh.*",
                                 Result,
                                 "Invalid C show-help: sh missing");
      Util.Tests.Assert_Matches (T, ".*extract.*",
                                 Result,
                                 "Invalid C show-help: extract missing");

      T.Execute ("examples/c-help/show-help sh", Result, Status => 0);
      Util.Tests.Assert_Matches (T, ".*shell.*",
                                 Result,
                                 "Invalid C show-help: sh");

      T.Execute ("examples/c-help/show-help extract", Result, Status => 0);
      Util.Tests.Assert_Matches (T, ".*extract files.*",
                                 Result,
                                 "Invalid C show-help: extract");
   end Test_Example_C_Help;

   procedure Test_Example_Ada_Help (T : in out Test) is
      Result : Ada.Strings.Unbounded.Unbounded_String;
   begin
      T.Execute ("make -C examples/ada-help clean", Result, Status => 0);
      T.Execute ("make -C examples/ada-help ARE=../../bin/are", Result, Status => 0);
      T.Execute ("examples/ada-help/show_help" & Are.Testsuite.EXE, Result, Status => 0);
      Util.Tests.Assert_Matches (T, ".*sh.*",
                                 Result,
                                 "Invalid Ada show-help: sh missing");
      Util.Tests.Assert_Matches (T, ".*extract.*",
                                 Result,
                                 "Invalid Ada show-help: extract missing");

      T.Execute ("examples/ada-help/show_help sh", Result, Status => 0);
      Util.Tests.Assert_Matches (T, ".*shell.*",
                                 Result,
                                 "Invalid Ada show-help: sh");

      T.Execute ("examples/ada-help/show_help extract", Result, Status => 0);
      Util.Tests.Assert_Matches (T, ".*extract files.*",
                                 Result,
                                 "Invalid Ada show-help: extract");
   end Test_Example_Ada_Help;

   procedure Test_Example_Ada_Lines (T : in out Test) is
      Result : Ada.Strings.Unbounded.Unbounded_String;
   begin
      T.Execute ("make -C examples/ada-lines clean", Result, Status => 0);
      T.Execute ("make -C examples/ada-lines ARE=../../bin/are", Result, Status => 0);
      T.Execute ("examples/ada-lines/show_script" & Are.Testsuite.EXE, Result, Status => 0);
      Util.Tests.Assert_Matches (T, "Create SQLite 7 lines.*",
                                 Result,
                                 "Invalid Ada show_script: SQL statement");
      Util.Tests.Assert_Matches (T, "Drop SQLite 4 lines.*",
                                 Result,
                                 "Invalid Ada show_script: SQL statement");

   end Test_Example_Ada_Lines;

   procedure Test_Example_Go_Help (T : in out Test) is
      Result : Ada.Strings.Unbounded.Unbounded_String;
   begin
      T.Execute ("make -C examples/go-help clean", Result, Status => 0);
      T.Execute ("make -C examples/go-help ARE=../../bin/are", Result, Status => 0);
      T.Execute ("examples/go-help/show-help", Result, Status => 0);
      Util.Tests.Assert_Matches (T, ".*sh.*",
                                 Result,
                                 "Invalid Go show-help: sh missing");
      Util.Tests.Assert_Matches (T, ".*extract.*",
                                 Result,
                                 "Invalid Go show-help: extract missing");

      T.Execute ("examples/go-help/show-help sh", Result, Status => 0);
      Util.Tests.Assert_Matches (T, ".*shell.*",
                                 Result,
                                 "Invalid Go show-help: sh");

      T.Execute ("examples/go-help/show-help extract", Result, Status => 0);
      Util.Tests.Assert_Matches (T, ".*extract files.*",
                                 Result,
                                 "Invalid Go show-help: extract");
   end Test_Example_Go_Help;

   procedure Test_Convert_Lines_1 (T : in out Test) is
      use Ada.Strings.Maps;

      Resource : Resource_Type;
      Lines    : Util.Strings.Vectors.Vector;
      File     : File_Info;
   begin
      Resource.Add_File ("lines", "regtests/files/lines-1.txt");
      Resource.Separators := To_Set (ASCII.CR) or To_Set (ASCII.LF);
      File := Resource.Files.First_Element;
      Resource.Convert_To_Lines (File, Lines);
      Util.Tests.Assert_Equals (T, 9, Natural (Lines.Length),
                                "Invalid number of lines");
      for I in 1 .. 9 loop
         Util.Tests.Assert_Equals (T, "line" & Util.Strings.Image (I),
                                   Lines.Element (I), "Invalid line");
      end loop;
   end Test_Convert_Lines_1;

   procedure Test_Convert_Lines_2 (T : in out Test) is
      use Ada.Strings.Maps;

      Resource : Resource_Type;
      Lines    : Util.Strings.Vectors.Vector;
      File     : File_Info;
   begin
      Resource.Add_File ("lines", "regtests/files/lines-2.txt");
      Resource.Separators := To_Set (";");

      --  Remove newlines and contiguous spaces.
      Are.Add_Line_Filter (Resource, "[\r\n]", "");
      Are.Add_Line_Filter (Resource, "[ \t][ \t]+", " ");

      --  Remove C comments.
      Are.Add_Line_Filter (Resource, "/\*[^/]*\*/", "");

      --  Remove contiguous spaces (can happen after C comment removal).
      Are.Add_Line_Filter (Resource, "[ \t][ \t]+", " ");
      File := Resource.Files.First_Element;
      Resource.Convert_To_Lines (File, Lines);
      Util.Tests.Assert_Equals (T, 5, Natural (Lines.Length),
                                "Invalid number of lines");

      for I in 1 .. 5 loop
         Ada.Text_IO.Put_Line (Lines.Element (I));
      end loop;

      Util.Tests.Assert_Equals (T, "pragma synchronous=OFF",
                                Lines.Element (1), "Invalid line 1");
      Util.Tests.Assert_Equals (T, "CREATE TABLE IF NOT EXISTS entity_type ( `id`"
                                & " INTEGER PRIMARY KEY AUTOINCREMENT, `name`"
                                & " VARCHAR(127) UNIQUE )",
                                Lines.Element (2), "Invalid line 2");
      Util.Tests.Assert_Equals (T, "CREATE TABLE IF NOT EXISTS sequence ( `name`"
                                & " VARCHAR(127) UNIQUE NOT NULL, `version` INTEGER NOT NULL,"
                                & " `value` BIGINT NOT NULL, `block_size` BIGINT NOT NULL,"
                                & " PRIMARY KEY (`name`))",
                                Lines.Element (3), "Invalid line 3");
      Util.Tests.Assert_Equals (T, "INSERT OR IGNORE INTO entity_type (name) VALUES"
                                & " (""entity_type"")",
                                Lines.Element (4), "Invalid line 4");
      Util.Tests.Assert_Equals (T, "INSERT OR IGNORE INTO entity_type (name) VALUES"
                                & " (""sequence"")",
                                Lines.Element (5), "Invalid line 5");
   end Test_Convert_Lines_2;

end Are.Tests;
