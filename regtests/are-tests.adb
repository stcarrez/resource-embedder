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

with Ada.Directories;

with Util.Test_Caller;
package body Are.Tests is

   function Tool return String;

   package Caller is new Util.Test_Caller (Test, "Are.Examples");

   function Tool return String is
   begin
      return "bin/are";
   end Tool;

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
      if Ada.Directories.Exists ("/usr/bin/go") then
         Caller.Add_Test (Suite, "Test examples go-config",
                          Test_Example_Go_Config'Access);
         Caller.Add_Test (Suite, "Test examples go-help",
                          Test_Example_Go_Help'Access);
      end if;
   end Add_Tests;

   procedure Test_Example_C_Config (T : in out Test) is
      Result : Ada.Strings.Unbounded.Unbounded_String;
   begin
      T.Execute ("make -C examples/c-config clean", Result, Status => 0);
      T.Execute ("make -C examples/c-config ARE=../../bin/are", Result, Status => 0);
      T.Execute ("examples/c-config/show-config", Result, Status => 0);
      Util.Tests.Assert_Matches (T, ".*Example of embedded configuration.*",
                                 Result,
                                 "Invalid C show-config");
   end Test_Example_C_Config;

   procedure Test_Example_Ada_Config (T : in out Test) is
      Result : Ada.Strings.Unbounded.Unbounded_String;
   begin
      T.Execute ("make -C examples/ada-config clean", Result, Status => 0);
      T.Execute ("make -C examples/ada-config ARE=../../bin/are", Result, Status => 0);
      T.Execute ("examples/ada-config/show_config", Result, Status => 0);
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
      T.Execute ("examples/c-help/show-help", Result, Status => 0);
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

      T.Execute ("examples/c-help/show-help extract.txt", Result, Status => 0);
      Util.Tests.Assert_Matches (T, ".*extract files.*",
                                 Result,
                                 "Invalid C show-help: extract");
   end Test_Example_C_Help;

   procedure Test_Example_Ada_Help (T : in out Test) is
      Result : Ada.Strings.Unbounded.Unbounded_String;
   begin
      T.Execute ("make -C examples/ada-help clean", Result, Status => 0);
      T.Execute ("make -C examples/ada-help ARE=../../bin/are", Result, Status => 0);
      T.Execute ("examples/ada-help/show_help", Result, Status => 0);
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

      T.Execute ("examples/ada-help/show_help extract.txt", Result, Status => 0);
      Util.Tests.Assert_Matches (T, ".*extract files.*",
                                 Result,
                                 "Invalid Ada show-help: extract");
   end Test_Example_Ada_Help;

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

      T.Execute ("examples/go-help/show-help extract.txt", Result, Status => 0);
      Util.Tests.Assert_Matches (T, ".*extract files.*",
                                 Result,
                                 "Invalid Go show-help: extract");
   end Test_Example_Go_Help;

end Are.Tests;
