-----------------------------------------------------------------------
--  are-generator-c-tests -- Tests for C generator
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
package body Are.Generator.C.Tests is

   function Tool return String;

   package Caller is new Util.Test_Caller (Test, "Are.Generator.C");

   function Tool return String is
   begin
      return "bin/are" & Are.Testsuite.EXE;
   end Tool;

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test Are.Generate_C1",
                       Test_Generate_C1'Access);
      Caller.Add_Test (Suite, "Test Are.Generate_C2",
                       Test_Generate_C2'Access);
   end Add_Tests;

   procedure Test_Generate_C1 (T : in out Test) is
      Dir    : constant String := Util.Tests.Get_Test_Path ("");
      Web    : constant String := "regtests/files/test-c-1/web";
      Result : Ada.Strings.Unbounded.Unbounded_String;
   begin
      --  Generate the resources.ad[bs] files
      T.Execute (Tool & " --lang=c -o " & Dir & " --name-access --resource=Resources1 --fileset '**/*' "
                 & Web, Result);

      T.Assert (Ada.Directories.Exists (Ada.Directories.Compose (Dir, "resources1.h")),
                "Resource file 'resources1.h' not generated");
      T.Assert (Ada.Directories.Exists (Ada.Directories.Compose (Dir, "resources1.c")),
                "Resource file 'resources1.c' not generated");

      --  Build the test program.
      T.Execute ("make -C regtests/files/test-c-1", Result);
      T.Assert (Ada.Directories.Exists ("bin/test-c-1" & Are.Testsuite.EXE),
                "Binary file 'bin/test-c-1' not created");

      T.Execute ("bin/test-c-1" & Are.Testsuite.EXE, Result);
      Util.Tests.Assert_Matches (T, "PASS: body {    background: #eee;  }p {    color: #2a2a2a;  }", Result,
                                 "Invalid generation");
   end Test_Generate_C1;

   procedure Test_Generate_C2 (T : in out Test) is
      Dir    : constant String := Util.Tests.Get_Test_Path ("");
      Web    : constant String := "regtests/files/test-c-2/web";
      Result : Ada.Strings.Unbounded.Unbounded_String;
   begin
      --  Generate the resources.ad[bs] files
      T.Execute (Tool & " --lang=c -o " & Dir
                 & " --name-access --var-access --resource=Resources2 --fileset '**/*' "
                 & Web, Result);

      T.Assert (Ada.Directories.Exists (Ada.Directories.Compose (Dir, "resources2.h")),
                "Resource file 'resources2.h' not generated");
      T.Assert (Ada.Directories.Exists (Ada.Directories.Compose (Dir, "resources2.c")),
                "Resource file 'resources2.c' not generated");

      --  Build the test program.
      T.Execute ("make -C regtests/files/test-c-2", Result);
      T.Assert (Ada.Directories.Exists ("bin/test-c-2" & Are.Testsuite.EXE),
                "Binary file 'bin/test-c-2' not created");

      T.Execute ("bin/test-c-2" & Are.Testsuite.EXE, Result);
      Util.Tests.Assert_Matches (T, "PASS: body {    background: #eee;  }p {    color: #2a2a2a;  }", Result,
                                 "Invalid generation");
   end Test_Generate_C2;

end Are.Generator.C.Tests;
