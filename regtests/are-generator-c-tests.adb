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

   Expect_Dir  : constant String := "regtests/expect/c/";

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
      Caller.Add_Test (Suite, "Test Are.Generate_Lines",
                       Test_Generate_Lines'Access);
   end Add_Tests;

   procedure Test_Generate_C1 (T : in out Test) is
      Dir    : constant String := Util.Tests.Get_Test_Path ("");
      Web    : constant String := "regtests/files/test-c-1/web";
      Result : Ada.Strings.Unbounded.Unbounded_String;
   begin
      --  Generate the resources.ad[bs] files
      T.Execute (Tool & " --lang=c -o " & Dir & " --name-access "
                 & "--resource=Resources1 --fileset '**/*' "
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
      Util.Tests.Assert_Matches (T, "PASS: body {    background: #eee;  }p"
                                 & " {    color: #2a2a2a;  }", Result,
                                 "Invalid generation");
   end Test_Generate_C1;

   procedure Test_Generate_C2 (T : in out Test) is
      Dir    : constant String := Util.Tests.Get_Test_Path ("");
      Web    : constant String := "regtests/files/test-c-2/web";
      Result : Ada.Strings.Unbounded.Unbounded_String;
   begin
      --  Generate the resources.ad[bs] files
      T.Execute (Tool & " --lang=c -o " & Dir
                 & " --name-access --var-prefix Id_ --resource=Resources2 --fileset '**/*' "
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
      Util.Tests.Assert_Matches (T, "PASS: body {    background: #eee;  }p"
                                 & " {    color: #2a2a2a;  }", Result,
                                 "Invalid generation");
   end Test_Generate_C2;

   procedure Test_Generate_Lines (T : in out Test) is
      Dir        : constant String := Util.Tests.Get_Test_Path ("");
      Rule       : constant String := "regtests/files/package-lines.xml";
      Files      : constant String := "regtests/files";
      Lines_H    : constant String := Ada.Directories.Compose (Dir, "lines.h");
      Lines_C    : constant String := Ada.Directories.Compose (Dir, "lines.c");
      Result     : Ada.Strings.Unbounded.Unbounded_String;
   begin
      --  Generate the lines.ads files
      T.Execute (Tool & " -o " & Dir & " --lang=c --content-only --var-prefix Id_ --rule="
                 & Rule & " " & Files & "/lines-empty", Result);

      T.Assert (Ada.Directories.Exists (Lines_H),
                "Resource file 'lines.h' not generated");

      Util.Tests.Assert_Equal_Files
        (T       => T,
         Expect  => Util.Tests.Get_Path (Expect_Dir & "lines-empty.h"),
         Test    => Lines_H,
         Message => "Invalid lines-empty.h generation");

      T.Assert (Ada.Directories.Exists (Lines_C),
                "Resource file 'lines.c' not generated");

      Util.Tests.Assert_Equal_Files
        (T       => T,
         Expect  => Util.Tests.Get_Path (Expect_Dir & "lines-empty.c"),
         Test    => Lines_C,
         Message => "Invalid lines-empty.c generation");

      Ada.Directories.Delete_File (Lines_H);
      Ada.Directories.Delete_File (Lines_C);
      T.Execute (Tool & " -o " & Dir & " --lang=c --content-only --var-prefix Id_ --rule="
                 & Rule & " " & Files & "/lines-single", Result);

      T.Assert (Ada.Directories.Exists (Lines_H),
                "Resource file 'lines.h' not generated");

      Util.Tests.Assert_Equal_Files
        (T       => T,
         Expect  => Util.Tests.Get_Path (Expect_Dir & "lines-single.h"),
         Test    => Lines_H,
         Message => "Invalid lines-single.h generation");

      T.Assert (Ada.Directories.Exists (Lines_C),
                "Resource file 'lines.c' not generated");

      Util.Tests.Assert_Equal_Files
        (T       => T,
         Expect  => Util.Tests.Get_Path (Expect_Dir & "lines-single.c"),
         Test    => Lines_C,
         Message => "Invalid lines-single.c generation");

      Ada.Directories.Delete_File (Lines_H);
      Ada.Directories.Delete_File (Lines_C);
      T.Execute (Tool & " -o " & Dir & " --lang=c --content-only --var-prefix Id_ --rule="
                 & Rule & " " & Files & "/lines-multiple", Result);

      T.Assert (Ada.Directories.Exists (Lines_H),
                "Resource file 'lines.h' not generated");

      Util.Tests.Assert_Equal_Files
        (T       => T,
         Expect  => Util.Tests.Get_Path (Expect_Dir & "lines-multiple.h"),
         Test    => Lines_H,
         Message => "Invalid lines-multiple.h generation");

      T.Assert (Ada.Directories.Exists (Lines_C),
                "Resource file 'lines.c' not generated");

      Util.Tests.Assert_Equal_Files
        (T       => T,
         Expect  => Util.Tests.Get_Path (Expect_Dir & "lines-multiple.c"),
         Test    => Lines_C,
         Message => "Invalid lines-multiple.c generation");

   end Test_Generate_Lines;

end Are.Generator.C.Tests;
