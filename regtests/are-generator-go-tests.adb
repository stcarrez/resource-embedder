-----------------------------------------------------------------------
--  are-generator-go-tests -- Tests for Go generator
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

with Util.Files;
with Util.Test_Caller;
package body Are.Generator.Go.Tests is

   Expect_Dir  : constant String := "regtests/expect/go/";

   function Tool return String;

   package Caller is new Util.Test_Caller (Test, "Are.Generator.Go");

   function Tool return String is
   begin
      return "bin/are" & Are.Testsuite.EXE;
   end Tool;

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test Are.Generate_Go1",
                       Test_Generate_Go1'Access);
      Caller.Add_Test (Suite, "Test Are.Generate_Go2",
                       Test_Generate_Go2'Access);
      Caller.Add_Test (Suite, "Test Are.Generate_Go3",
                       Test_Generate_Go3'Access);
   end Add_Tests;

   procedure Test_Generate_Go1 (T : in out Test) is
      Dir    : constant String := Util.Tests.Get_Test_Path ("");
      Target : constant String := Util.Files.Compose (Dir, "resources1/resources1.go");
      Web    : constant String := "regtests/files/test-c-1/web";
      Result : Ada.Strings.Unbounded.Unbounded_String;
   begin
      --  Generate the resources1.go file
      T.Execute (Tool & " --lang=go -o " & Dir & " --name-access --resource=Resources1 --fileset '**/*' "
                 & Web, Result);

      T.Assert (Ada.Directories.Exists (Target),
                "Resource file 'resources1/resources1.go' not generated");

      Are.Testsuite.Assert_Equal_Files (T       => T,
                                        Expect  => Util.Tests.Get_Path (Expect_Dir & "resources1.go"),
                                        Test    => Target,
                                        Message => "Invalid Go generation");
   end Test_Generate_Go1;

   procedure Test_Generate_Go2 (T : in out Test) is
      Dir    : constant String := Util.Tests.Get_Test_Path ("");
      Target : constant String := Util.Files.Compose (Dir, "resources2/resources2.go");
      Web    : constant String := "regtests/files/test-ada-2";
      Rule   : constant String := "regtests/files/test-ada-2/package.xml";
      Result : Ada.Strings.Unbounded.Unbounded_String;
   begin
      --  Generate the resources1.go file
      T.Execute (Tool & " --lang=go -o " & Dir & " --name-access --rule=" & Rule
                 & " " & Web, Result);

      T.Assert (Ada.Directories.Exists (Target),
                "Resource file 'resources2/resources2.go' not generated");

      Are.Testsuite.Assert_Equal_Files (T       => T,
                                        Expect  => Util.Tests.Get_Path (Expect_Dir & "resources2.go"),
                                        Test    => Target,
                                        Message => "Invalid Go generation");
   end Test_Generate_Go2;

   procedure Test_Generate_Go3 (T : in out Test) is
      Dir    : constant String := Util.Tests.Get_Test_Path ("");
      Target : constant String := Util.Files.Compose (Dir, "resource4/resource4.go");
      Web    : constant String := "regtests/files/test-ada-4";
      Rule   : constant String := "regtests/files/test-ada-4/package.xml";
      Result : Ada.Strings.Unbounded.Unbounded_String;
   begin
      --  Generate the resources1.go file
      T.Execute (Tool & " --lang=go -o " & Dir & " --name-access --rule=" & Rule
                 & " " & Web, Result);

      T.Assert (Ada.Directories.Exists (Target),
                "Resource file 'resource4/resource4.go' not generated");

      Are.Testsuite.Assert_Equal_Files (T       => T,
                                        Expect  => Util.Tests.Get_Path (Expect_Dir & "resource4.go"),
                                        Test    => Target,
                                        Message => "Invalid Go generation");
   end Test_Generate_Go3;

end Are.Generator.Go.Tests;
