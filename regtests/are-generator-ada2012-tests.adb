-----------------------------------------------------------------------
--  are-generator-ada2012-tests -- Tests for Ada generator
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
package body Are.Generator.Ada2012.Tests is

   Expect_Dir  : constant String := "regtests/expect/ada/";

   function Tool return String;

   package Caller is new Util.Test_Caller (Test, "Are.Generator.Ada");

   function Tool return String is
   begin
      return "bin/are";
   end Tool;

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test Are.Generate_Ada1",
                       Test_Generate_Ada1'Access);
      Caller.Add_Test (Suite, "Test Are.Generate_Ada2",
                       Test_Generate_Ada2'Access);
      Caller.Add_Test (Suite, "Test Are.Generate_Ada3",
                       Test_Generate_Ada3'Access);
      Caller.Add_Test (Suite, "Test Are.Generate_Ada4",
                       Test_Generate_Ada4'Access);
      Caller.Add_Test (Suite, "Test Are.Generate_Ada5",
                       Test_Generate_Ada5'Access);
      Caller.Add_Test (Suite, "Test Are.Generate_Ada6",
                       Test_Generate_Ada6'Access);
      Caller.Add_Test (Suite, "Test Are.Generate_Merge",
                       Test_Generate_Merge'Access);
      Caller.Add_Test (Suite, "Test Are.Generate_Concat",
                       Test_Generate_Concat'Access);
      Caller.Add_Test (Suite, "Test Are.Generate_Bundle",
                       Test_Generate_Bundle'Access);
   end Add_Tests;

   procedure Test_Generate_Ada1 (T : in out Test) is
      Dir    : constant String := Util.Tests.Get_Test_Path ("");
      Web    : constant String := "regtests/files/test-ada-1/web";
      Result : Ada.Strings.Unbounded.Unbounded_String;
   begin
      --  Generate the resources.ad[bs] files
      T.Execute (Tool & " -o " & Dir
                 & " --name-access --content-only --resource=Resources1 --fileset '**/*' "
                 & Web, Result);

      T.Assert (Ada.Directories.Exists (Ada.Directories.Compose (Dir, "resources1.ads")),
                "Resource file 'resources1.ads' not generated");
      T.Assert (Ada.Directories.Exists (Ada.Directories.Compose (Dir, "resources1.adb")),
                "Resource file 'resources1.adb' not generated");

      --  Build the test program.
      T.Execute ("gprbuild -Pregtests/files/test-ada-1/test1.gpr", Result);
      T.Assert (Ada.Directories.Exists ("bin/test1"),
                "Binary file 'bin/test1' not created");

      T.Execute ("bin/test1", Result);
      Util.Tests.Assert_Matches (T, "PASS: body {    background: #eee;  }p {    color: #2a2a2a;  }",
                                 Result,
                                 "Invalid generation");
   end Test_Generate_Ada1;

   procedure Test_Generate_Ada2 (T : in out Test) is
      Dir    : constant String := Util.Tests.Get_Test_Path ("");
      Web    : constant String := "regtests/files/test-ada-2";
      Rule   : constant String := "regtests/files/test-ada-2/package.xml";
      Result : Ada.Strings.Unbounded.Unbounded_String;
   begin
      --  Generate the resources.ad[bs] files
      T.Execute (Tool & " -o " & Dir & " --name-access --content-only --rule="
                 & Rule & " " & Web, Result);

      T.Assert (Ada.Directories.Exists (Ada.Directories.Compose (Dir, "resources2.ads")),
                "Resource file 'resources2.ads' not generated");
      T.Assert (Ada.Directories.Exists (Ada.Directories.Compose (Dir, "resources2.adb")),
                "Resource file 'resources2.adb' not generated");

      --  Build the test program.
      T.Execute ("gprbuild -Pregtests/files/test-ada-2/test2.gpr", Result);
      T.Assert (Ada.Directories.Exists ("bin/test2"),
                "Binary file 'bin/test2' not created");

      T.Execute ("bin/test2", Result);
      Util.Tests.Assert_Matches (T, "PASS", Result,
                                 "Invalid generation");
   end Test_Generate_Ada2;

   procedure Test_Generate_Ada3 (T : in out Test) is
      Dir    : constant String := Util.Tests.Get_Test_Path ("");
      Web    : constant String := "regtests/files/test-ada-3";
      Rule   : constant String := "regtests/files/test-ada-3/package.xml";
      Result : Ada.Strings.Unbounded.Unbounded_String;
   begin
      --  Generate the resources.ad[bs] files
      T.Execute (Tool & " -o " & Dir
                 & " --no-type-declaration --content-only --name-access --rule="
                 & Rule & " " & Web, Result);

      T.Assert (Ada.Directories.Exists (Ada.Directories.Compose (Dir, "resource-web.ads")),
                "Resource file 'resource-web.ads' not generated");
      T.Assert (Ada.Directories.Exists (Ada.Directories.Compose (Dir, "resource-web.adb")),
                "Resource file 'resource-web.adb' not generated");
      T.Assert (Ada.Directories.Exists (Ada.Directories.Compose (Dir, "resource-config.ads")),
                "Resource file 'resource-config.ads' not generated");
      T.Assert (Ada.Directories.Exists (Ada.Directories.Compose (Dir, "resource-config.adb")),
                "Resource file 'resource-config.adb' not generated");

      --  Build the test program.
      T.Execute ("gprbuild -Pregtests/files/test-ada-3/test3.gpr", Result);
      T.Assert (Ada.Directories.Exists ("bin/test3"),
                "Binary file 'bin/test3' not created");

      T.Execute ("bin/test3", Result);
      Util.Tests.Assert_Matches (T, "PASS: <config></config>", Result,
                                 "Invalid generation");
   end Test_Generate_Ada3;

   procedure Test_Generate_Ada4 (T : in out Test) is
      Dir    : constant String := Util.Tests.Get_Test_Path ("");
      Web    : constant String := "regtests/files/test-ada-4";
      Rule   : constant String := "regtests/files/test-ada-4/package.xml";
      Result : Ada.Strings.Unbounded.Unbounded_String;
   begin
      --  Generate the resources.ad[bs] files
      T.Execute (Tool & " -o " & Dir & " --name-access --content-only --rule="
                 & Rule & " " & Web, Result);

      T.Assert (Ada.Directories.Exists (Ada.Directories.Compose (Dir, "resource4.ads")),
                "Resource file 'resource4.ads' not generated");
      T.Assert (Ada.Directories.Exists (Ada.Directories.Compose (Dir, "resource4.adb")),
                "Resource file 'resource4.adb' not generated");

      --  Build the test program.
      T.Execute ("gprbuild -Pregtests/files/test-ada-4/test4.gpr", Result);
      T.Assert (Ada.Directories.Exists ("bin/test4"),
                "Binary file 'bin/test4' not created");

      T.Execute ("bin/test4", Result);
      Util.Tests.Assert_Matches (T, "PASS: <config>test4</config>", Result,
                                 "Invalid generation");
   end Test_Generate_Ada4;

   procedure Test_Generate_Ada5 (T : in out Test) is
      Dir    : constant String := Util.Tests.Get_Test_Path ("");
      Web    : constant String := "regtests/files/test-ada-5/web";
      Result : Ada.Strings.Unbounded.Unbounded_String;
   begin
      --  Generate the resources.ad[bs] files
      T.Execute (Tool & " -o " & Dir
                 & " --name-access --content-only --var-access --resource=Resources5 --fileset '**/*' "
                 & Web, Result);

      T.Assert (Ada.Directories.Exists (Ada.Directories.Compose (Dir, "resources5.ads")),
                "Resource file 'resources5.ads' not generated");
      T.Assert (Ada.Directories.Exists (Ada.Directories.Compose (Dir, "resources5.adb")),
                "Resource file 'resources5.adb' not generated");

      --  Build the test program.
      T.Execute ("gprbuild -Pregtests/files/test-ada-5/test5.gpr", Result);
      T.Assert (Ada.Directories.Exists ("bin/test5"),
                "Binary file 'bin/test5' not created");

      T.Execute ("bin/test5", Result);
      Util.Tests.Assert_Matches (T, "PASS: body {    background: #eee;  }p {    color: #2a2a2a;  }",
                                 Result,
                                 "Invalid generation");
   end Test_Generate_Ada5;

   procedure Test_Generate_Ada6 (T : in out Test) is
      Dir    : constant String := Util.Tests.Get_Test_Path ("");
      Web    : constant String := "regtests/files/test-ada-6/web";
      Result : Ada.Strings.Unbounded.Unbounded_String;
   begin
      --  Generate the resources.ad[bs] files
      T.Execute (Tool & " -o " & Dir
                 & " --content-only --var-access --resource=Resources6 --fileset '**/*' "
                 & Web, Result);

      T.Assert (Ada.Directories.Exists (Ada.Directories.Compose (Dir, "resources6.ads")),
                "Resource file 'resources6.ads' not generated");
      T.Assert (not Ada.Directories.Exists (Ada.Directories.Compose (Dir, "resources6.adb")),
                "Resource file 'resources6.adb' was generated (expecting no body)");

      --  Build the test program.
      T.Execute ("gprbuild -Pregtests/files/test-ada-6/test6.gpr", Result);
      T.Assert (Ada.Directories.Exists ("bin/test6"),
                "Binary file 'bin/test6' not created");

      T.Execute ("bin/test6", Result);
      Util.Tests.Assert_Matches (T, "PASS: body {    background: #eee;  }p {    color: #2a2a2a;  }",
                                 Result,
                                 "Invalid generation");
   end Test_Generate_Ada6;

   procedure Test_Generate_Merge (T : in out Test) is
      Dir     : constant String := Util.Tests.Get_Test_Path ("");
      Web     : constant String := "examples/c-web";
      Rule    : constant String := "examples/c-web/package.xml";
      Web_Ads : constant String := Ada.Directories.Compose (Dir, "web.ads");
      Web_Adb : constant String := Ada.Directories.Compose (Dir, "web.adb");
      Result  : Ada.Strings.Unbounded.Unbounded_String;
   begin
      --  Generate the web.ad[bs] files
      T.Execute (Tool & " -o " & Dir & " --name-access --rule=" & Rule & " " & Web, Result);

      T.Assert (Ada.Directories.Exists (Web_Ads),
                "Resource file 'web.ads' not generated");
      T.Assert (Ada.Directories.Exists (Web_Adb),
                "Resource file 'web.adb' not generated");

      Util.Tests.Assert_Equal_Files (T       => T,
                                     Expect  => Util.Tests.Get_Path (Expect_Dir & "web.ads"),
                                     Test    => Web_Ads,
                                     Message => "Invalid Ada spec generation");
      Util.Tests.Assert_Equal_Files (T       => T,
                                     Expect  => Util.Tests.Get_Path (Expect_Dir & "web.adb"),
                                     Test    => Web_Adb,
                                     Message => "Invalid Ada body generation");
   end Test_Generate_Merge;

   procedure Test_Generate_Concat (T : in out Test) is
      Dir        : constant String := Util.Tests.Get_Test_Path ("");
      Rule       : constant String := "regtests/files/package-concat.xml";
      Concat_Ads : constant String := Ada.Directories.Compose (Dir, "concat.ads");
      Concat_Adb : constant String := Ada.Directories.Compose (Dir, "concat.adb");
      Result     : Ada.Strings.Unbounded.Unbounded_String;
   begin
      --  Generate the concat.ad[bs] files
      T.Execute (Tool & " -o " & Dir & " --name-access --rule=" & Rule & " "
                 & " regtests/files/test-ada-2 regtests/files/test-ada-3"
                 & " regtests/files/test-ada-4 regtests/files/test-c-1", Result);

      T.Assert (Ada.Directories.Exists (Concat_Ads),
                "Resource file 'concat.ads' not generated");
      T.Assert (Ada.Directories.Exists (Concat_Adb),
                "Resource file 'concat.adb' not generated");

      Util.Tests.Assert_Equal_Files (T       => T,
                                     Expect  => Util.Tests.Get_Path (Expect_Dir & "concat.ads"),
                                     Test    => Concat_Ads,
                                     Message => "Invalid Ada spec generation");
      Util.Tests.Assert_Equal_Files (T       => T,
                                     Expect  => Util.Tests.Get_Path (Expect_Dir & "concat.adb"),
                                     Test    => Concat_Adb,
                                     Message => "Invalid Ada body generation");
   end Test_Generate_Concat;

   procedure Test_Generate_Bundle (T : in out Test) is
      Dir        : constant String := Util.Tests.Get_Test_Path ("");
      Web        : constant String := "examples/ada-bundles";
      Rule       : constant String := "examples/ada-bundles/package.xml";
      Bundle_Ads : constant String := Ada.Directories.Compose (Dir, "bundle.ads");
      Bundle_Adb : constant String := Ada.Directories.Compose (Dir, "bundle.adb");
      Result     : Ada.Strings.Unbounded.Unbounded_String;
   begin
      --  Generate the bundle.ad[bs] files
      T.Execute (Tool & " -o " & Dir & " --name-access --rule=" & Rule & " "
                 & Web, Result);

      T.Assert (Ada.Directories.Exists (Bundle_Ads),
                "Resource file 'bundle.ads' not generated");
      T.Assert (Ada.Directories.Exists (Bundle_Adb),
                "Resource file 'bundle.adb' not generated");

      Util.Tests.Assert_Equal_Files (T       => T,
                                     Expect  => Util.Tests.Get_Path (Expect_Dir & "bundle.ads"),
                                     Test    => Bundle_Ads,
                                     Message => "Invalid Ada spec generation");
      Util.Tests.Assert_Equal_Files (T       => T,
                                     Expect  => Util.Tests.Get_Path (Expect_Dir & "bundle.adb"),
                                     Test    => Bundle_Adb,
                                     Message => "Invalid Ada body generation");
   end Test_Generate_Bundle;

end Are.Generator.Ada2012.Tests;
