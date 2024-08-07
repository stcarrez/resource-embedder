-----------------------------------------------------------------------
--  are-generator-ada2012-tests -- Tests for Ada generator
--  Copyright (C) 2021, 2023 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Ada.Directories;

with Util.Test_Caller;
package body Are.Generator.Ada2012.Tests is

   Expect_Dir  : constant String := "regtests/expect/ada/";

   function Tool return String;

   package Caller is new Util.Test_Caller (Test, "Are.Generator.Ada");

   function Tool return String is
   begin
      return "bin/are" & Are.Testsuite.EXE;
   end Tool;

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test Are.To_Ada_Name",
                       Test_Ada_Names'Access);
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
      Caller.Add_Test (Suite, "Test Are.Generate_Ada7",
                       Test_Generate_Ada7'Access);
      Caller.Add_Test (Suite, "Test Are.Generate_Ada8",
                       Test_Generate_Ada8'Access);
      Caller.Add_Test (Suite, "Test Are.Generate_Merge",
                       Test_Generate_Merge'Access);
      Caller.Add_Test (Suite, "Test Are.Generate_Concat",
                       Test_Generate_Concat'Access);
      Caller.Add_Test (Suite, "Test Are.Generate_Bundle",
                       Test_Generate_Bundle'Access);
      Caller.Add_Test (Suite, "Test Are.Generate_Lines",
                       Test_Generate_Lines'Access);
      Caller.Add_Test (Suite, "Test Are.Generate_Lines_Keep_Empty",
                       Test_Generate_Lines_Keep_Empty'Access);
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
      T.Assert (Ada.Directories.Exists ("bin/test1" & Are.Testsuite.EXE),
                "Binary file 'bin/test1' not created");

      T.Execute ("bin/test1" & Are.Testsuite.EXE, Result);
      Util.Tests.Assert_Matches (T, "PASS: body {    background: #eee;  }"
                                 & "p {    color: #2a2a2a;  }",
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
      T.Assert (Ada.Directories.Exists ("bin/test2" & Are.Testsuite.EXE),
                "Binary file 'bin/test2' not created");

      T.Execute ("bin/test2" & Are.Testsuite.EXE, Result);
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
      T.Assert (Ada.Directories.Exists ("bin/test3" & Are.Testsuite.EXE),
                "Binary file 'bin/test3' not created");

      T.Execute ("bin/test3" & Are.Testsuite.EXE, Result);
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
      T.Assert (Ada.Directories.Exists ("bin/test4" & Are.Testsuite.EXE),
                "Binary file 'bin/test4' not created");

      T.Execute ("bin/test4" & Are.Testsuite.EXE, Result);
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
                 & " --name-access --content-only --var-prefix Id_"
                 & " --resource=Resources5 --fileset '**/*' "
                 & Web, Result);

      T.Assert (Ada.Directories.Exists (Ada.Directories.Compose (Dir, "resources5.ads")),
                "Resource file 'resources5.ads' not generated");
      T.Assert (Ada.Directories.Exists (Ada.Directories.Compose (Dir, "resources5.adb")),
                "Resource file 'resources5.adb' not generated");

      --  Build the test program.
      T.Execute ("gprbuild -Pregtests/files/test-ada-5/test5.gpr", Result);
      T.Assert (Ada.Directories.Exists ("bin/test5" & Are.Testsuite.EXE),
                "Binary file 'bin/test5' not created");

      T.Execute ("bin/test5" & Are.Testsuite.EXE, Result);
      Util.Tests.Assert_Matches (T, "PASS: body {    background: #eee;  }p"
                                 & " {    color: #2a2a2a;  }",
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
                 & " --content-only --var-prefix Id_ --resource=Resources6 --fileset '**/*' "
                 & Web, Result);

      T.Assert (Ada.Directories.Exists (Ada.Directories.Compose (Dir, "resources6.ads")),
                "Resource file 'resources6.ads' not generated");
      T.Assert (not Ada.Directories.Exists (Ada.Directories.Compose (Dir, "resources6.adb")),
                "Resource file 'resources6.adb' was generated (expecting no body)");

      --  Build the test program.
      T.Execute ("gprbuild -Pregtests/files/test-ada-6/test6.gpr", Result);
      T.Assert (Ada.Directories.Exists ("bin/test6" & Are.Testsuite.EXE),
                "Binary file 'bin/test6' not created");

      T.Execute ("bin/test6" & Are.Testsuite.EXE, Result);
      Util.Tests.Assert_Matches (T, "PASS: body {    background: #eee;  }p "
                                 & "{    color: #2a2a2a;  }",
                                 Result,
                                 "Invalid generation");
   end Test_Generate_Ada6;

   procedure Test_Generate_Ada7 (T : in out Test) is
      Dir    : constant String := Util.Tests.Get_Test_Path ("");
      Web    : constant String := "regtests/files/test-ada-7";
      Rule   : constant String := "regtests/files/test-ada-7/package.xml";
      Result : Ada.Strings.Unbounded.Unbounded_String;
   begin
      --  Generate the resources.ad[bs] files
      T.Execute (Tool & " -o " & Dir & " --ignore-case --name-access --content-only --rule="
                 & Rule & " " & Web, Result);

      T.Assert (Ada.Directories.Exists (Ada.Directories.Compose (Dir, "resources7.ads")),
                "Resource file 'resources7.ads' not generated");
      T.Assert (Ada.Directories.Exists (Ada.Directories.Compose (Dir, "resources7.adb")),
                "Resource file 'resources7.adb' not generated");

      --  Build the test program.
      T.Execute ("gprbuild -Pregtests/files/test-ada-7/test7.gpr", Result);
      T.Assert (Ada.Directories.Exists ("bin/test7" & Are.Testsuite.EXE),
                "Binary file 'bin/test7' not created");

      T.Execute ("bin/test7" & Are.Testsuite.EXE, Result);
      Util.Tests.Assert_Matches (T, "PASS: <config>test7</config>", Result,
                                 "Invalid generation");
   end Test_Generate_Ada7;

   procedure Test_Generate_Ada8 (T : in out Test) is
      Dir    : constant String := Util.Tests.Get_Test_Path ("");
      Web    : constant String := "regtests/files/test-ada-8";
      Rule   : constant String := "regtests/files/test-ada-8/package.xml";
      Result : Ada.Strings.Unbounded.Unbounded_String;
   begin
      --  Generate the resources.ad[bs] files
      T.Execute (Tool & " -o " & Dir & " --ignore-case --name-access --content-only --rule="
                 & Rule & " " & Web, Result);

      T.Assert (Ada.Directories.Exists (Ada.Directories.Compose (Dir, "resources8.ads")),
                "Resource file 'resources8.ads' not generated");
      T.Assert (Ada.Directories.Exists (Ada.Directories.Compose (Dir, "resources8.adb")),
                "Resource file 'resources78.adb' not generated");

      --  Build the test program.
      T.Execute ("gprbuild -Pregtests/files/test-ada-8/test8.gpr", Result);
      T.Assert (Ada.Directories.Exists ("bin/test8" & Are.Testsuite.EXE),
                "Binary file 'bin/test8' not created");

      T.Execute ("bin/test8" & Are.Testsuite.EXE, Result);
      Util.Tests.Assert_Matches (T, "PASS: " & ASCII.HT & "<config>.*", Result,
                                 "Invalid generation");
      Util.Tests.Assert_Matches (T, ".*éèà@.*", Result,
                                 "Invalid generation");
   end Test_Generate_Ada8;

   procedure Test_Generate_Merge (T : in out Test) is
      Dir     : constant String := Util.Tests.Get_Test_Path ("");
      Web     : constant String := "examples/c-web";
      Rule    : constant String := "examples/c-web/package.xml";
      Web_Ads : constant String := Ada.Directories.Compose (Dir, "web.ads");
      Web_Adb : constant String := Ada.Directories.Compose (Dir, "web.adb");
      Result  : Ada.Strings.Unbounded.Unbounded_String;
   begin
      --  Generate the web.ad[bs] files
      T.Execute (Tool & " -o " & Dir & " --content-only --name-access --rule="
                 & Rule & " " & Web, Result);

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
      T.Execute (Tool & " -o " & Dir & " --content-only --name-access --rule=" & Rule & " "
                 & " regtests/files/test-ada-2 regtests/files/test-ada-3"
                 & " regtests/files/test-ada-4 regtests/files/test-c-1", Result);

      T.Assert (Ada.Directories.Exists (Concat_Ads),
                "Resource file 'concat.ads' not generated");
      T.Assert (Ada.Directories.Exists (Concat_Adb),
                "Resource file 'concat.adb' not generated");

      Util.Tests.Assert_Equal_Files
        (T       => T,
         Expect  => Util.Tests.Get_Path (Expect_Dir & "concat.ads"),
         Test    => Concat_Ads,
         Message => "Invalid Ada spec generation");
      Util.Tests.Assert_Equal_Files
        (T       => T,
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
      T.Execute (Tool & " -o " & Dir & " --content-only --name-access --rule=" & Rule & " "
                 & Web, Result);

      T.Assert (Ada.Directories.Exists (Bundle_Ads),
                "Resource file 'bundle.ads' not generated");
      T.Assert (Ada.Directories.Exists (Bundle_Adb),
                "Resource file 'bundle.adb' not generated");

      Util.Tests.Assert_Equal_Files
        (T       => T,
         Expect  => Util.Tests.Get_Path (Expect_Dir & "bundle.ads"),
         Test    => Bundle_Ads,
         Message => "Invalid Ada spec generation");
      Util.Tests.Assert_Equal_Files
        (T       => T,
         Expect  => Util.Tests.Get_Path (Expect_Dir & "bundle.adb"),
         Test    => Bundle_Adb,
         Message => "Invalid Ada body generation");
   end Test_Generate_Bundle;

   procedure Test_Generate_Lines (T : in out Test) is
      Dir        : constant String := Util.Tests.Get_Test_Path ("");
      Rule       : constant String := "regtests/files/package-lines.xml";
      Files      : constant String := "regtests/files";
      Lines_Ads  : constant String := Ada.Directories.Compose (Dir, "lines.ads");
      Result     : Ada.Strings.Unbounded.Unbounded_String;
   begin
      --  Generate the lines.ads files
      T.Execute (Tool & " -o " & Dir & " --content-only --var-prefix Id_ --rule="
                 & Rule & " " & Files & "/lines-empty", Result);

      T.Assert (Ada.Directories.Exists (Lines_Ads),
                "Resource file 'lines.ads' not generated");

      Util.Tests.Assert_Equal_Files
        (T       => T,
         Expect  => Util.Tests.Get_Path (Expect_Dir & "lines-empty.ads"),
         Test    => Lines_Ads,
         Message => "Invalid Ada spec generation");

      Ada.Directories.Delete_File (Lines_Ads);
      T.Execute (Tool & " -o " & Dir & " --content-only --var-prefix Id_ --rule="
                 & Rule & " " & Files & "/lines-single", Result);

      T.Assert (Ada.Directories.Exists (Lines_Ads),
                "Resource file 'lines.ads' not generated");

      Util.Tests.Assert_Equal_Files
        (T       => T,
         Expect  => Util.Tests.Get_Path (Expect_Dir & "lines-single.ads"),
         Test    => Lines_Ads,
         Message => "Invalid Ada spec generation");

      Ada.Directories.Delete_File (Lines_Ads);
      T.Execute (Tool & " -o " & Dir & " --content-only --var-prefix Id_ --rule="
                 & Rule & " " & Files & "/lines-multiple", Result);

      T.Assert (Ada.Directories.Exists (Lines_Ads),
                "Resource file 'lines.ads' not generated");

      Util.Tests.Assert_Equal_Files
        (T       => T,
         Expect  => Util.Tests.Get_Path (Expect_Dir & "lines-multiple.ads"),
         Test    => Lines_Ads,
         Message => "Invalid Ada spec generation");
   end Test_Generate_Lines;

   procedure Test_Generate_Lines_Keep_Empty (T : in out Test) is
      Dir        : constant String := Util.Tests.Get_Test_Path ("");
      Rule       : constant String := "regtests/files/package-lines-keep-empty.xml";
      Files      : constant String := "regtests/files";
      Lines_Ads  : constant String := Ada.Directories.Compose (Dir, "lines_with_empty.ads");
      Result     : Ada.Strings.Unbounded.Unbounded_String;
   begin
      --  Generate the lines-with-empty.ads files
      T.Execute (Tool & " -o " & Dir & " --content-only --var-prefix Id_ --rule="
                 & Rule & " " & Files & "/lines-multiple", Result);

      T.Assert (Ada.Directories.Exists (Lines_Ads),
                "Resource file 'lines_with_empty.ads' not generated");

      Util.Tests.Assert_Equal_Files
        (T       => T,
         Expect  => Util.Tests.Get_Path (Expect_Dir & "lines_with_empty.ads"),
         Test    => Lines_Ads,
         Message => "Invalid Ada spec generation");
   end Test_Generate_Lines_Keep_Empty;

   procedure Test_Ada_Names (T : in out Test) is
   begin
      Util.Tests.Assert_Equals (T, "Id_file_c",
                                To_Ada_Name ("Id_", "file.c"),
                                "Bad conversion");
      Util.Tests.Assert_Equals (T, "Id_file_name_h",
                                To_Ada_Name ("Id_", "file-name.h"),
                                "Bad conversion");
      Util.Tests.Assert_Equals (T, "Plop_File_Dat",
                                To_Ada_Name ("Plop_", "File.Dat"),
                                "Bad conversion");
      Util.Tests.Assert_Equals (T, "Id_File23_Dat",
                                To_Ada_Name ("Id_", "File 23 .Dat"),
                                "Bad conversion");
   end Test_Ada_Names;

end Are.Generator.Ada2012.Tests;
