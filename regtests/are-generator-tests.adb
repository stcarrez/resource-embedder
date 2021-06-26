-----------------------------------------------------------------------
--  are-generator-tests -- Tests for generator
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
with Are.Generator.C.Tests;
with Are.Generator.Ada2012.Tests;
with Are.Generator.Go.Tests;
package body Are.Generator.Tests is

   function Tool return String;

   package Caller is new Util.Test_Caller (Test, "Are.Generator");

   function Tool return String is
   begin
      return "bin/are";
   end Tool;

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test are (usage)",
                       Test_Wrong_Usage'Access);
      Caller.Add_Test (Suite, "Test are (wrong directory)",
                       Test_Wrong_Directory'Access);
      Caller.Add_Test (Suite, "Test are (exec wrong command)",
                       Test_Exec_Error_1'Access);
      Caller.Add_Test (Suite, "Test are (missing rule file)",
                       Test_Missing_Rule'Access);
      Caller.Add_Test (Suite, "Test are (wrong include)",
                       Test_Exec_Error_2'Access);
      Caller.Add_Test (Suite, "Test are (wrong exclude)",
                       Test_Exec_Error_3'Access);
      Caller.Add_Test (Suite, "Test are (wrong fileset)",
                       Test_Exec_Error_4'Access);
      Caller.Add_Test (Suite, "Test are (wrong install)",
                       Test_Exec_Error_5'Access);
      Caller.Add_Test (Suite, "Test are (XML format error)",
                       Test_Exec_Error_6'Access);
      Caller.Add_Test (Suite, "Test are (wrong resource format)",
                       Test_Exec_Error_7'Access);
      Caller.Add_Test (Suite, "Test are (missing <line-separator>)",
                       Test_Exec_Error_8'Access);
      Caller.Add_Test (Suite, "Test are (invalid <line-filter>)",
                       Test_Exec_Error_9'Access);
      Caller.Add_Test (Suite, "Test are (verbose mode with error)",
                       Test_Verbose'Access);
      Are.Generator.Ada2012.Tests.Add_Tests (Suite);
      Are.Generator.C.Tests.Add_Tests (Suite);
      Are.Generator.Go.Tests.Add_Tests (Suite);
   end Add_Tests;

   procedure Test_Wrong_Usage (T : in out Test) is
      Result : Ada.Strings.Unbounded.Unbounded_String;
   begin
      T.Execute (Tool, Result, Status => 1);
      T.Execute (Tool & " --zorro", Result, Status => 1);
      Util.Tests.Assert_Matches (T, "are: unrecognized option '--zorro'", Result,
                                 "Invalid error message with --zorro");

      T.Execute (Tool & " --lang=plop .", Result, Status => 1);
      Util.Tests.Assert_Matches (T, "are: error: language plop not recognized", Result,
                                 "Invalid error message with --lang=plop");
   end Test_Wrong_Usage;

   procedure Test_Wrong_Directory (T : in out Test) is
      Result : Ada.Strings.Unbounded.Unbounded_String;
   begin
      T.Execute (Tool & " regtests/toto", Result, Status => 1);
      Util.Tests.Assert_Matches (T, "are: error: path regtests/toto does not exist", Result,
                                 "Invalid error message");
      T.Execute (Tool & " /dev/null", Result, Status => 1);
      Util.Tests.Assert_Matches (T, "are: error: path /dev/null is not a directory", Result,
                                 "Invalid error message");
   end Test_Wrong_Directory;

   procedure Test_Missing_Rule (T : in out Test) is
      Dir    : constant String := Util.Tests.Get_Test_Path ("");
      Web    : constant String := "regtests/files/test-ada-4";
      Result : Ada.Strings.Unbounded.Unbounded_String;
   begin
      T.Execute (Tool & " -o " & Dir
                 & " --rule=package-missing-rule.xml " & Web, Result, Status => 1);
      Util.Tests.Assert_Matches (T, "are: error: package file "
                                 & "package-missing-rule.xml does not exist",
                                 Result,
                                 "Invalid error message");
   end Test_Missing_Rule;

   procedure Test_Exec_Error_1 (T : in out Test) is
      Dir    : constant String := Util.Tests.Get_Test_Path ("");
      Web    : constant String := "regtests/files/test-ada-4";
      Rule   : constant String := "regtests/files/package-error-1.xml";
      Result : Ada.Strings.Unbounded.Unbounded_String;
   begin
      T.Execute (Tool & " -o " & Dir & " --rule=" & Rule & " " & Web, Result, Status => 1);
      Util.Tests.Assert_Matches (T, ".*missing-command", Result,
                                 "Invalid error message");

      T.Assert (not Ada.Directories.Exists (Ada.Directories.Compose (Dir, "error1.ads")),
                "Unexpected file error1.ads was created");
   end Test_Exec_Error_1;

   procedure Test_Exec_Error_2 (T : in out Test) is
      Dir    : constant String := Util.Tests.Get_Test_Path ("");
      Web    : constant String := "regtests/files/test-ada-4";
      Rule   : constant String := "regtests/files/package-error-2.xml";
      Result : Ada.Strings.Unbounded.Unbounded_String;
   begin
      T.Execute (Tool & " -o " & Dir & " --rule=" & Rule & " " & Web, Result, Status => 1);
      Util.Tests.Assert_Matches (T, "are: error: .*package-error-2.xml: empty include", Result,
                                 "Invalid error message");

      T.Assert (not Ada.Directories.Exists (Ada.Directories.Compose (Dir, "error1.ads")),
                "Unexpected file error1.ads was created");
   end Test_Exec_Error_2;

   procedure Test_Exec_Error_3 (T : in out Test) is
      Dir    : constant String := Util.Tests.Get_Test_Path ("");
      Web    : constant String := "regtests/files/test-ada-4";
      Rule   : constant String := "regtests/files/package-error-3.xml";
      Result : Ada.Strings.Unbounded.Unbounded_String;
   begin
      T.Execute (Tool & " -o " & Dir & " --rule=" & Rule & " " & Web, Result, Status => 1);
      Util.Tests.Assert_Matches (T, "are: error: .*package-error-3.xml: empty exclude", Result,
                                 "Invalid error message");

      T.Assert (not Ada.Directories.Exists (Ada.Directories.Compose (Dir, "error1.ads")),
                "Unexpected file error1.ads was created");
   end Test_Exec_Error_3;

   procedure Test_Exec_Error_4 (T : in out Test) is
      Dir    : constant String := Util.Tests.Get_Test_Path ("");
      Web    : constant String := "regtests/files/test-ada-4";
      Rule   : constant String := "regtests/files/package-error-4.xml";
      Result : Ada.Strings.Unbounded.Unbounded_String;
   begin
      T.Execute (Tool & " -o " & Dir & " --rule=" & Rule & " " & Web, Result, Status => 1);
      Util.Tests.Assert_Matches (T, "are: error: .*package-error-4.xml: empty fileset",
                                 Result,
                                 "Invalid error message");

      T.Assert (not Ada.Directories.Exists (Ada.Directories.Compose (Dir, "error1.ads")),
                "Unexpected file error1.ads was created");
   end Test_Exec_Error_4;

   procedure Test_Exec_Error_5 (T : in out Test) is
      Dir    : constant String := Util.Tests.Get_Test_Path ("");
      Web    : constant String := "regtests/files/test-ada-4";
      Rule   : constant String := "regtests/files/package-error-5.xml";
      Result : Ada.Strings.Unbounded.Unbounded_String;
   begin
      T.Execute (Tool & " -o " & Dir & " --rule=" & Rule & " " & Web, Result, Status => 1);
      Util.Tests.Assert_Matches (T, "are: error: .*package-error-5.xml: rule 'some-plugin'",
                                 Result,
                                 "Invalid error message");

      T.Assert (not Ada.Directories.Exists (Ada.Directories.Compose (Dir, "error1.ads")),
                "Unexpected file error1.ads was created");
   end Test_Exec_Error_5;

   procedure Test_Exec_Error_6 (T : in out Test) is
      Dir    : constant String := Util.Tests.Get_Test_Path ("");
      Web    : constant String := "regtests/files/test-ada-4";
      Rule   : constant String := "regtests/files/package-error-6.xml";
      Result : Ada.Strings.Unbounded.Unbounded_String;
   begin
      T.Execute (Tool & " -o " & Dir & " --rule=" & Rule & " " & Web, Result, Status => 1);
      Util.Tests.Assert_Matches (T, "are: error: package-error-6.xml:2:0: "
                                 & "Node <package> is not closed",
                                 Result,
                                 "Invalid error message");
   end Test_Exec_Error_6;

   procedure Test_Exec_Error_7 (T : in out Test) is
      Dir    : constant String := Util.Tests.Get_Test_Path ("");
      Web    : constant String := "regtests/files/test-ada-4";
      Rule   : constant String := "regtests/files/package-error-7.xml";
      Result : Ada.Strings.Unbounded.Unbounded_String;
   begin
      T.Execute (Tool & " -o " & Dir & " --rule=" & Rule & " " & Web, Result, Status => 1);
      Util.Tests.Assert_Matches (T, "are: error: .*: invalid resource format 'bad-format'",
                                 Result,
                                 "Invalid error message");
   end Test_Exec_Error_7;

   procedure Test_Exec_Error_8 (T : in out Test) is
      Dir    : constant String := Util.Tests.Get_Test_Path ("");
      Web    : constant String := "regtests/files/test-ada-4";
      Rule   : constant String := "regtests/files/package-error-8.xml";
      Result : Ada.Strings.Unbounded.Unbounded_String;
   begin
      T.Execute (Tool & " -o " & Dir & " --rule=" & Rule & " " & Web, Result, Status => 1);
      Util.Tests.Assert_Matches (T, "are: error: .*: missing 'line-separator'"
                                 & " for resource 'Error1'",
                                 Result,
                                 "Invalid error message");
   end Test_Exec_Error_8;

   procedure Test_Exec_Error_9 (T : in out Test) is
      Dir    : constant String := Util.Tests.Get_Test_Path ("");
      Web    : constant String := "regtests/files/test-ada-4";
      Rule   : constant String := "regtests/files/package-error-9.xml";
      Result : Ada.Strings.Unbounded.Unbounded_String;
   begin
      T.Execute (Tool & " -o " & Dir & " --rule=" & Rule & " " & Web, Result, Status => 1);
      Util.Tests.Assert_Matches (T, "are: error: .*: invalid pattern '",
                                 Result,
                                 "Invalid error message");
   end Test_Exec_Error_9;

   procedure Test_Verbose (T : in out Test) is
      Dir    : constant String := Util.Tests.Get_Test_Path ("");
      Web    : constant String := "regtests/files/test-ada-4";
      Rule   : constant String := "regtests/files/package-error-1.xml";
      Result : Ada.Strings.Unbounded.Unbounded_String;
   begin
      T.Execute (Tool & " -v -o " & Dir & " --rule=" & Rule & " " & Web, Result, Status => 1);
      Util.Tests.Assert_Matches (T, ".*missing-command", Result,
                                 "Invalid error message");

      T.Assert (not Ada.Directories.Exists (Ada.Directories.Compose (Dir, "error1.ads")),
                "Unexpected file error1.ads was created");

      T.Execute (Tool & " -vv -o " & Dir & " --rule=" & Rule & " " & Web, Result, Status => 1);
      Util.Tests.Assert_Matches (T, ".*DEBUG.*Process rule", Result,
                                 "Invalid debug message");
      Util.Tests.Assert_Matches (T, ".*INFO  - Util.Processes", Result,
                                 "Missing debug message");

      T.Assert (not Ada.Directories.Exists (Ada.Directories.Compose (Dir, "error1.ads")),
                "Unexpected file error1.ads was created");

      T.Execute (Tool & " -V", Result, Status => 0);
      Util.Tests.Assert_Matches (T, "Advanced Resource Embedder 1.*",
                                 Result,
                                 "Invalid version");
   end Test_Verbose;

end Are.Generator.Tests;
