-----------------------------------------------------------------------
--  are-testsuite -- Testsuite for are
--  Copyright (C) 2021, 2023 Stephane Carrez
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
with Ada.Characters.Handling;

with Util.Files;
with Util.Strings.Vectors;

with Are.Generator.Tests;
with Are.Tests;
package body Are.Testsuite is

   Tests : aliased Util.Tests.Test_Suite;

   function Suite return Util.Tests.Access_Test_Suite is
   begin
      Are.Generator.Tests.Add_Tests (Tests'Access);

      Are.Tests.Add_Tests (Tests'Access);
      return Tests'Access;
   end Suite;

   --  ------------------------------
   --  Check that two generated files are almost equal.  While doing the comparison,
   --  we ignore some generated timestamps in the form '1622183646'.
   --  ------------------------------
   procedure Assert_Equal_Files (T       : in Test'Class;
                                 Expect  : in String;
                                 Test    : in String;
                                 Message : in String := "Test failed";
                                 Source  : String := GNAT.Source_Info.File;
                                 Line    : Natural := GNAT.Source_Info.Line) is
      use Util.Files;
      use type Util.Strings.Vectors.Vector;
      use Ada.Characters.Handling;
      function Is_Almost_Equal (Line1, Line2 : in String) return Boolean;

      function Is_Almost_Equal (Line1, Line2 : in String) return Boolean is
         Pos1        : Natural := Line1'First;
         Pos2        : Natural := Line2'First;
         Digit_Count : Natural := 0;
         Digit_Error : Boolean := False;
      begin
         loop
            if Pos1 > Line1'Last then
               return Pos2 > Line2'Last;
            end if;
            exit when Pos2 > Line2'Last;
            if Line1 (Pos1) = Line2 (Pos2) then
               if Is_Digit (Line1 (Pos1)) then
                  Digit_Count := Digit_Count + 1;
               else
                  --  Accept a difference only on numbers with 10 digits (ie, timestamp).
                  exit when Digit_Error and then Digit_Count /= 10;
                  Digit_Count := 0;
                  Digit_Error := False;
               end if;
               Pos1 := Pos1 + 1;
               Pos2 := Pos2 + 1;
            else
               exit when not Is_Digit (Line1 (Pos1));
               exit when not Is_Digit (Line2 (Pos2));
               Digit_Count := Digit_Count + 1;
               Digit_Error := True;
               Pos1 := Pos1 + 1;
               Pos2 := Pos2 + 1;
            end if;
         end loop;
         return False;
      end Is_Almost_Equal;

      Expect_File : Util.Strings.Vectors.Vector;
      Test_File   : Util.Strings.Vectors.Vector;
      Same        : Boolean;
   begin
      if not Ada.Directories.Exists (Expect) then
         T.Assert (Condition => False,
                   Message => "Expect file '" & Expect & "' does not exist",
                   Source  => Source, Line => Line);
      end if;
      Read_File (Path => Expect,
                 Into => Expect_File);
      Read_File (Path => Test,
                 Into => Test_File);

      --  Check that both files have the same number of lines.
      Util.Tests.Assert_Equals (T       => T,
                                Expect  => Natural (Expect_File.Length),
                                Value   => Natural (Test_File.Length),
                                Message => Message & ": Invalid number of lines",
                                Source  => Source,
                                Line    => Line);

      Same := Expect_File = Test_File;
      if Same then
         return;
      end if;

      for Pos in 1 .. Natural (Expect_File.Length) loop
         declare
            Expect    : constant String := Expect_File.Element (Pos);
            Test_Line : constant String := Test_File.Element (Pos);
         begin
            if not Is_Almost_Equal (Expect, Test_Line) then
               Util.Tests.Fail (T       => T,
                                Message => Message & ": Content is different at line "
                                & Util.Strings.Image (Pos),
                                Source  => Source,
                                Line    => Line);
               return;
            end if;
         end;
      end loop;

   end Assert_Equal_Files;

end Are.Testsuite;
