-----------------------------------------------------------------------
--  are-testsuite -- Testsuite for are
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
with Ada.Strings.Unbounded;
with GNAT.Source_Info;

with Util.Tests;
with Util.Systems.Os;
package Are.Testsuite is

   pragma Warnings (Off, "*condition is always*");

   function Is_Windows return Boolean is
     (Util.Systems.Os.Directory_Separator = '\');

   pragma Warnings (On, "*condition is always*");

   EXE   : constant String
     := (if Is_Windows then ".exe" else "");

   function Suite return Util.Tests.Access_Test_Suite;

   type Test is new Util.Tests.Test with null record;

   --  Execute the command and get the output in a string.
   procedure Execute (T       : in out Test;
                      Command : in String;
                      Result  : out Ada.Strings.Unbounded.Unbounded_String;
                      Status  : in Natural := 0);

   procedure Assert_Equal_Files (T       : in Test'Class;
                                 Expect  : in String;
                                 Test    : in String;
                                 Message : in String := "Test failed";
                                 Source  : String := GNAT.Source_Info.File;
                                 Line    : Natural := GNAT.Source_Info.Line);
end Are.Testsuite;
