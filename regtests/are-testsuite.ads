-----------------------------------------------------------------------
--  are-testsuite -- Testsuite for are
--  Copyright (C) 2021, 2023 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
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

   procedure Assert_Equal_Files (T       : in Test'Class;
                                 Expect  : in String;
                                 Test    : in String;
                                 Message : in String := "Test failed";
                                 Source  : String := GNAT.Source_Info.File;
                                 Line    : Natural := GNAT.Source_Info.Line);
end Are.Testsuite;
