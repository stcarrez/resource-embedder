-----------------------------------------------------------------------
--  are-tests -- Various tests for the are tool (based on examples)
--  Copyright (C) 2021, 2023 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Tests;
with Are.Testsuite;
package Are.Tests is

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite);

   type Test is new Are.Testsuite.Test with null record;

   procedure Test_Example_C_Config (T : in out Test);

   procedure Test_Example_Ada_Config (T : in out Test);

   procedure Test_Example_Go_Config (T : in out Test);

   procedure Test_Example_C_Help (T : in out Test);

   procedure Test_Example_C_Lines (T : in out Test);

   procedure Test_Example_Ada_Help (T : in out Test);

   procedure Test_Example_Ada_Lines (T : in out Test);

   procedure Test_Example_Ada_Mapping (T : in out Test);

   procedure Test_Example_Go_Help (T : in out Test);

   --  Test converting some content into a list of lines.
   procedure Test_Convert_Lines_1 (T : in out Test);
   procedure Test_Convert_Lines_2 (T : in out Test);

end Are.Tests;
