-----------------------------------------------------------------------
--  are-generator-ada2012-tests -- Tests for Ada generator
--  Copyright (C) 2021, 2023 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Tests;
with Are.Testsuite;
package Are.Generator.Ada2012.Tests is

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite);

   type Test is new Are.Testsuite.Test with null record;

   procedure Test_Generate_Ada1 (T : in out Test);

   procedure Test_Generate_Ada2 (T : in out Test);

   procedure Test_Generate_Ada3 (T : in out Test);

   procedure Test_Generate_Ada4 (T : in out Test);

   procedure Test_Generate_Ada5 (T : in out Test);

   procedure Test_Generate_Ada6 (T : in out Test);

   procedure Test_Generate_Ada7 (T : in out Test);

   procedure Test_Generate_Ada8 (T : in out Test);

   procedure Test_Generate_Merge (T : in out Test);

   procedure Test_Generate_Concat (T : in out Test);

   procedure Test_Generate_Bundle (T : in out Test);

   procedure Test_Generate_Lines (T : in out Test);

   procedure Test_Generate_Lines_Keep_Empty (T : in out Test);

   procedure Test_Ada_Names (T : in out Test);

end Are.Generator.Ada2012.Tests;
