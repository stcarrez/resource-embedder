-----------------------------------------------------------------------
--  are-generator-tests -- Tests for generator
--  Copyright (C) 2021 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Tests;
with Are.Testsuite;
package Are.Generator.Tests is

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite);

   type Test is new Are.Testsuite.Test with null record;

   procedure Test_Wrong_Usage (T : in out Test);

   procedure Test_Wrong_Directory (T : in out Test);

   procedure Test_Missing_Rule (T : in out Test);

   procedure Test_Exec_Error_1 (T : in out Test);

   procedure Test_Exec_Error_2 (T : in out Test);

   procedure Test_Exec_Error_3 (T : in out Test);

   procedure Test_Exec_Error_4 (T : in out Test);

   procedure Test_Exec_Error_5 (T : in out Test);

   procedure Test_Exec_Error_6 (T : in out Test);

   procedure Test_Exec_Error_7 (T : in out Test);

   procedure Test_Exec_Error_8 (T : in out Test);

   procedure Test_Exec_Error_9 (T : in out Test);

   procedure Test_Merge_Error_1 (T : in out Test);

   procedure Test_Verbose (T : in out Test);

end Are.Generator.Tests;
