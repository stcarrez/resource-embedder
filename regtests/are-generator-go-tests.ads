-----------------------------------------------------------------------
--  are-generator-go-tests -- Tests for Go generator
--  Copyright (C) 2021 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Tests;
with Are.Testsuite;
package Are.Generator.Go.Tests is

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite);

   type Test is new Are.Testsuite.Test with null record;

   procedure Test_Generate_Go1 (T : in out Test);

   procedure Test_Generate_Go2 (T : in out Test);

   procedure Test_Generate_Go3 (T : in out Test);

end Are.Generator.Go.Tests;
