-----------------------------------------------------------------------
--  are_harness -- Unit tests
--  Copyright (C) 2021 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Tests;
with Are.Testsuite;

procedure Are_Harness is

   procedure Harness is new Util.Tests.Harness (Are.Testsuite.Suite);

begin
   Harness ("are-tests.xml");
end Are_Harness;
