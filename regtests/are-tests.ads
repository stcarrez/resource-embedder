-----------------------------------------------------------------------
--  are-tests -- Various tests for the are tool (based on examples)
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

with Util.Tests;
with Are.Testsuite;
package Are.Tests is

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite);

   type Test is new Are.Testsuite.Test with null record;

   procedure Test_Example_C_Config (T : in out Test);

   procedure Test_Example_Ada_Config (T : in out Test);

   procedure Test_Example_Go_Config (T : in out Test);

   procedure Test_Example_C_Help (T : in out Test);

   procedure Test_Example_Ada_Help (T : in out Test);

   procedure Test_Example_Go_Help (T : in out Test);

end Are.Tests;
