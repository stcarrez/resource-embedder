-----------------------------------------------------------------------
--  are-generator-ada2012-tests -- Tests for Ada generator
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

   procedure Test_Ada_Names (T : in out Test);

end Are.Generator.Ada2012.Tests;
