-----------------------------------------------------------------------
--  are-generator -- Advanced Resource Embedder Generator
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
with GNAT.Command_Line;
package Are.Generator is

   package GC renames GNAT.Command_Line;

   --  Main entry point to parse command line arguments.
   procedure Main;

   procedure Usage;

private

   type Generator_Type is limited interface;

   --  Generate the code for the resources that have been collected.
   procedure Generate (Generator : in out Generator_Type;
                       Resources : in Resource_List;
                       Context   : in out Are.Context_Type'Class) is abstract;

   --  Setup the command line configuration to accept specific generation options.
   procedure Setup (Generator : in out Generator_Type;
                    Config    : in out GC.Command_Line_Configuration) is abstract;

   --  Return a string that identifies the program.
   function Get_Title return String;

   procedure Add_Option (Switch, Value : in String);

   --  Portability hack to support old GNAT.Command_Line before gcc 7.3
   procedure Specific_Options (Config  : in out GC.Command_Line_Configuration;
                               Context : in out Are.Context_Type'Class);

end Are.Generator;
