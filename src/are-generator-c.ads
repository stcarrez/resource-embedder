-----------------------------------------------------------------------
--  are-generator-c -- Generator for C/C++
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
private with Util.Strings.Vectors;
private package Are.Generator.C is

   type Generator_Type is new Are.Generator.Generator_Type with private;

   --  Generate the C/C++ code for the resources that have been collected.
   overriding
   procedure Generate (Generator : in out Generator_Type;
                       Resources : in Resource_List;
                       Context   : in out Are.Context_Type'Class);

   --  Setup the command line configuration to accept specific generation options.
   overriding
   procedure Setup (Generator : in out Generator_Type;
                    Config    : in out GC.Command_Line_Configuration);

private

   type Generator_Type is new Are.Generator.Generator_Type with record
      Names               : Util.Strings.Vectors.Vector;
   end record;

   --  Generate the header declaration file.
   procedure Generate_Header (Generator : in out Generator_Type;
                              Resource  : in Are.Resource_Type;
                              Context   : in out Are.Context_Type'Class);

   --  Generate the source file.
   procedure Generate_Source (Generator : in out Generator_Type;
                              Resource  : in Are.Resource_Type;
                              Context   : in out Are.Context_Type'Class);

end Are.Generator.C;
