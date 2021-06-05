-----------------------------------------------------------------------
--  are-generator-specific_options -- Hack for GNAT.Command_Line portability
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

separate (Are.Generator)

--  ------------------------------
--  Portability hack to support old GNAT.Command_Line after gcc 8 (GNAT 2018 and above).
--  ------------------------------
procedure Specific_Options (Config  : in out GC.Command_Line_Configuration;
                            Context : in out Are.Context_Type'Class) is
   pragma Unreferenced (Context);
begin
   GC.Define_Switch (Config => Config,
                     Callback => Add_Option'Access,
                     Long_Switch => "--resource=",
                     Help   => -("Define the name of the resource collection"));
   GC.Define_Switch (Config => Config,
                     Callback => Add_Option'Access,
                     Long_Switch => "--fileset=",
                     Help   => -("Define the pattern to match files for the resource collection"));
end Specific_Options;
