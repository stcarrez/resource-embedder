-----------------------------------------------------------------------
--  are-generator-specific_options -- Hack for GNAT.Command_Line portability
--  Copyright (C) 2021 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
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
                     Argument => "NAME",
                     Help   => -("Define the name of the resource collection"));
   GC.Define_Switch (Config => Config,
                     Callback => Add_Option'Access,
                     Long_Switch => "--fileset=",
                     Argument => "PATTERN",
                     Help   => -("Define the pattern to match files for the resource collection"));
end Specific_Options;
