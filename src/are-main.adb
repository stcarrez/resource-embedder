-----------------------------------------------------------------------
--  are-main -- Main tool program
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
with Ada.IO_Exceptions;
with Ada.Command_Line;
with Ada.Exceptions;

with GNAT.Command_Line;

with Util.Log.Loggers;

with Are.Generator;
procedure Are.Main is

   Log     : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("Are.Main");

begin
   Are.Configure_Logs (Debug => False, Verbose => False);

   Are.Generator.Main;

exception
   when GNAT.Command_Line.Exit_From_Command_Line | GNAT.Command_Line.Invalid_Switch =>
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);

   when E : Ada.IO_Exceptions.Name_Error =>
      Log.Error (-("cannot access file: {0}"), Ada.Exceptions.Exception_Message (E));
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);

   when E : others =>
      Log.Error (-("some internal error occurred"), E, True);
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);

end Are.Main;
