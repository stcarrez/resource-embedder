-----------------------------------------------------------------------
--  are-installer-exec -- External command based distribution artifact
--  Copyright (C) 2012, 2020, 2021 Stephane Carrez
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
with Ada.Directories;

with Util.Processes;
with Util.Beans.Objects;
with Util.Log.Loggers;

with EL.Variables.Default;
with EL.Contexts.Default;

with Are.Utils;

package body Are.Installer.Exec is

   use Util.Log;

   Log : constant Loggers.Logger := Loggers.Create ("Are.Installer.Exec");

   --  ------------------------------
   --  Create a distribution rule to copy a set of files or directories and
   --  execute an external command.
   --  ------------------------------
   function Create_Rule (Node : in DOM.Core.Node;
                         Copy : in Boolean) return Distrib_Rule_Access is
      use type DOM.Core.Node;

      Ctx     : EL.Contexts.Default.Default_Context;
      C       : constant DOM.Core.Node := Are.Utils.Get_Child (Node, "command");
      Command : constant String := Are.Utils.Get_Data_Content (Node, "command");
      Result  : constant Exec_Rule_Access := new Exec_Rule;
   begin
      if C /= null then
         declare
            Output : constant String := Are.Utils.Get_Attribute (C, "output");
         begin
            if Output /= "" then
               Result.Output := EL.Expressions.Create_Expression (Output, Ctx);
               Result.Output_Append := Are.Utils.Get_Attribute (C, "append");
            end if;
            Result.Slow_Flag := Are.Utils.Get_Attribute (C, "slow");
            if Result.Slow_Flag then
               Result.Level := Util.Log.INFO_LEVEL;
            end if;
         end;
      end if;
      Result.Command := EL.Expressions.Create_Expression (Command, Ctx);
      Result.Copy_First := Copy;
      return Result.all'Access;
   end Create_Rule;

   --  ------------------------------
   --  Distribution artifact
   --  ------------------------------

   --  ------------------------------
   --  Get a name to qualify the installation rule (used for logs).
   --  ------------------------------
   overriding
   function Get_Install_Name (Rule    : in Exec_Rule) return String is
      E : constant String := Rule.Command.Get_Expression;
   begin
      return "exec '" & E & "'";
   end Get_Install_Name;

   overriding
   procedure Install (Rule    : in Exec_Rule;
                      Path    : in String;
                      Files   : in File_Vector;
                      Context : in out Context_Type'Class) is

      Ctx       : EL.Contexts.Default.Default_Context;
      Variables : aliased EL.Variables.Default.Default_Variable_Mapper;
      Source    : constant String := Get_Source_Path (Files);
      Target    : constant String := Context.Get_Generation_Path (Path);
      Name      : constant String := Ada.Directories.Simple_Name (Source);
   begin
      if Rule.Level >= Util.Log.INFO_LEVEL then
         Log.Info ("install {0} to {1}", Source, Path);
      end if;

      Variables.Bind ("src", Util.Beans.Objects.To_Object (Source));
      Variables.Bind ("name", Util.Beans.Objects.To_Object (Name));
      Variables.Bind ("dst", Util.Beans.Objects.To_Object (Target));

      --  If necessary copy the source file to the destination before running the command.
      if Rule.Copy_First then
         Ada.Directories.Copy_File (Source_Name => Source,
                                    Target_Name => Target,
                                    Form        => "preserve=all_attributes, mode=overwrite");
      end if;

      Ctx.Set_Variable_Mapper (Variables'Unchecked_Access);
      declare
         Cmd     : constant Util.Beans.Objects.Object := Rule.Command.Get_Value (Ctx);
         Command : constant String := Util.Beans.Objects.To_String (Cmd);
         Proc    : Util.Processes.Process;
      begin
         --  If an output is specified, redirect the process output stream.
         if not Rule.Output.Is_Null then
            declare
               Output   : constant Util.Beans.Objects.Object := Rule.Output.Get_Value (Ctx);
               Out_File : constant String := Util.Beans.Objects.To_String (Output);
               Out_Dir  : constant String := Ada.Directories.Containing_Directory (Out_File);
            begin
               Ada.Directories.Create_Path (Out_Dir);
               Util.Processes.Set_Output_Stream (Proc, Out_File, Rule.Output_Append);
            end;
         end if;
         Util.Processes.Spawn (Proc, Command);
         Util.Processes.Wait (Proc);
         if Util.Processes.Get_Exit_Status (Proc) /= 0 then
            Context.Error ("Command {0} exited with status {1}", Command,
                           Integer'Image (Util.Processes.Get_Exit_Status (Proc)));
         elsif Rule.Source_Timestamp then
            Rule.Resource.Add_File (Path, Target,
                                    Ada.Directories.Modification_Time (Source));
         else
            Rule.Resource.Add_File (Path, Target);
         end if;
      end;

   exception
      when others =>
         if Ada.Directories.Exists (Target) then
            Ada.Directories.Delete_File (Target);
         end if;
         raise;
   end Install;

end Are.Installer.Exec;
