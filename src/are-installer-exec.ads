-----------------------------------------------------------------------
--  are-installer-exec -- External command based distribution artifact
--  Copyright (C) 2012, 2021 Stephane Carrez
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
with EL.Expressions;

--  == Install mode: exec ==
--  The `exec` mode is the most powerful installation rule since it allows
--  to execute a command on the source file.
--  to copy a file or a directory by using an external program.  The rule is
--  created by using the following XML definition:
--
--    <install mode='exec' dir='target' source-timestamp='true'>
--      <command slow='false' output='...'>cmd #{src} #{dst}</command>
--      <fileset dir="source">
--        <include name="**/*"/>
--      </fileset>
--    </install>
--
--  The command is a string which can contain EL expressions that are
--  evaluated before executing the command.  The command is executed for
--  each source file.  The following EL variables are defined:
--
--    src   defines the absolute source path
--    dst   defines the target destination path
--    name  defines the relative source name (ie, the name of the resource file)
--
private package Are.Installer.Exec is

   --  Create a distribution rule to copy a set of files or directories and
   --  execute an external command.
   function Create_Rule (Node : in DOM.Core.Node;
                         Copy : in Boolean) return Distrib_Rule_Access;

   --  ------------------------------
   --  Distribution artifact
   --  ------------------------------
   type Exec_Rule is new Distrib_Rule with private;
   type Exec_Rule_Access is access all Exec_Rule'Class;

   --  Get a name to qualify the installation rule (used for logs).
   overriding
   function Get_Install_Name (Rule    : in Exec_Rule) return String;

   overriding
   procedure Install (Rule    : in Exec_Rule;
                      Path    : in String;
                      Files   : in File_Vector;
                      Context : in out Context_Type'Class);

private

   type Exec_Rule is new Distrib_Rule with record
      Command          : EL.Expressions.Expression;
      Output           : EL.Expressions.Expression;
      Output_Append    : Boolean;
      Copy_First       : Boolean := False;
      Slow_Flag        : Boolean := False;
      Source_Timestamp : Boolean := False;
   end record;

end Are.Installer.Exec;
