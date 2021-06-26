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
with Ada.Directories;
with Ada.Command_Line;
with Ada.Text_IO;

with Are.Generator.Ada2012;
with Are.Generator.C;
with Are.Generator.Go;
with Are.Installer;
package body Are.Generator is

   use type GNAT.Strings.String_Access;
   use type Ada.Command_Line.Exit_Status;

   Ada_Generator : Are.Generator.Ada2012.Generator_Type;
   C_Generator   : Are.Generator.C.Generator_Type;
   Go_Generator  : Are.Generator.Go.Generator_Type;
   Installer     : Are.Installer.Installer_Type;
   Config        : GC.Command_Line_Configuration;
   Context       : Are.Context_Type;
   Cur_Resource  : Are.Resource_Access;

   procedure Add_Option (Switch, Value : in String) is
   begin
      if Switch = "--resource" then
         Are.Create_Resource (List     => Context.Resources,
                              Name     => Value,
                              Resource => Cur_Resource);
      else
         Installer.Add_Rule (Resource => Cur_Resource,
                             Pattern  => Value);
      end if;
   end Add_Option;

   procedure Specific_Options (Config  : in out GC.Command_Line_Configuration;
                               Context : in out Are.Context_Type'Class) is separate;

   --  ------------------------------
   --  Return a string that identifies the program.
   --  ------------------------------
   function Get_Title return String is
   begin
      return "Advanced Resource Embedder 1.1.0";
   end Get_Title;

   --  ------------------------------
   --  Print the command usage.
   --  ------------------------------
   procedure Usage is
   begin
      GC.Display_Help (Config);
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end Usage;

   procedure Main is
      Has_Files : Boolean := False;
   begin
      GC.Set_Usage (Config => Config,
                    Usage  => "[switchs] {file|directory}",
                    Help   => -("are - resource embedder to include files "
                      & "in your Ada,C/C++,Go binaries"));
      GC.Define_Switch (Config => Config,
                        Output => Context.Verbose'Access,
                        Switch => "-v",
                        Long_Switch => "--verbose",
                        Help   => -("Verbose execution mode"));
      GC.Define_Switch (Config => Config,
                        Output => Context.Debug'Access,
                        Switch => "-vv",
                        Long_Switch => "--debug",
                        Help   => -("Enable debug execution"));
      GC.Define_Switch (Config => Config,
                        Output => Context.Version'Access,
                        Switch => "-V",
                        Long_Switch => "--version",
                        Help   => -("Print the program version"));
      GC.Define_Switch (Config => Config,
                        Output => Context.Tmp_Dir'Access,
                        Long_Switch => "--tmp=",
                        Argument => "DIRECTORY",
                        Help   => -("Use the directory to build the resource files"));
      GC.Define_Switch (Config => Config,
                        Output => Context.Keep_Temporary'Access,
                        Switch => "-k",
                        Long_Switch => "--keep",
                        Help   => -("Keep the directory used to prepare the resource files"));
      GC.Define_Switch (Config => Config,
                        Output => Context.Output'Access,
                        Switch => "-o:",
                        Long_Switch => "--output=",
                        Argument => "DIRECTORY",
                        Help   => -("Set the output directory path where generators "
                          & "write the code"));
      GC.Define_Switch (Config => Config,
                        Output => Context.Language'Access,
                        Switch => "-l:",
                        Long_Switch => "--lang=",
                        Argument => "NAME",
                        Help   => -("Select the target generator language"));
      GC.Define_Switch (Config => Config,
                        Output => Context.Rule_File'Access,
                        Switch => "-r:",
                        Long_Switch => "--rule=",
                        Argument => "PATH",
                        Help   => -("Read the XML file that describe the resources to generate"));

      --  Hack to cope with different implementations of GNAT.Command_Line package.
      Specific_Options (Config, Context);

      GC.Define_Switch (Config => Config,
                        Output => Context.Ignore_Case'Access,
                        Long_Switch => "--ignore-case",
                        Help   => -("Ignore the case when comparing names"));
      GC.Define_Switch (Config => Config,
                        Output => Context.Name_Index'Access,
                        Long_Switch => "--name-access",
                        Help   => -("Generate support to query content with a name"));
      GC.Define_Switch (Config => Config,
                        Output => Context.List_Content'Access,
                        Long_Switch => "--list-access",
                        Help   => -("Generate support to list the content names"));
      GC.Define_Switch (Config => Config,
                        Output => Context.Declare_Var'Access,
                        Long_Switch => "--var-access",
                        Help   => -("Declare a variable to give access to each content"));
      GC.Define_Switch (Config => Config,
                        Output => Context.No_Type_Declaration'Access,
                        Long_Switch => "--no-type-declaration",
                        Help   => -("Do not declare any type in the package "
                          & "specification (assume they are inherited)"));
      GC.Define_Switch (Config => Config,
                        Output => Context.Type_Name'Access,
                        Long_Switch => "--type-name=",
                        Argument => "NAME",
                        Help   => -("Define the name of the type used to hold the information"));
      GC.Define_Switch (Config => Config,
                        Output => Context.Function_Name'Access,
                        Long_Switch => "--function-name=",
                        Argument => "NAME",
                        Help   => -("Define the name of the function to get the "
                          & "information from a name"));
      GC.Define_Switch (Config => Config,
                        Output => Context.Member_Content_Name'Access,
                        Long_Switch => "--member-content=",
                        Argument => "NAME",
                        Help   => -("Define the name data structure member holding "
                          & "the content"));
      GC.Define_Switch (Config => Config,
                        Output => Context.Member_Length_Name'Access,
                        Long_Switch => "--member-length=",
                        Argument => "NAME",
                        Help   => -("Define the name data structure member holding "
                          & "the length"));
      GC.Define_Switch (Config => Config,
                        Output => Context.Member_Modtime_Name'Access,
                        Long_Switch => "--member-modtime=",
                        Argument => "NAME",
                        Help   => -("Define the name data structure member holding "
                          & "the modification time"));
      GC.Define_Switch (Config => Config,
                        Output => Context.Member_Format_Name'Access,
                        Long_Switch => "--member-format=",
                        Argument => "NAME",
                        Help   => -("Define the name data structure member holding "
                          & "the content format"));

      Ada_Generator.Setup (Config);
      GC.Getopt (Config => Config);

      if Context.Version then
         Ada.Text_IO.Put_Line (Get_Title);
         return;
      end if;

      if Context.Verbose or Context.Debug then
         Are.Configure_Logs (Verbose => Context.Verbose,
                             Debug   => Context.Debug);
      end if;

      --  Emulate the Define_Switch with the Callback support
      if Context.Resource_Name /= null and then Context.Resource_Name'Length > 0 then
         Add_Option ("--resource", Context.Resource_Name.all);
      end if;
      if Context.Fileset_Pattern /= null and then Context.Fileset_Pattern'Length > 0 then
         Add_Option ("--fileset", Context.Fileset_Pattern.all);
      end if;

      --  Read the rule definitions.
      if Context.Rule_File'Length > 0 then
         Installer.Read_Package (Context.Rule_File.all, Context);
         if Context.Status /= 0 then
            Ada.Command_Line.Set_Exit_Status (Context.Status);
            return;
         end if;
      end if;

      --  Step #1: collect the files by scanning the directories and files passed as argument.
      loop
         declare
            use Ada.Directories;

            S : constant String := GC.Get_Argument;
         begin
            exit when S'Length = 0;
            Has_Files := True;
            if not Ada.Directories.Exists (S) then
               Context.Error ("path {0} does not exist", S);
            elsif Ada.Directories.Kind (S) = Ada.Directories.Directory then
               Installer.Scan_Directory (S, Context);
            else
               Context.Error ("path {0} is not a directory", S);
            end if;
         end;
      end loop;

      if not Has_Files then
         Usage;
         return;
      end if;

      --  Step #2: apply the package rules to load the files and optionally apply some
      --  specific transformations on them (such as running a Javascript minifier).
      Installer.Execute (Context);

      if Context.Status = 0 then
         --  Step #3: run the code generator.
         if Context.Language.all = "c" then
            C_Generator.Generate (Context.Resources, Context);

         elsif Context.Language.all = "go" then
            Go_Generator.Generate (Context.Resources, Context);

         elsif Context.Language.all = "Ada" or Context.Language'Length = 0 then
            Ada_Generator.Generate (Context.Resources, Context);

         else
            Context.Error ("language {0} not recognized", Context.Language.all);
         end if;
      end if;

      if not Context.Keep_Temporary then
         declare
            Path : constant String := Context.Get_Generation_Path ("");
         begin
            if Ada.Directories.Exists (Path) then
               Ada.Directories.Delete_Tree (Path);
            end if;
         end;
      end if;

      Ada.Command_Line.Set_Exit_Status (Context.Status);
   end Main;

end Are.Generator;
