-----------------------------------------------------------------------
--  are-installer-copies -- Copy based distribution artifact
--  Copyright (C) 2012, 2013, 2021 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Ada.Directories;

with Util.Log.Loggers;

package body Are.Installer.Copies is

   use Util.Log;

   Log : constant Loggers.Logger := Loggers.Create ("Are.Installer.Copies");

   --  ------------------------------
   --  Create a distribution rule to copy a set of files or directories.
   --  ------------------------------
   function Create_Rule (Copy_First_File : in Boolean) return Distrib_Rule_Access is
      Result : constant Copy_Rule_Access := new Copy_Rule;
   begin
      Result.Copy_First_File := Copy_First_File;
      return Result.all'Access;
   end Create_Rule;

   --  ------------------------------
   --  Get a name to qualify the installation rule (used for logs).
   --  ------------------------------
   overriding
   function Get_Install_Name (Rule : in Copy_Rule) return String is
      pragma Unreferenced (Rule);
   begin
      return "copy";
   end Get_Install_Name;

   overriding
   procedure Install (Rule    : in Copy_Rule;
                      Path    : in String;
                      Files   : in File_Vector;
                      Context : in out Context_Type'Class) is
      use type Ada.Containers.Count_Type;

      Source : constant String := Get_Source_Path (Files, Rule.Copy_First_File);
      Target : constant String := Context.Get_Generation_Path (Path);
      Dir    : constant String := Ada.Directories.Containing_Directory (Target);
   begin
      if Files.Length > 1 then
         Log.Info ("copy {0} to {1} (ignoring {2} files)", Source, Path,
                   Natural'Image (Natural (Files.Length) - 1));
      else
         Log.Info ("copy {0} to {1}", Source, Path);
      end if;

      Ada.Directories.Create_Path (Dir);
      Ada.Directories.Copy_File (Source_Name => Source,
                                 Target_Name => Target,
                                 Form        => "preserve=all_attributes, mode=overwrite");

      Rule.Add_File (Path, Target, Ada.Directories.Modification_Time (Source));
   end Install;

end Are.Installer.Copies;
