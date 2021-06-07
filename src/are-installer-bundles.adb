-----------------------------------------------------------------------
--  are-installer-bundles -- Merge bundles for distribution artifact
--  Copyright (C) 2013, 2017, 2021 Stephane Carrez
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
with Ada.Exceptions;
with Ada.IO_Exceptions;
with Ada.Text_IO;

with Util.Log.Loggers;
with Util.Properties;

package body Are.Installer.Bundles is

   use Util.Log;
   use type Ada.Calendar.Time;

   Log : constant Loggers.Logger := Loggers.Create ("Are.Installer.Bundles");

   --  ------------------------------
   --  Create a distribution rule to copy a set of files or directories.
   --  ------------------------------
   function Create_Rule (Node : in DOM.Core.Node) return Distrib_Rule_Access is
      pragma Unreferenced (Node);

      Result : constant Bundle_Rule_Access := new Bundle_Rule;
   begin
      return Result.all'Access;
   end Create_Rule;

   --  ------------------------------
   --  Get a name to qualify the installation rule (used for logs).
   --  ------------------------------
   overriding
   function Get_Install_Name (Rule : in Bundle_Rule) return String is
      pragma Unreferenced (Rule);
   begin
      return "bundle";
   end Get_Install_Name;

   --  ------------------------------
   --  Install the file <b>File</b> according to the distribution rule.
   --  Merge all the files listed in <b>Files</b> in the target path specified by <b>Path</b>.
   --  ------------------------------
   overriding
   procedure Install (Rule    : in Bundle_Rule;
                      Path    : in String;
                      Files   : in File_Vector;
                      Context : in out Context_Type'Class) is
      procedure Load_File (File : in File_Record);
      procedure Merge_Property (Name : in String;
                                Item : in Util.Properties.Value);
      procedure Save_Property (Name : in String;
                               Item : in Util.Properties.Value);

      Target    : constant String := Context.Get_Generation_Path (Path);
      Dir       : constant String := Ada.Directories.Containing_Directory (Target);
      Output    : Ada.Text_IO.File_Type;
      Merge     : Util.Properties.Manager;
      Modtime   : Ada.Calendar.Time;
      Has_Files : Boolean := False;

      --  ------------------------------
      --  Merge the property into the target property list.
      --  ------------------------------
      procedure Merge_Property (Name : in String;
                                Item : in Util.Properties.Value) is
      begin
         Merge.Set_Value (Name, Item);
      end Merge_Property;

      procedure Save_Property (Name : in String;
                               Item : in Util.Properties.Value) is
      begin
         Ada.Text_IO.Put (Output, Name);
         Ada.Text_IO.Put (Output, "=");
         Ada.Text_IO.Put_Line (Output, Util.Properties.To_String (Item));
      end Save_Property;

      --  ------------------------------
      --  Append the file to the output
      --  ------------------------------
      procedure Load_File (File : in File_Record) is
         File_Path : constant String := Rule.Get_Source_Path (File);
         Props     : Util.Properties.Manager;
         Time      : Ada.Calendar.Time;
      begin
         Log.Info ("loading {0}", File_Path);

         Time := Ada.Directories.Modification_Time (File_Path);
         if not Has_Files or else Time >= Modtime then
            Modtime := Time;
         end if;
         Has_Files := True;
         Props.Load_Properties (Path => File_Path);

         Props.Iterate (Process => Merge_Property'Access);
      exception
         when Ex : Ada.IO_Exceptions.Name_Error =>
            Context.Error ("Cannot read {0}: ", File_Path, Ada.Exceptions.Exception_Message (Ex));

      end Load_File;

      Iter   : File_Cursor := Files.First;
   begin
      Ada.Directories.Create_Path (Dir);

      while File_Record_Vectors.Has_Element (Iter) loop
         File_Record_Vectors.Query_Element (Iter, Load_File'Access);
         File_Record_Vectors.Next (Iter);
      end loop;
      Ada.Text_IO.Create (File => Output, Mode => Ada.Text_IO.Out_File, Name => Target);
      Merge.Iterate (Process => Save_Property'Access);
      Ada.Text_IO.Close (File => Output);

      if Has_Files then
         Rule.Add_File (Path, Target, Modtime);
      end if;
   end Install;

end Are.Installer.Bundles;
