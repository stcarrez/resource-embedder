-----------------------------------------------------------------------
--  are-installer-copies -- Copy based distribution artifact
--  Copyright (C) 2012, 2013, 2021 Stephane Carrez
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

--  == Install mode: copy and copy-first ==
--  The `copy` and `copy-first` mode are the simpler distribution rules that
--  only copy the source file to the destination.  The rule is created by using
--  the following XML definition:
--
--    <install mode='copy'>
--      <include name="*.txt"/>
--    </install>
--
--  If the tool is called with several directories that contain a same file name
--  then the `copy` installer will complain because it has two source files for
--  a same destination name.  When this happens, you may instead use the `copy-first`
--  mode which will take into account only the first file found in the first directory.
--
--  By default the relative path name of the file is used to identify the embedded
--  content.  Sometimes, you may want to drop the file extension and access the
--  content by using only the name of the file without its extension.  This is
--  possible by setting the `strip-extension` attribute to `yes` as follows:
--
--    <install mode='copy' strip-extension='yes'>
--      <install name="*.txt"/>
--    </install>
--
--  If the file has the name `help.txt`, then it is known internally by the
--  name `help`.
--
private package Are.Installer.Copies is

   --  Create a distribution rule to copy a set of files or directories.
   function Create_Rule (Copy_First_File : in Boolean) return Distrib_Rule_Access;

   --  ------------------------------
   --  Distribution artifact
   --  ------------------------------
   type Copy_Rule is new Distrib_Rule with private;
   type Copy_Rule_Access is access all Copy_Rule'Class;

   --  Get a name to qualify the installation rule (used for logs).
   overriding
   function Get_Install_Name (Rule : in Copy_Rule) return String;

   overriding
   procedure Install (Rule    : in Copy_Rule;
                      Path    : in String;
                      Files   : in File_Vector;
                      Context : in out Context_Type'Class);

private

   type Copy_Rule is new Distrib_Rule with record
      --  When True and there are several source files, use the first file.
      --  Otherwise, use the last file.
      Copy_First_File : Boolean := False;
   end record;

end Are.Installer.Copies;
