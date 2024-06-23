-----------------------------------------------------------------------
--  are-installer-concat -- Concatenate based distribution artifact
--  Copyright (C) 2012, 2021 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

--  == Install mode: concat ==
--  The `concat` mode provides a distribution rule that concatenates a list of
--  files.  The rule is created by using the following XML definition:
--
--    <install mode='concat' source-timestamp='yes'>
--      <include name="NOTICE.txt"/>
--    </install>
--
--  This rule is useful when the tool is invoked with several directories that
--  contain files with identical names.  Unlike the `copy` and `copy-first`
--  rules that take into account only one source file, the `concat` mode handles
--  this situation by concatenatating the source files.
--
--  By default the generated file has a timestamp which correspond to the time
--  when the `are` command is executed.  By setting the `source-timestamp`
--  attribute to `true`, the generated file is assigned the timestamp of the
--  newest file in the source files.
--
private package Are.Installer.Concat is

   --  Create a distribution rule to concatenate a set of files.
   function Create_Rule (Node : in DOM.Core.Node) return Distrib_Rule_Access;

   --  ------------------------------
   --  Distribution artifact
   --  ------------------------------
   type Concat_Rule is new Distrib_Rule with private;
   type Concat_Rule_Access is access all Concat_Rule'Class;

   --  Get a name to qualify the installation rule (used for logs).
   overriding
   function Get_Install_Name (Rule : in Concat_Rule) return String;

   --  Install the file <b>File</b> according to the distribution rule.
   --  Concatenate the files listed in <b>Files</b> in the target path specified by <b>Path</b>.
   overriding
   procedure Install (Rule    : in Concat_Rule;
                      Path    : in String;
                      Files   : in File_Vector;
                      Context : in out Context_Type'Class);

private

   type Concat_Rule is new Distrib_Rule with null record;

end Are.Installer.Concat;
