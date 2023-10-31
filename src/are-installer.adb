-----------------------------------------------------------------------
--  are-installer -- Resource selector, preparer and installer
--  Copyright (C) 2012, 2013, 2015, 2020, 2021, 2023 Stephane Carrez
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
with Ada.Unchecked_Deallocation;

with Util.Files;
with Util.Strings;
with Util.Log.Loggers;

with Are.Utils;
with Are.Installer.Copies;
with Are.Installer.Exec;
with Are.Installer.Concat;
with Are.Installer.Bundles;
with Are.Installer.Merges;

with Sax.Readers;
with Input_Sources.File;
with DOM.Core.Documents;
with DOM.Readers;

package body Are.Installer is

   use Util.Log;

   Log : constant Loggers.Logger := Loggers.Create ("Are.Installer");

   --  ------------------------------
   --  Create a resource rule identified by `Kind`.
   --  The resource rule is configured according to the DOM tree whose node is `Node`.
   --  ------------------------------
   function Create_Rule (Kind     : in String;
                         Node     : in DOM.Core.Node) return Distrib_Rule_Access is
   begin
      Log.Debug ("Creating resource rule {0}", Kind);

      if Kind = "copy" or else Kind = "" then
         return Are.Installer.Copies.Create_Rule (False);
      elsif Kind = "copy-first" then
         return Are.Installer.Copies.Create_Rule (True);
      elsif Kind = "exec" then
         return Are.Installer.Exec.Create_Rule (Node, False);
      elsif Kind = "copy-exec" then
         return Are.Installer.Exec.Create_Rule (Node, True);
      elsif Kind = "concat" then
         return Are.Installer.Concat.Create_Rule (Node);
      elsif Kind = "bundle" then
         return Are.Installer.Bundles.Create_Rule (Node);
      elsif Kind = "webmerge" then
         return Are.Installer.Merges.Create_Rule (Node);
      else
         return null;
      end if;
   end Create_Rule;

   --  ------------------------------
   --  Add a simple rule to copy the files matching the pattern on the resource.
   --  ------------------------------
   procedure Add_Rule (Installer : in out Installer_Type;
                       Resource  : in Are.Resource_Access;
                       Pattern   : in String) is
      Rule  : constant Distrib_Rule_Access := Copies.Create_Rule (Copy_First_File => True);
      Match : Match_Rule;
   begin
      Rule.Resource := Resource;
      Installer.Rules.Append (Rule);
      Match.Match := Ada.Strings.Unbounded.To_Unbounded_String (Pattern);
      Rule.Matches.Append (Match);
   end Add_Rule;

   --  ------------------------------
   --  Read the XML package file that describes the resources and build the list
   --  of rules to collect and build those resources.
   --  ------------------------------
   procedure Read_Package (Installer : in out Installer_Type;
                           File      : in String;
                           Context   : in out Context_Type'Class) is
      use Ada.Strings.Maps;

      procedure Register_Rule (Resource : in out Are.Resource_Access;
                               Node     : in DOM.Core.Node);
      procedure Register_Resource (List   : in out Are.Resource_List;
                                   Node   : in DOM.Core.Node);
      procedure Register_Resources (List  : in out Are.Resource_List;
                                    Node  : in DOM.Core.Node);
      procedure Register_Line_Separator (Resource : in out Are.Resource_Access;
                                         Node     : in DOM.Core.Node);
      procedure Register_Line_Filter (Resource : in out Are.Resource_Access;
                                      Node     : in DOM.Core.Node);
      procedure Register_Header (Resource : in out Are.Resource_Access;
                                 Node     : in DOM.Core.Node);

      --  ------------------------------
      --  Register a new type mapping.
      --  ------------------------------
      procedure Register_Rule (Resource : in out Are.Resource_Access;
                               Node : in DOM.Core.Node) is

         --  Collect the include definitions for the distribution rule.
         procedure Collect_Includes (Rule  : in out Distrib_Rule_Access;
                                     Node  : in DOM.Core.Node);

         --  Collect the exclude definitions for the distribution rule.
         procedure Collect_Excludes (Rule  : in out Distrib_Rule_Access;
                                     Node  : in DOM.Core.Node);

         --  Collect the fileset definitions for the distribution rule.
         procedure Collect_Filesets (Rule  : in out Distrib_Rule_Access;
                                     Node  : in DOM.Core.Node);

         use Ada.Strings.Unbounded;

         Dir   : constant UString := Are.Utils.Get_Attribute (Node, "dir");
         Mode  : constant String := To_String (Are.Utils.Get_Attribute (Node, "mode"));
         Level : constant String := To_String (Are.Utils.Get_Attribute (Node, "log"));
         Match : Match_Rule;

         --  ------------------------------
         --  Collect the include definitions for the distribution rule.
         --  ------------------------------
         procedure Collect_Includes (Rule  : in out Distrib_Rule_Access;
                                     Node  : in DOM.Core.Node) is
            Name : constant UString := Are.Utils.Get_Attribute (Node, "name");
         begin
            if Name = "" then
               Context.Error ("{0}: empty include name in '<include>' definition", File);
               return;
            end if;
            Match.Match := Name;
            Rule.Matches.Append (Match);
         end Collect_Includes;

         --  ------------------------------
         --  Collect the exclude definitions for the distribution rule.
         --  ------------------------------
         procedure Collect_Excludes (Rule  : in out Distrib_Rule_Access;
                                     Node  : in DOM.Core.Node) is
            Name : constant UString := Are.Utils.Get_Attribute (Node, "name");
         begin
            if Name = "" then
               Context.Error ("{0}: empty exclude name in '<exclude>' definition", File);
               return;
            end if;
            Match.Match := Name;
            Rule.Excludes.Append (Match);
         end Collect_Excludes;

         procedure Iterate is
           new Are.Utils.Iterate_Nodes (T => Distrib_Rule_Access,
                                        Process => Collect_Includes);

         procedure Iterate_Excludes is
           new Are.Utils.Iterate_Nodes (T => Distrib_Rule_Access,
                                        Process => Collect_Excludes);

         --  ------------------------------
         --  Collect the include definitions for the distribution rule.
         --  ------------------------------
         procedure Collect_Filesets (Rule  : in out Distrib_Rule_Access;
                                     Node  : in DOM.Core.Node) is
            Dir : constant UString := Are.Utils.Get_Attribute (Node, "dir");
         begin
            if Dir /= "." then
               Match.Base_Dir := Dir;
            end if;
            Iterate (Rule, Node, "include");
            Iterate_Excludes (Rule, Node, "exclude", False);
         end Collect_Filesets;

         procedure Iterate_Filesets is
           new Are.Utils.Iterate_Nodes (T => Distrib_Rule_Access,
                                        Process => Collect_Filesets);

         Rule  : Distrib_Rule_Access := Create_Rule (Kind     => Mode,
                                                     Node     => Node);
      begin
         Log.Debug ("Install {0} in {1}", Dir, To_String (Resource.Name));

         if Rule = null then
            Context.Error ("{0}: rule '" & Mode & "' is not recognized", File);
            return;
         end if;

         Rule.Resource := Resource;
         Rule.Dir := Dir;
         if Level = "info" then
            Rule.Level := Util.Log.INFO_LEVEL;
         elsif Level = "debug" then
            Rule.Level := Util.Log.DEBUG_LEVEL;
         else
            Rule.Level := Util.Log.WARN_LEVEL;
         end if;
         Rule.Source_Timestamp := Are.Utils.Get_Attribute (Node, "source-timestamp");
         Rule.Strip_Extension := Are.Utils.Get_Attribute (Node, "strip-extension");
         Installer.Rules.Append (Rule);
         Iterate (Rule, Node, "include", False);
         Iterate_Excludes (Rule, Node, "exclude", False);
         Iterate_Filesets (Rule, Node, "fileset");
         if Rule.Matches.Is_Empty then
            Context.Error ("{0}: empty fileset definition", File);
         end if;
      end Register_Rule;

      --  ------------------------------
      --  Register a new line separator.
      --  ------------------------------
      procedure Register_Line_Separator (Resource : in out Are.Resource_Access;
                                         Node     : in DOM.Core.Node) is
         Separator : constant String := Are.Utils.Get_Data_Content (Node);
      begin
         if Separator'Length = 0 then
            return;
         end if;
         if Separator'Length = 1 then
            Resource.Separators := Resource.Separators or To_Set (Separator);

         elsif Separator = "\n" then
            Resource.Separators := Resource.Separators or To_Set (ASCII.LF);

         elsif Separator = "\r" then
            Resource.Separators := Resource.Separators or To_Set (ASCII.CR);

         elsif Separator = "\t" then
            Resource.Separators := Resource.Separators or To_Set (ASCII.HT);

         else
            Resource.Separators := Resource.Separators or To_Set (Separator);
         end if;
      end Register_Line_Separator;

      --  ------------------------------
      --  Register a new line filter.
      --  ------------------------------
      procedure Register_Line_Filter (Resource : in out Are.Resource_Access;
                                      Node     : in DOM.Core.Node) is
         Pattern : constant String := Are.Utils.Get_Data_Content (Node);
         Replace : constant String := Are.Utils.Get_Attribute (Node, "replace");
      begin
         Resource.Add_Line_Filter (Pattern, Replace);

      exception
         when GNAT.Regpat.Expression_Error =>
            Context.Error ("{0}: invalid pattern '{1}'", File, Pattern);
      end Register_Line_Filter;

      --  ------------------------------
      --  Register a new header line to add in the generated files.
      --  ------------------------------
      procedure Register_Header (Resource : in out Are.Resource_Access;
                                 Node     : in DOM.Core.Node) is
         Header : constant String := Are.Utils.Get_Data_Content (Node);
         Kind   : constant String := Are.Utils.Get_Attribute (Node, "type");
      begin
         if Kind = "" or else Kind = "spec" then
            Resource.Headers_Spec.Append (Header);
         end if;
         if Kind = "" or else Kind = "body" then
            Resource.Headers_Impl.Append (Header);
         end if;
      end Register_Header;

      --  ------------------------------
      --  Register the resource definition.
      --  ------------------------------
      procedure Register_Resource (List  : in out Are.Resource_List;
                                   Node  : in DOM.Core.Node) is
         function Get_Format (Name : in String) return Are.Format_Type;
         procedure Iterate is
           new Are.Utils.Iterate_Nodes (T => Are.Resource_Access,
                                        Process => Register_Rule);
         procedure Iterate_Line_Separator is
           new Are.Utils.Iterate_Nodes (T => Are.Resource_Access,
                                        Process => Register_Line_Separator);
         procedure Iterate_Line_Filter is
           new Are.Utils.Iterate_Nodes (T => Are.Resource_Access,
                                        Process => Register_Line_Filter);
         procedure Iterate_Header is
           new Are.Utils.Iterate_Nodes (T => Are.Resource_Access,
                                        Process => Register_Header);

         function Get_Format (Name : in String) return Are.Format_Type is
         begin
            if Name = "binary" then
               return R_BINARY;
            elsif Name = "string" then
               return R_STRING;
            elsif Name = "lines" then
               return R_LINES;
            else
               Context.Error ("{0}: invalid resource format '{1}'", File, Name);
               return R_BINARY;
            end if;
         end Get_Format;

         Name     : constant String := Are.Utils.Get_Attribute (Node, "name");
         Resource : Are.Resource_Access;
      begin
         Are.Create_Resource (List, Name, Resource);
         Resource.Type_Name := Are.Utils.Get_Attribute (Node, "type", "");
         Resource.Format := Get_Format (Are.Utils.Get_Attribute (Node, "format", "binary"));
         Resource.Function_Name := Are.Utils.Get_Attribute (Node, "function-name", "");
         Resource.Member_Content_Name := Are.Utils.Get_Attribute (Node, "member-content", "");
         Resource.Member_Modtime_Name := Are.Utils.Get_Attribute (Node, "member-time", "");
         Resource.Member_Length_Name := Are.Utils.Get_Attribute (Node, "member-length", "");
         Resource.Member_Format_Name := Are.Utils.Get_Attribute (Node, "member-format", "");
         Resource.Keep_Empty_Lines := Are.Utils.Get_Attribute (Node, "keep-empty-lines", False);
         Iterate_Header (Resource, Node, "header");
         Iterate_Line_Separator (Resource, Node, "line-separator");
         Iterate_Line_Filter (Resource, Node, "line-filter");
         Iterate (Resource, Node, "install");
         if Resource.Format = R_LINES and then Resource.Separators = Null_Set then
            Context.Error ("{0}: missing 'line-separator' for resource '{1}'",
                           File, Name);
         end if;
      end Register_Resource;

      --  ------------------------------
      --  Register a resource description
      --  ------------------------------
      procedure Register_Resources (List  : in out Are.Resource_List;
                                    Node  : in DOM.Core.Node) is
         procedure Iterate is
           new Are.Utils.Iterate_Nodes (T => Are.Resource_List,
                                        Process => Register_Resource);
      begin
         Iterate (List, Node, "resource");
      end Register_Resources;

      procedure Iterate is new Are.Utils.Iterate_Nodes (T => Are.Resource_List,
                                                        Process => Register_Resources);

      Read           : Input_Sources.File.File_Input;
      My_Tree_Reader : DOM.Readers.Tree_Reader;
      Name_Start     : Natural;
   begin
      Log.Info ("Reading package file '{0}'", File);

      --  Base file name should be used as the public Id
      Name_Start := File'Last;
      while Name_Start >= File'First  and then File (Name_Start) /= '/' loop
         Name_Start := Name_Start - 1;
      end loop;
      Input_Sources.File.Open (File, Read);

      --  Full name is used as the system id
      Input_Sources.File.Set_System_Id (Read, File);
      Input_Sources.File.Set_Public_Id (Read, File (Name_Start + 1 .. File'Last));

      DOM.Readers.Set_Feature (My_Tree_Reader, Sax.Readers.Validation_Feature, False);

      DOM.Readers.Parse (My_Tree_Reader, Read);
      Input_Sources.File.Close (Read);

      declare
         Doc  : constant DOM.Core.Document := DOM.Readers.Get_Tree (My_Tree_Reader);
         Root : constant DOM.Core.Element  := DOM.Core.Documents.Get_Element (Doc);
      begin
         Iterate (Context.Resources, Root, "package");
      end;
      DOM.Readers.Free (My_Tree_Reader);

      Log.Info ("Loaded {0} rules for {1} resources",
                Util.Strings.Image (Natural (Installer.Rules.Length)),
                Util.Strings.Image (Are.Length (Context.Resources)));

   exception
      when Ada.IO_Exceptions.Name_Error =>
         Context.Error ("package file {0} does not exist", File);

      when E : Sax.Readers.XML_Fatal_Error =>
         Context.Error ("{0}",
                        Ada.Exceptions.Exception_Message (E));

   end Read_Package;

   --  ------------------------------
   --  Scan the directory collecting the files that must be taken into account and
   --  processed by the distribution rules.
   --  ------------------------------
   procedure Scan_Directory (Installer : in out Installer_Type;
                             Path      : in String;
                             Context   : in out Context_Type'Class) is
      pragma Unreferenced (Context);

      Tree : constant Directory_List_Access :=
        new Directory_List '(Length => 1, Name => ".", Rel_Pos => Path'Length + 2,
                             Path_Length => Path'Length, Path => Path, others => <>);
   begin
      Log.Debug ("Scanning directory: {0}", Path);

      Installer.Trees.Append (Tree);
      Scan (Path, ".", Tree);

      Log.Info ("Scanning directory: {0} found {1} files in {2} directories", Path,
                Util.Strings.Image (Natural (Tree.Files.Length)),
                Util.Strings.Image (Natural (Tree.Directories.Length)));
   end Scan_Directory;

   --  ------------------------------
   --  Execute the installation rules and collect the resources to be written
   --  in the context.
   --  ------------------------------
   procedure Execute (Installer : in out Installer_Type;
                      Context   : in out Context_Type'Class) is

      procedure Scan_Rule (Pos : in Distrib_Rule_Vectors.Cursor);
      procedure Execute_Rule (Pos : in Distrib_Rule_Vectors.Cursor);

      --  ------------------------------
      --  Process the rule by scaning the directory tree and detecting files that are concerned.
      --  ------------------------------
      procedure Scan_Rule (Pos : in Distrib_Rule_Vectors.Cursor) is
         Rule : constant Distrib_Rule_Access := Distrib_Rule_Vectors.Element (Pos);
         Iter : Directory_List_Vector.Cursor := Installer.Trees.First;
      begin
         Log.Debug ("Scanning rule");

         while Directory_List_Vector.Has_Element (Iter) loop
            Rule.Scan (Directory_List_Vector.Element (Iter).all);
            Directory_List_Vector.Next (Iter);
         end loop;
      end Scan_Rule;

      --  ------------------------------
      --  Execute the rules.
      --  ------------------------------
      procedure Execute_Rule (Pos : in Distrib_Rule_Vectors.Cursor) is
         Rule : constant Distrib_Rule_Access := Distrib_Rule_Vectors.Element (Pos);
      begin
         Log.Debug ("Process rule");

         Rule.Execute (Context);
      end Execute_Rule;

   begin
      Log.Info ("Executing {0} rules on {1} directory tree",
                Util.Strings.Image (Natural (Installer.Rules.Length)),
                Util.Strings.Image (Natural (Installer.Trees.Length)));

      Installer.Rules.Iterate (Process => Scan_Rule'Access);

      Installer.Rules.Iterate (Process => Execute_Rule'Access);
   end Execute;

   --  ------------------------------
   --  Get the relative path of the directory.
   --  ------------------------------
   function Get_Relative_Path (Dir : in Directory_List) return String is
   begin
      return Dir.Path (Dir.Rel_Pos .. Dir.Path'Last);
   end Get_Relative_Path;

   --  ------------------------------
   --  Get the first source path from the list.
   --  ------------------------------
   function Get_Source_Path (From           : in File_Vector;
                             Use_First_File : in Boolean := False) return String is
      use type Ada.Containers.Count_Type;
   begin
      if From.Length = 0 then
         return "";
      elsif Use_First_File then
         declare
            File : constant File_Record := From.Element (1);
         begin
            return Util.Files.Compose (File.Dir.Path, File.Name);
         end;
      else
         declare
            File : constant File_Record := From.Element (From.Last_Index);
         begin
            return Util.Files.Compose (File.Dir.Path, File.Name);
         end;
      end if;
   end Get_Source_Path;

   --  ------------------------------
   --  Build a regular expression pattern from a pattern string.
   --  ------------------------------
   function Make_Regexp (Pattern : in String) return String is
      Result : String (1 .. Pattern'Length * 2 + 2);
      Pos    : Natural := 1;
   begin
      Result (1) := '^';
      for I in Pattern'Range loop
         if Pattern (I) = '*' then
            Pos := Pos + 1;
            Result (Pos) := '.';
         elsif Pattern (I) in '.' | '$' | '^' then
            Pos := Pos + 1;
            Result (Pos) := '\';
         end if;
         Pos := Pos + 1;
         Result (Pos) := Pattern (I);
      end loop;
      Pos := Pos + 1;
      Result (Pos) := '$';
      return Result (1 .. Pos);
   end Make_Regexp;

   --  ------------------------------
   --  Build a regular expression pattern from a pattern string.
   --  ------------------------------
   function Make_Regexp (Pattern : in String) return GNAT.Regpat.Pattern_Matcher is
      Expr   : constant String := Make_Regexp (Pattern);
   begin
      return GNAT.Regpat.Compile (Expr);
   end Make_Regexp;

   --  ------------------------------
   --  Scan the directory whose root path is <b>Path</b> and with the relative path
   --  <b>Rel_Path</b> and build in <b>Dir</b> the list of files and directories.
   --  ------------------------------
   procedure Scan (Path     : in String;
                   Rel_Path : in String;
                   Dir      : in Directory_List_Access) is

      use Ada.Directories;

      Full_Path : constant String := Util.Files.Compose (Path, Rel_Path);

      Filter  : constant Filter_Type := (Ordinary_File => True,
                                         Directory     => True,
                                         others        => False);
      Ent     : Ada.Directories.Directory_Entry_Type;
      Search  : Search_Type;
   begin
      Log.Debug ("Scanning {0}", Full_Path);

      Start_Search (Search, Directory => Full_Path,
                    Pattern => "*", Filter => Filter);
      while More_Entries (Search) loop
         Get_Next_Entry (Search, Ent);
         declare
            Name      : constant String := Simple_Name (Ent);
            File_Path : constant String := Util.Files.Compose (Rel_Path, Name);
            Full_Path : constant String := Ada.Directories.Full_Name (Ent);
         begin
            if Are.Utils.Is_File_Ignored (Name) then
               Log.Debug ("Ignoring {0}", Name);

            --  If this is a directory, recursively scan it and collect its files.
            elsif Ada.Directories.Kind (Full_Path) = Ada.Directories.Directory then
               declare
                  Sub_Dir : constant Directory_List_Access
                    := new Directory_List '(Length      => Name'Length,
                                            Path_Length => Full_Path'Length,
                                            Rel_Pos     => Full_Path'Length - File_Path'Length + 1,
                                            Name        => Name,
                                            Path        => Full_Path,
                                            others      => <>);
               begin
                  Dir.Directories.Append (Sub_Dir);
                  Scan (Path, File_Path, Sub_Dir);
               end;
            else
               Log.Debug ("Collect {0}", File_Path);

               Dir.Files.Append (File_Record '(Length => Name'Length,
                                               Name   => Name,
                                               Dir    => Dir));
            end if;
         end;
      end loop;
   end Scan;

   procedure Execute (Rule    : in out Distrib_Rule;
                      Context : in out Context_Type'Class) is
      use Ada.Containers;

      procedure Process (Key : in String;
                         Files : in out File_Vector);

      procedure Process (Key   : in String;
                         Files : in out File_Vector) is
      begin
         Distrib_Rule'Class (Rule).Install (Key, Files, Context);

      exception
         when Ex : others =>
            Context.Error ("install of {0} failed: {1}",
                           Key, Ada.Exceptions.Exception_Message (Ex));
      end Process;

      Iter  : File_Tree.Cursor := Rule.Files.First;
      Count : constant Count_Type := Rule.Files.Length;
      Name  : constant String := Distrib_Rule'Class (Rule).Get_Install_Name;
   begin
      if Count = 0 or else Rule.Resource = null then
         return;
      elsif Count = 1 then
         Log.Info ("Installing 1 file with {0} in {1}", Name,
                   Ada.Strings.Unbounded.To_String (Rule.Resource.Name));
      else
         Log.Info ("Installing{0} files with {1} in {2}", Count_Type'Image (Count),
                   Name, Ada.Strings.Unbounded.To_String (Rule.Resource.Name));
      end if;
      while File_Tree.Has_Element (Iter) loop
         Rule.Files.Update_Element (Iter, Process'Access);
         File_Tree.Next (Iter);
      end loop;
   end Execute;

   --  ------------------------------
   --  Strip the base part of the path
   --  ------------------------------
   function Get_Strip_Path (Base : in String;
                            Path : in String) return String is
   begin
      if Base /= "." and then Path'Length >= Base'Length
        and then Path (Path'First .. Path'First + Base'Length - 1) = Base
      then
         return Path (Path'First + Base'Length + 1 .. Path'Last);
      else
         return Path;
      end if;
   end Get_Strip_Path;

   --  ------------------------------
   --  Get the target path associate with the given source file for the distribution rule.
   --  ------------------------------
   function Get_Target_Path (Rule : in Distrib_Rule;
                             Base : in String;
                             File : in File_Record) return String is
      Rel_Path : constant String := Get_Relative_Path (File.Dir.all);
      Path     : constant String := Get_Strip_Path (Base, Rel_Path);
   begin
      return Util.Files.Compose (Ada.Strings.Unbounded.To_String (Rule.Dir),
                                 Util.Files.Compose (Path, File.Name));
   end Get_Target_Path;

   --  ------------------------------
   --  Get the source path of the file.
   --  ------------------------------
   function Get_Source_Path (Rule : in Distrib_Rule;
                             File : in File_Record) return String is
      pragma Unreferenced (Rule);
   begin
      return Util.Files.Compose (File.Dir.Path, File.Name);
   end Get_Source_Path;

   --  ------------------------------
   --  Get the path that must be exported by the rule.
   --  ------------------------------
   function Get_Export_Path (Rule : in Distrib_Rule;
                             Path : in String) return String is
   begin
      if not Rule.Strip_Extension then
         return Path;
      end if;
      declare
         Pos : constant Natural := Util.Strings.Rindex (Path, '.');
      begin
         if Pos = 0 then
            return Path;
         else
            return Path (Path'First .. Pos - 1);
         end if;
      end;
   end Get_Export_Path;

   --  ------------------------------
   --  Add the file to be processed by the distribution rule.  The file has a relative
   --  path represented by <b>Path</b>.  The path is relative from the base directory
   --  specified in <b>Base_Dir</b>.
   --  ------------------------------
   procedure Add_Source_File (Rule     : in out Distrib_Rule;
                              Path     : in String;
                              File     : in File_Record) is
      procedure Add_File (Key  : in String;
                          Info : in out File_Vector);

      procedure Add_File (Key  : in String;
                          Info : in out File_Vector) is
         pragma Unreferenced (Key);
      begin
         Info.Append (File);
      end Add_File;

      Target_Path : constant String := Distrib_Rule'Class (Rule).Get_Target_Path (Path, File);
      Pos         : constant File_Tree.Cursor := Rule.Files.Find (Target_Path);
   begin
      Log.Debug ("Adding {0} - {1}", Path, Target_Path);

      if File_Tree.Has_Element (Pos) then
         Rule.Files.Update_Element (Pos, Add_File'Access);
      else
         declare
            Info : File_Vector;
         begin
            Info.Append (File);
            Rule.Files.Insert (Target_Path, Info);
         end;
      end if;
   end Add_Source_File;

   --  ------------------------------
   --  Remove the file to be processed by the distribution rule.  This is the opposite of
   --  <tt>Add_Source_File</tt> and used for the <exclude name="xxx"/> rules.
   --  ------------------------------
   procedure Remove_Source_File (Rule     : in out Distrib_Rule;
                                 Path     : in String;
                                 File     : in File_Record) is
      procedure Remove_File (Key  : in String;
                             Info : in out File_Vector);

      Target_Path : constant String := Distrib_Rule'Class (Rule).Get_Target_Path (Path, File);
      Need_Remove : Boolean := False;

      procedure Remove_File (Key  : in String;
                             Info : in out File_Vector) is
         pragma Unreferenced (Key);
         Pos : File_Cursor := Info.Find (File);
      begin
         if File_Record_Vectors.Has_Element (Pos) then
            Log.Debug ("Excluding {0} - {1}", Path, Target_Path);

            Info.Delete (Pos);
            Need_Remove := Info.Is_Empty;
         end if;
      end Remove_File;

      Pos : File_Tree.Cursor := Rule.Files.Find (Target_Path);
   begin
      if File_Tree.Has_Element (Pos) then
         Rule.Files.Update_Element (Pos, Remove_File'Access);
         if Need_Remove then
            Rule.Files.Delete (Pos);
         end if;
      end if;
   end Remove_Source_File;

   --  ------------------------------
   --  Load and add the file in the resource library.
   --  ------------------------------
   procedure Add_File (Rule : in Distrib_Rule;
                       Name : in String;
                       Path : in String;
                       Modtime  : in Ada.Calendar.Time;
                       Override : in Boolean := False) is
      Export_Path : constant String := Rule.Get_Export_Path (Name);
   begin
      if Rule.Source_Timestamp then
         Rule.Resource.Add_File (Export_Path, Path, Modtime, Override);
      else
         Rule.Resource.Add_File (Export_Path, Path, Override);
      end if;
   end Add_File;

   --  ------------------------------
   --  Scan the directory tree whose root is defined by <b>Dir</b> and find the files
   --  that match the current rule.
   --  ------------------------------
   procedure Scan (Rule : in out Distrib_Rule;
                   Dir  : in Directory_List) is
      procedure Scan_Pattern (Pos : in Match_Rule_Vector.Cursor);

      Exclude : Boolean;

      procedure Scan_Pattern (Pos : in Match_Rule_Vector.Cursor) is
         Match   : constant Match_Rule := Match_Rule_Vector.Element (Pos);
         Base    : constant String := Ada.Strings.Unbounded.To_String (Match.Base_Dir);
         Pattern : constant String := Ada.Strings.Unbounded.To_String (Match.Match);
      begin
         Log.Debug ("Scan pattern base {0} - pat {1}", Base, Pattern);
         if Base = "" then
            Rule.Scan (Dir, ".", Pattern, Exclude);
            return;
         end if;
         declare
            Iter    : Directory_List_Vector.Cursor := Dir.Directories.First;
            D       : Directory_List_Access;
            P       : Natural := Base'First;
            N       : Natural;
         begin
            while P < Base'Last loop
               N := Util.Strings.Index (Base, '/', P);
               if N = 0 then
                  N := Base'Last;
               else
                  N := N - 1;
               end if;

               while Directory_List_Vector.Has_Element (Iter) loop
                  D := Directory_List_Vector.Element (Iter);
                  if D.Name = Base (P .. N) then
                     if N = Base'Last then
                        Log.Debug ("Scanning from sub directory at {0}", Base);
                        Rule.Scan (D.all, Base, Pattern, Exclude);
                        return;
                     end if;
                     Iter := D.Directories.First;
                     exit;
                  end if;
                  Directory_List_Vector.Next (Iter);
               end loop;
               P := N + 2;
            end loop;
         end;
      end Scan_Pattern;

   begin
      Exclude := False;
      Rule.Matches.Iterate (Scan_Pattern'Access);
      Exclude := True;
      Rule.Excludes.Iterate (Scan_Pattern'Access);
   end Scan;

   procedure Scan (Rule     : in out Distrib_Rule;
                   Dir      : in Directory_List;
                   Base_Dir : in String;
                   Pattern  : in String;
                   Exclude  : in Boolean) is

      procedure Collect_Subdirs (Name_Pattern : in String);
      procedure Collect_Files (Name_Pattern : in String);

      --  **/*.xhtml
      --  bin/**
      --  bin/**/test.bin
      N   : constant Natural := Util.Strings.Index (Pattern, '/');
      Pos : Natural := Pattern'First;

      procedure Collect_Files (Name_Pattern : in String) is
         use GNAT.Regpat;

         procedure Collect_File (File : in File_Record);

         Matcher : constant Pattern_Matcher := Make_Regexp (Name_Pattern);

         procedure Collect_File (File : in File_Record) is
         begin
            Log.Debug ("Check {0} - {1}", Base_Dir, File.Name);

            if Match (Matcher, File.Name) then
               if Exclude then
                  Rule.Remove_Source_File (Base_Dir, File);
               else
                  Rule.Add_Source_File (Base_Dir, File);
               end if;
            end if;
         end Collect_File;

         Iter : File_Record_Vectors.Cursor := Dir.Files.First;
      begin
         while File_Record_Vectors.Has_Element (Iter) loop
            File_Record_Vectors.Query_Element (Iter, Collect_File'Access);
            File_Record_Vectors.Next (Iter);
         end loop;
      end Collect_Files;

      procedure Collect_Subdirs (Name_Pattern : in String) is
         procedure Collect_Dir (Sub_Dir : in Directory_List_Access);

         procedure Collect_Dir (Sub_Dir : in Directory_List_Access) is
         begin
            if Name_Pattern = Sub_Dir.Name or else Name_Pattern = "*" then
               Rule.Scan (Sub_Dir.all, Base_Dir,
                          Pattern (Pos .. Pattern'Last), Exclude);
            end if;
         end Collect_Dir;

         Iter : Directory_List_Vector.Cursor := Dir.Directories.First;
      begin
         while Directory_List_Vector.Has_Element (Iter) loop
            Directory_List_Vector.Query_Element (Iter, Collect_Dir'Access);
            Directory_List_Vector.Next (Iter);
         end loop;
      end Collect_Subdirs;

      Next : Natural;
   begin
      Log.Debug ("Scan {0}/{1} for pattern {2}", Base_Dir, Dir.Name, Pattern);

      if N > 0 then
         if Pattern = "**" then
            Collect_Subdirs (Name_Pattern => "**");
            Collect_Files (Name_Pattern => "*");
            return;

         elsif Pattern (Pattern'First .. N) = "*/" then
            Pos := N + 1;
            Collect_Subdirs (Name_Pattern => "*");

         elsif Pattern (Pattern'First .. N) = "**/" then
            Collect_Subdirs (Name_Pattern => "*");

         else
            Pos := N + 1;
            Collect_Subdirs (Name_Pattern => Pattern (Pattern'First .. N - 1));
            return;
         end if;
         Next := Util.Strings.Index (Pattern, '/', N + 1);
         if Next = 0 then
            Collect_Files (Name_Pattern => Pattern (N + 1 .. Pattern'Last));
         end if;
      end if;
      if N = 0 then
         --  No more directory
         Collect_Files (Name_Pattern => Pattern);
      end if;
   end Scan;

   procedure Delete (Directory : in out Directory_List_Access) is
      procedure Free is
        new Ada.Unchecked_Deallocation (Object => Directory_List,
                                        Name   => Directory_List_Access);
   begin
      while not Directory.Directories.Is_Empty loop
         declare
            Child : Directory_List_Access := Directory.Directories.First_Element;
         begin
            Directory.Directories.Delete_First;
            Delete (Child);
         end;
      end loop;
      Free (Directory);
   end Delete;

   --  ------------------------------
   --  Clear the rules and files that have been loaded.
   --  ------------------------------
   procedure Clear (Installer : in out Installer_Type) is
      procedure Free is
        new Ada.Unchecked_Deallocation (Object => Distrib_Rule'Class,
                                        Name   => Distrib_Rule_Access);
   begin
      --  Release the rules until the list becomes empty.
      while not Installer.Rules.Is_Empty loop
         declare
            Rule : Distrib_Rule_Access := Installer.Rules.First_Element;
         begin
            Installer.Rules.Delete_First;
            Free (Rule);
         end;
      end loop;

      --  Likewise for directories.
      while not Installer.Trees.Is_Empty loop
         declare
            Dir : Directory_List_Access := Installer.Trees.First_Element;
         begin
            Installer.Trees.Delete_First;
            Delete (Dir);
         end;
      end loop;
   end Clear;

   overriding
   procedure Finalize (Installer : in out Installer_Type) is
   begin
      Installer.Clear;
   end Finalize;

end Are.Installer;
