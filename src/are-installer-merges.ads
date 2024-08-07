-----------------------------------------------------------------------
--  are-installer-merges -- Web file merge
--  Copyright (C) 2020, 2021 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with EL.Variables.Default;
with EL.Contexts.Default;
with Util.Strings.Maps;
with Util.Beans.Objects.Maps;

--  == Install mode: webmerge ==
--  The `webmerge` distribution rule is intended to merge Javascript or CSS files
--  which are used by XHTML presentation files.  It requires some help from the
--  developer to describe what files must be merged.  The XHTML file must contain
--  well defined XML comments which are used to identify the merging areas.
--  The CSS file merge start section begins with:
--
--    <!-- ARE-MERGE-START link=#{contextPath}/css/target-merge-1.css -->
--
--  and the Javascript merge start begings with:
--
--    <!-- ARE-MERGE-START script=#{contextPath}/js/target-merge-1.js -->
--
--  The merge section is terminated by the following XML comment:
--
--    <!-- ARE-MERGE-END -->
--
--  Everything withing these XML comments is then replaced either by a `link`
--  HTML tag or by a `script` HTML tag and a file described either by the
--  `link=` or `script=` markers is generated to include every `link` or `script`
--  that was defined within the XML comment markers.  For example, with the following
--  XHTML extract:
--
--    <!-- ARE-MERGE-START link=#{contextPath}/css/merged.css -->
--    <link media="screen" type="text/css" rel="stylesheet"
--          href="#{contextPath}/css/awa.css"/>
--    <link media="screen" type="text/css" rel="stylesheet"
--          href="#{jquery.uiCssPath}"/>
--    <link media="screen" type="text/css" rel="stylesheet"
--          href="#{jquery.chosenCssPath}"/>
--    <!-- ARE-MERGE-END -->
--
--  The generated file `css/merged.css` will include `awa.css`, `jquery-ui-1.12.1.css`,
--  `chosen.css` and the XHTML will be replaced to include `css/merge.css` only
--  by using the following XHTML:
--
--    <link media='screen' type='text/css' rel='stylesheet'
--          href='#{contextPath}/css/merged.css'/>
--
--  To use the `webmerge`, the `package.xml` description file should contain
--  the following command:
--
--    <install mode='webmerge' dir='web' source-timestamp='true'>
--       <property name="contextPath"></property>
--       <property name="jquery.path">/js/jquery-3.4.1.js</property>
--       <property name="jquery.uiCssPath">/css/redmond/jquery-ui-1.12.1.css</property>
--       <property name="jquery.chosenCssPath">/css/jquery-chosen-1.8.7/chosen.css</property>
--       <property name="jquery.uiPath">/js/jquery-ui-1.12.1</property>
--       <fileset dir="web">
--          <include name="WEB-INF/layouts/*.xhtml"/>
--       </fileset>
--    </install>
--
--  The merging areas are identified by the default tags `ARE-MERGE-START` and `ARE-MERGE-END`.
--  These tags can be changed by specifying the expected value in the `merge-start` and `merge-end`
--  attributes in the `install` XML element.  For example, with
--
--    <install mode='webmerge' dir='web' source-timestamp='true'
--             merge-start='RESOURCE-MERGE-START'
--             merge-end='RESOURCE-MERGE-END'>
--
--    </install>
--
--  the markers becomes:
--
--    <!-- RESOURCE-MERGE-START link=#{contextPath}/css/target-merge-1.css -->
--    <!-- RESOURCE-MERGE-END -->
--
private package Are.Installer.Merges is

   DEFAULT_MERGE_START : constant String := "ARE-MERGE-START";
   DEFAULT_MERGE_END   : constant String := "ARE-MERGE-END";

   --  Create a distribution rule to copy a set of files or directories.
   function Create_Rule (Node : in DOM.Core.Node) return Distrib_Rule_Access;

   --  ------------------------------
   --  Distribution artifact
   --  ------------------------------
   type Merge_Rule is new Distrib_Rule with private;
   type Merge_Rule_Access is access all Merge_Rule;

   --  Get a name to qualify the installation rule (used for logs).
   overriding
   function Get_Install_Name (Rule : in Merge_Rule) return String;

   overriding
   procedure Install (Rule    : in Merge_Rule;
                      Path    : in String;
                      Files   : in File_Vector;
                      Context : in out Context_Type'Class);

private

   type Merge_Rule is new Distrib_Rule with record
      Params           : Util.Beans.Objects.Maps.Map_Bean;
      Context          : EL.Contexts.Default.Default_Context;
      Variables        : aliased EL.Variables.Default.Default_Variable_Mapper;
      Replace          : Util.Strings.Maps.Map;
      Start_Mark       : UString;
      End_Mark         : UString;
   end record;

end Are.Installer.Merges;
