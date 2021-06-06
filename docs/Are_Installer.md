# Rules
The `Advanced Resource Embedder` provides several mechanisms to integrate
files in the generated code.

An XML file file contains a set of rules which describe how to select the
files to include in the generator.  The XML file is read and resource rules
introduced by the `resource` XML element are collected.

The source paths are then scanned and a complete tree of source files is created.
Because several source paths are given, we have several source trees with possibly
duplicate files and names in them.

The source paths are matched against the resource rules and each installation rule
is filled with the source files that they match.

The resource installation rules are executed in the order defined
in the `package.xml` file.  Each resource rule can have its own way to make
the installation for the set of files that matched the rule definition.
A resource rule can copy the file, another can concatenate the source files,
another can do some transformation on the source files and prepare it before being
embedded and used by the generator.

## Install mode: copy and copy-first
The `copy` and `copy-first` mode are the simpler distribution rules that
the following XML definition:

```Ada
<install mode='copy'>
  <include name="*.txt"/>
</install>
```

If the tool is called with several directories that contain a same file name
then the `copy` installer will complain because it has two source files for
a same destination name.  When this happens, you may instead use the `copy-first`
mode which will take into account only the first found in the first directory.

## Install mode: concat
The `concat` mode provides a distribution rule that concatenates a list of
files.  The rule is created by using the following XML definition:

```Ada
<install mode='concat' source-timestamp='yes'>
  <include name="NOTICE.txt"/>
</install>
```

This rule is useful when the tool is invoked with several directories that
contain files with identical names.  Unlike the `copy` and `copy-first`
rules that take into account only one source file, the `concat` mode handles
this situation by concatenatating the source files.

By default the generated file has a timestamp which correspond to the time
when the `are` command is executed.  By setting the `source-timestamp`
attribute to `true`, the generated file is assigned the timestamp of the
newest file in the source files.

## Install mode: exec
The `exec` mode is the most powerful installation rule since it allows
to execute a command on the source file.
to copy a file or a directory by using an external program.  The rule is
created by using the following XML definition:

```Ada
<install mode='exec' dir='target' source-timestamp='true'>
  <command slow='false' output='...'>cmd #{src} #{dst}</command>
  <fileset dir="source">
    <include name="**/*"/>
  </fileset>
</install>
```

The command is a string which can contain EL expressions that are
evaluated before executing the command.  The command is executed for
each source file.  The following EL variables are defined:

```Ada
src   defines the absolute source path
dst   defines the target destination path
name  defines the relative source name (ie, the name of the resource file)
```
## Install mode: bundles
The `Are.Installer.Bundles` package provides distribution rules
to merge a list of bundles to the distribution area.  The rule is
created by using the following XML definition:

```Ada
<install mode='bundles' source-timestamp='true'>
  <fileset dir='bundles'>
    <include name="**/*.properties"/>
  </fileset>
</install>
```
## Install mode: webmerge
The `webmerge` distribution rule is intended to merge Javascript or CSS files
which are used by XHTML presentation files.  It requires some help from the
developer to describe what files must be merged.  The XHTML file must contain
well defined XML comments which are used to identify the merging areas.
The CSS file merge start section begins with:

```Ada
<!-- ARE-MERGE-START link=#{contextPath}/css/target-merge-1.css -->
```

and the Javascript merge start begings with:

```Ada
<!-- ARE-MERGE-START script=#{contextPath}/js/target-merge-1.js -->
```

The merge section is terminated by the following XML comment:

```Ada
<!-- ARE-MERGE-END -->
```

Everything withing these XML comments is then replaced either by a `link`
HTML tag or by a `script` HTML tag and a file described either by the
`link=` or `script=` markers is generated to include every `link` or `script`
that was defined within the XML comment markers.  For example, with the following
XHTML extract:

```Ada
<!-- ARE-MERGE-START link=#{contextPath}/css/merged.css -->
<link media="screen" type="text/css" rel="stylesheet" href="#{contextPath}/css/awa.css"/>
<link media="screen" type="text/css" rel="stylesheet" href="#{jquery.uiCssPath}"/>
<link media="screen" type="text/css" rel="stylesheet" href="#{jquery.chosenCssPath}"/>
<!-- ARE-MERGE-END -->
```

The generated file `css/merged.css` will include `awa.css`, `jquery-ui-1.12.1.css`,
`chosen.css` and the XHTML will be replaced to include `css/merge.css` only
by using the following XHTML:

```Ada
<link media='screen' type='text/css' rel='stylesheet' href='#{contextPath}/css/merged.css'/>
```

To use the `webmerge`, the `package.xml` description file should contain
the following command:

```Ada
<install mode='merge' dir='web' source-timestamp='true'>
   <property name="contextPath"></property>
   <property name="jquery.path">/js/jquery-3.4.1.js</property>
   <property name="jquery.uiCssPath">/css/redmond/jquery-ui-1.12.1.css</property>
   <property name="jquery.chosenCssPath">/css/jquery-chosen-1.8.7/chosen.css</property>
   <property name="jquery.uiPath">/js/jquery-ui-1.12.1</property>
   <fileset dir="web">
      <include name="WEB-INF/layouts/*.xhtml"/>
   </fileset>
</install>
```

