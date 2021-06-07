# Embedding web pages in C

This example shows how to you can embed web pages in a C program
and how to can make some transformations before their inclusion.
This example uses a `package.xml` definition to
describe the files and the methods to collect and embed the files.

The configuration files are embedded by using the following
command:

```
are --lang=c -o src --rule=package.xml --list-access --name-access  .
```

The `--lang=c` option selects the C generator for the output and the
`-o src` option indicates the target directory where files are generated.

The `--rule=package.xml` option indicates to read the `package.xml` file
which describes the rules to collect the resource and embed them.

The `--list-access` option tells the generator to emit a declaration
of a sorted C array of names allowing to access the resource names.

The `--name-access` option tells the code generator to emit a C function that allows to retrieve
the resource by using its name.  To describe the content that is embedded,
a C structure is declared that gives information about the raw data content,
the content size, the modification date and data format.
The header file will declare the following structure and function declaration:

The `package.xml` file describes a list of resources with their content.
The root XML element is `package` and each resource is described by a `resource`
XML element.  The resource is assigned a name that will be used to prefix the
C function and C type declaration and also choose the file name for the generation.

The following resource definition declares the `web` resource.  It contains
an installation rule that will copy the files under the `web` directory
in the resource set.  Files will be passed through the `xmllint` utility
and it will first validate the XML and remove unecessary blanks.

```XML
<package>
  <resource name='web'
            type="web_content"
            function-name="web_get_content">
    <install mode='exec'>
      <command output='#{dst}'>xmllint --noblanks #{src}</command>
      <fileset dir="web">
        <include name="**/*.xhtml"/>
        <exclude name="WEB-INF/layouts/**"/>
      </fileset>
    </install>
  ...
</package>
```

The next resource definition is more complex and uses the `webmerge` rule to
merge Javascript and CSS files.  The source files that match the fileset
are read and analyzed.  Some patterns are recognized and replaced by a merged
content.

```XML
<package>
   ...
  <resource name='web'
            type="web_content"
            function-name="web_get_content">
    ...
    <install mode='webmerge' log='info' source-timestamp='yes'
             merge-start='RESOURCE-MERGE-START'
             merge-end='RESOURCE-MERGE-END'>
       <!-- Define transformation variables: the #{name} are replaced by the given value -->
       <property name="contextPath"></property>
       <property name="jquery.path">/js/jquery-3.4.1.js</property>
       <property name="jquery.uiCssPath">/css/redmond/jquery-ui-1.12.1.css</property>
       <property name="jquery.chosenCssPath">/css/jquery-chosen-1.8.7/chosen.css</property>
       <property name="jquery.uiPath">/js/jquery-ui-1.12.1/</property>

       <!-- When CSS is merged, the relative path can change: do some URL transformation
            so that the relative path remains valid -->
       <replace>
         <from>url("images/</from>
         <to>url("redmond/images/</to>
       </replace>
       <replace>
         <from>url(../../images/</from>
         <to>url(../images/</to>
       </replace>

       <!-- Files to merge are located in 'WEB-INF/layouts' directory under 'web' -->
       <fileset dir="web">
          <include name="WEB-INF/layouts/*.xhtml"/>
       </fileset>
    </install>
  </resource>
</package>
```

When this rule is executed on the file `web/WEB-INF/layouts/anonymous.xhtml`,
the following XML sequence:

```
    <!-- RESOURCE-MERGE-START link=#{contextPath}/css/merge-1.css -->
    <link media="screen" type="text/css" rel="stylesheet" href="#{contextPath}/css/bootstrap.min.css"/>
    <link media="screen" type="text/css" rel="stylesheet" href="#{contextPath}/css/grids/fluid_grid.css"/>
    <link media="screen" type="text/css" rel="stylesheet" href="#{contextPath}/css/asf.css"/>
    <link media="screen" type="text/css" rel="stylesheet" href="#{contextPath}/css/awa.css"/>
    <link media="screen" type="text/css" rel="stylesheet" href="#{contextPath}/css/users.css"/>
    <link media="screen" type="text/css" rel="stylesheet" href="#{jquery.uiCssPath}"/>
    <link media="screen" type="text/css" rel="stylesheet" href="#{jquery.chosenCssPath}"/>
    <link media="screen" type="text/css" rel="stylesheet" href="#{contextPath}/css/comments.css"/>
    <link media="screen" type="text/css" rel="stylesheet" href="#{contextPath}/css/blogs.css"/>
    <link media="screen" type="text/css" rel="stylesheet" href="#{contextPath}/css/markedit/jquery.markedit.css" />
    <link media="screen" type="text/css" rel="stylesheet" href="#{contextPath}/css/storages.css"/>
    <link media="screen" type="text/css" rel="stylesheet" href="#{contextPath}/css/wikis.css"/>
    <link media="screen" type="text/css" rel="stylesheet" href="#{contextPath}/css/jquery.tagedit.css"/>
    <link media="screen" type="text/css" rel="stylesheet" href="#{contextPath}/css/jquery.datetimepicker.css"/>
    <link media="screen" type="text/css" rel="stylesheet" href="#{contextPath}/css/trumbowyg/trumbowyg.css"/>
    <!-- RESOURCE-MERGE-END -->
```

is replaced by:

```
    <link media='screen' type='text/css' rel='stylesheet' href='#{contextPath}/css/merge-1.css?build=23'/>
```

and a file `css/merge-1.css` is created and contains the files that were replaced.

# Build

Run the command

```
make
```

and this generates:

* src/web.h: header file generated by *are* for the `web` resource,
* src/web.c: embedded documentation file generated by *are* for the `web` resource,
* show-web: binary that prints the web pages (the web server is left as an exercise!)

The program usage is then:

```
show-web
show-web js/merge-1.js
show-web js/merge-2.js
show-web css/merge-1.css
```
