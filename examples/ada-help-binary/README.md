# Embedding help and documentation in Ada (Binary version)

This example is similar to the 
[Embedding help and documentation in Ada](https://github.com/stcarrez/resource-embedder/tree/master/examples/ada-help)
but it uses the binary content type `System.Storage_Elements.Storage_Array`.

The configuration files are embedded by using the following
command:

```
are --lang=Ada -o src --rule=package.xml \
    --list-access --content-only --name-access .
```

The main differences with the 
[Embedding help and documentation in Ada](https://github.com/stcarrez/resource-embedder/tree/master/examples/ada-help)
version is the declaration of the package.xml file which defines the `type` returned by the
generated `Get_Content` function:

```XML
<package>
  <resource name='Resources.Help'
            format='binary'
            type='access constant System.Storage_Elements.Storage_Array'>
    <install mode='copy'>
      <fileset dir="help">
        <include name="**/*.txt"/>
      </fileset>
    </install>
  </resource>
  ...
</package>
```

With the above description, the Ada code generator produces the
following package specification:

```Ada
with System.Storage_Elements;
package Resources.Help is
   type Content_Access is access constant System.Storage_Elements.Storage_Array;
   type Name_Access is access constant String;
   type Name_Array is array (Natural range <>) of Name_Access;
   Names : constant Name_Array;
   function Get_Content (Name : String)
      access constant System.Storage_Elements.Storage_Array;
private
   ...
end Resources.Help;
```

# Build

Run the command

```
make
```

and this generates:

* src/resources-help.ads: specification file for the `help` resources,
* src/resources-help.adb: body file for the `help` resources,
* src/resources-man.ads: specification file for the `man` resources,
* src/resources-man.adb: body file for the `man` resources,
* show_help: binary that prints the documentation

The program usage is then:

```
show_help
show_help ls
show_help edit
```

