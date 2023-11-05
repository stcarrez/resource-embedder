# Generating a compiled time key/value mapping table in Ada

This example shows how to get a static mapping table that converts
a key string into a value string.  The mapping table is defined in
a JSON file which contains a list of key/value pairs.  The resource
embedder reads the JSON file and generates a static mapping table
with a `Get_Mapping` function.  The function uses a perfect hash
generator built from all the keys defined in the JSON file.

The Advance Resource Embedder scans the directory and prepares the
SQLite scripts by stipping any comment, splitting the SQL statements
and preparing the result as an array of strings.

The JSON mapping table is created by using the following
command:

```
are --lang=Ada -o src --rule=package.xml .
```

The `--lang=Ada` option selects the Ada generator for the output and the
`-o src` option indicates the target directory where files are generated.

The `--rule=package.xml` option indicates to read the `package.xml` file
which describes the rules to collect the resource and embed them.

The `package.xml` file describes a list of resources with their content.
The root XML element is `package` and each resource is described by a `resource`
XML element.  The resource is assigned a name that will be used for the Ada
package generation.  Each resource is generated in its separate package.

The following resource definition declares the `Extensions_Map` resource.
It contains an installation rule that will copy the files under the current
directory in the resource set.  Only files matching the `*.json` pattern will
be taken into account.  The `format` attribute specifies the `map` type which
activates the generation of the mapping table.  The mapper is configured
by using the `mapper` element and the `json` type indicates to use the JSON
parser to read the key/value paires.

```XML
<package>
  <resource name='Extensions_Map'
            format='map'
            type='access constant String'>
    <mapper type="json"/>

    <install mode='copy' strip-extension='yes'>
      <fileset dir=".">
        <include name="**/*.json"/>
      </fileset>
    </install>
  </resource>
</package>
```

With the above description, the Ada code generator produces the
following package specification:

```Ada
package Extensions_Map is
   function Get_Mapping (Name : String) return
      access constant String;
end Extensions_Map;
```


# Build

Run the command

```
make
```

and this generates:

* src/extensions_map.ads: specification file for the `extensions` resources,
* extension: binary that identifies a file based on the extension

The program usage is then:

```
./extension test.adb test.c test.py document.tex
```

