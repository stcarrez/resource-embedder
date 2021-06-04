-----------------------------------------------------------------------
--  are-utils -- Utilities for model generator
--  Copyright (C) 2010, 2011, 2012, 2021 Stephane Carrez
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
with Ada.Strings.Unbounded;
with Ada.Containers.Ordered_Sets;

with DOM.Core;
with Util.Strings.Vectors;
package Are.Utils is

   --  Generic procedure to iterate over the DOM nodes children of <b>node</b>
   --  and having the entity name <b>name</b>.
   generic
      type T (<>) is limited private;
      with procedure Process (Closure : in out T;
                              Node    : DOM.Core.Node);
   procedure Iterate_Nodes (Closure : in out T;
                            Node    : in DOM.Core.Node;
                            Name    : in String;
                            Recurse : in Boolean := True);

   --  Get the first DOM child from the given entity tag
   function Get_Child (Node : in DOM.Core.Node;
                       Name : in String) return DOM.Core.Node;

   --  Get the content of the node
   function Get_Data_Content (Node : in DOM.Core.Node) return String;

   --  Get the content of the node identified by <b>Name</b> under the given DOM node.
   function Get_Data_Content (Node : in DOM.Core.Node;
                              Name : in String) return String;

   --  Get a boolean attribute
   function Get_Attribute (Node    : in DOM.Core.Node;
                           Name    : in String;
                           Default : in Boolean := False) return Boolean;

   --  Get a string attribute
   function Get_Attribute (Node    : in DOM.Core.Node;
                           Name    : in String;
                           Default : in String := "") return String;

   use Ada.Strings.Unbounded;
   package String_Set is
     new Ada.Containers.Ordered_Sets (Element_Type => Ada.Strings.Unbounded.Unbounded_String,
                                      "<"          => Ada.Strings.Unbounded."<",
                                      "="          => Ada.Strings.Unbounded."=");

   package String_List renames Util.Strings.Vectors;

   --  Returns True if the file name must be ignored (.svn, CVS, .git, are ignored).
   function Is_File_Ignored (Name : in String) return Boolean;

   --  Get a string attribute
   function Get_Attribute (Node    : in DOM.Core.Node;
                           Name    : in String;
                           Default : in String := "")
                           return Ada.Strings.Unbounded.Unbounded_String;

end Are.Utils;
