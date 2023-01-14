-----------------------------------------------------------------------
--  are-utils -- Utilities for model generator
--  Copyright (C) 2010, 2011, 2012, 2015, 2021, 2023 Stephane Carrez
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

with DOM.Core.Nodes;
with DOM.Core.Elements;
with DOM.Core.Character_Datas;

with Util.Strings;

package body Are.Utils is

   --  ------------------------------
   --  Generic procedure to iterate over the DOM nodes children of <b>node</b>
   --  and having the entity name <b>name</b>.
   --  ------------------------------
   procedure Iterate_Nodes (Closure : in out T;
                            Node    : in DOM.Core.Node;
                            Name    : in String;
                            Recurse : in Boolean := True) is
   begin
      if Recurse then
         declare
            Nodes : DOM.Core.Node_List := DOM.Core.Elements.Get_Elements_By_Tag_Name (Node, Name);
            Size  : constant Natural := DOM.Core.Nodes.Length (Nodes);
         begin
            for I in 0 .. Size - 1 loop
               declare
                  N : constant DOM.Core.Node := DOM.Core.Nodes.Item (Nodes, I);
               begin
                  Process (Closure, N);
               end;
            end loop;
            DOM.Core.Free (Nodes);

         end;
      else
         declare
            use type DOM.Core.Node_Types;

            Nodes : constant DOM.Core.Node_List := DOM.Core.Nodes.Child_Nodes (Node);
            Size  : constant Natural := DOM.Core.Nodes.Length (Nodes);
         begin
            for I in 0 .. Size - 1 loop
               declare
                  N : constant DOM.Core.Node := DOM.Core.Nodes.Item (Nodes, I);
                  T : constant DOM.Core.Node_Types := DOM.Core.Nodes.Node_Type (N);
               begin
                  if T = DOM.Core.Element_Node and then Name = DOM.Core.Nodes.Node_Name (N) then
                     Process (Closure, N);
                  end if;
               end;
            end loop;
         end;
      end if;
   end Iterate_Nodes;

   --  ------------------------------
   --  Get the first DOM child from the given entity tag
   --  ------------------------------
   function Get_Child (Node : DOM.Core.Node;
                       Name : String) return DOM.Core.Node is
      Nodes : DOM.Core.Node_List :=
        DOM.Core.Elements.Get_Elements_By_Tag_Name (Node, Name);
   begin
      if DOM.Core.Nodes.Length (Nodes) = 0 then
         DOM.Core.Free (Nodes);
         return null;
      else
         return Result : constant DOM.Core.Node := DOM.Core.Nodes.Item (Nodes, 0) do
            DOM.Core.Free (Nodes);
         end return;
      end if;
   end Get_Child;

   --  ------------------------------
   --  Get the content of the node
   --  ------------------------------
   function Get_Data_Content (Node : in DOM.Core.Node) return String is
      Nodes  : constant DOM.Core.Node_List := DOM.Core.Nodes.Child_Nodes (Node);
      S      : constant Natural   := DOM.Core.Nodes.Length (Nodes);
      Result : Unbounded_String;
   begin
      for J in 0 .. S - 1 loop
         Append (Result, DOM.Core.Character_Datas.Data (DOM.Core.Nodes.Item (Nodes, J)));
      end loop;
      return To_String (Result);
   end Get_Data_Content;

   --  ------------------------------
   --  Get the content of the node identified by <b>Name</b> under the given DOM node.
   --  ------------------------------
   function Get_Data_Content (Node : in DOM.Core.Node;
                              Name : in String) return String is
      use type DOM.Core.Node;

      N    : constant DOM.Core.Node := Get_Child (Node, Name);
   begin
      if N = null then
         return "";
      else
         return Are.Utils.Get_Data_Content (N);
      end if;
   end Get_Data_Content;

   --  ------------------------------
   --  Get a boolean attribute
   --  ------------------------------
   function Get_Attribute (Node    : in DOM.Core.Node;
                           Name    : in String;
                           Default : in Boolean := False) return Boolean is
      V : constant DOM.Core.DOM_String := DOM.Core.Elements.Get_Attribute (Node, Name);
   begin
      if V in "yes" | "true" then
         return True;
      elsif V in "no" | "false" then
         return False;
      else
         return Default;
      end if;
   end Get_Attribute;

   --  ------------------------------
   --  Get a string attribute
   --  ------------------------------
   function Get_Attribute (Node    : in DOM.Core.Node;
                           Name    : in String;
                           Default : in String := "") return String is
      V : constant DOM.Core.DOM_String := DOM.Core.Elements.Get_Attribute (Node, Name);
   begin
      if V = "" then
         return Default;
      else
         return V;
      end if;
   end Get_Attribute;

   --  ------------------------------
   --  Returns True if the file name must be ignored (.svn, CVS, .git, are ignored).
   --  ------------------------------
   function Is_File_Ignored (Name : in String) return Boolean is
   begin
      if Name =  ".svn" then
         return True;
      elsif Name = ".git" then
         return True;
      elsif Name = "CVS" then
         return True;
      elsif Name = "." or else Name = ".." then
         return True;
      elsif Name'Length = 0 then
         return True;
      elsif Name (Name'Last) = '~' then
         return True;
      elsif Name (Name'First) = '#' and then Name (Name'Last) = '#' then
         return True;
      else
         return False;
      end if;
   end Is_File_Ignored;

   --  ------------------------------
   --  Get a string attribute
   --  ------------------------------
   function Get_Attribute (Node    : DOM.Core.Node;
                           Name    : String;
                           Default : String := "") return Ada.Strings.Unbounded.Unbounded_String is
      V : constant DOM.Core.DOM_String := DOM.Core.Elements.Get_Attribute (Node, Name);
   begin
      if V = "" then
         return To_Unbounded_String (Default);
      else
         return To_Unbounded_String (V);
      end if;
   end Get_Attribute;

end Are.Utils;
