--  Advanced Resource Embedder 1.5.1
with Ada.Streams;
package web is

   type Content_Access is access constant Ada.Streams.Stream_Element_Array;

   --  Returns the data stream with the given name or null.
   function Get_Content (Name : String) return
      web_content;

end web;
