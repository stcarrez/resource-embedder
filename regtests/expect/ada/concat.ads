--  Advanced Resource Embedder 1.3.0
with Ada.Streams;
package Concat is

   type Content_Access is access constant Ada.Streams.Stream_Element_Array;

   --  Returns the data stream with the given name or null.
   function Get_Content (Name : String) return
      Content_Access;

end Concat;
