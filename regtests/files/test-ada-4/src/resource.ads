with Ada.Streams;
package Resource is

   type Content_Access is access constant Ada.Streams.Stream_Element_Array;

end Resource;
