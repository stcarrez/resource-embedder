with Ada.Streams;
with Interfaces.C;
package Resources is
   type Content_Access is access constant String;

   type Name_Access is access constant String;
   
   type Name_Array is array (Natural range <>) of Name_Access;

   type Format_Type is (FILE_RAW, FILE_GZIP);

   type Content_Type is record
      Name    : Name_Access;
      Content : Content_Access;
      Modtime : Interfaces.C.long := 0;
      Format  : Format_Type := FILE_RAW;
   end record;

   Null_Content : constant Content_Type;

private
   Null_Content : constant Content_Type := (others => <>);
end Resources;