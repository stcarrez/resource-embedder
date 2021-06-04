-- Advanced Resource Embedder
with Interfaces; use Interfaces;

package body config is
   function Hash (S : String) return Natural;

   P : constant array (0 .. 0) of Natural :=
     (0 .. 0 => 1);

   T1 : constant array (0 .. 0) of Unsigned_8 :=
     (0 .. 0 => 1);

   T2 : constant array (0 .. 0) of Unsigned_8 :=
     (0 .. 0 => 0);

   G : constant array (0 .. 2) of Unsigned_8 :=
     (0, 0, 0);

   function Hash (S : String) return Natural is
      F : constant Natural := S'First - 1;
      L : constant Natural := S'Length;
      F1, F2 : Natural := 0;
      J : Natural;
   begin
      for K in P'Range loop
         exit when L < P (K);
         J  := Character'Pos (S (P (K) + F));
         F1 := (F1 + Natural (T1 (K)) * J) mod 3;
         F2 := (F2 + Natural (T2 (K)) * J) mod 3;
      end loop;
      return (Natural (G (F1)) + Natural (G (F2))) mod 1;
   end Hash;

   C_0 : aliased constant Ada.Streams.Stream_Element_Array :=
     (35, 32, 69, 120, 97, 109, 112, 108, 101, 32, 111, 102, 32, 101, 109, 98, 101, 100, 100, 101, 100,
      32, 99, 111, 110, 102, 105, 103, 117, 114, 97, 116, 105, 111, 110, 32, 102, 105, 108, 101, 10,
      100, 97, 116, 97, 98, 97, 115, 101, 32, 61, 32, 109, 121, 115, 113, 108, 58, 47, 47, 108,
      111, 99, 97, 108, 104, 111, 115, 116, 58, 51, 51, 48, 54, 47, 97, 100, 111, 95, 116, 101,
      115, 116, 63, 117, 115, 101, 114, 61, 97, 100, 111, 38, 101, 110, 99, 111, 100, 105, 110, 103,
      61, 117, 116, 102, 56, 10, 10, 115, 111, 99, 107, 101, 116, 32, 61, 32, 47, 118, 97, 114,
      47, 114, 117, 110, 47, 109, 121, 115, 113, 108, 100, 47, 109, 121, 115, 113, 108, 100, 46, 115,
      111, 99, 107, 10, 10, 100, 101, 102, 97, 117, 108, 116, 45, 99, 104, 97, 114, 97, 99, 116,
      101, 114, 45, 115, 101, 116, 32, 61, 32, 117, 116, 102, 56, 109, 98, 52, 10);



   type Name_Access is access constant String;
   type Keyword_Array is array (Natural range <>) of Name_Access;

   K_0             : aliased constant String := "example.conf";

   Keywords : constant Keyword_Array := (
      0 => K_0'Access);

   type Content_Array is array (Natural range <>) of Content_Access;
   Contents : constant Content_Array := (
     0 =>  C_0'Access);

   --  Returns the data stream with the given name or null.
   function Get_Content (Name : String) return Content_Access is
      H : constant Natural := Hash (Name);
   begin
      return (if Keywords (H).all = Name then Contents (H) else null);
   end Get_Content;

end config;
