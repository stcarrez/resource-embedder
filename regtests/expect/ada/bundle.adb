-- Advanced Resource Embedder
with Interfaces; use Interfaces;

package body Bundle is
   function Hash (S : String) return Natural;

   P : constant array (0 .. 0) of Natural :=
     (0 .. 0 => 4);

   T1 : constant array (0 .. 0) of Unsigned_8 :=
     (0 .. 0 => 1);

   T2 : constant array (0 .. 0) of Unsigned_8 :=
     (0 .. 0 => 3);

   G : constant array (0 .. 5) of Unsigned_8 :=
     (0, 0, 0, 0, 0, 1);

   function Hash (S : String) return Natural is
      F : constant Natural := S'First - 1;
      L : constant Natural := S'Length;
      F1, F2 : Natural := 0;
      J : Natural;
   begin
      for K in P'Range loop
         exit when L < P (K);
         J  := Character'Pos (S (P (K) + F));
         F1 := (F1 + Natural (T1 (K)) * J) mod 6;
         F2 := (F2 + Natural (T2 (K)) * J) mod 6;
      end loop;
      return (Natural (G (F1)) + Natural (G (F2))) mod 2;
   end Hash;

   C_0 : aliased constant Ada.Streams.Stream_Element_Array :=
     (109, 115, 103, 95, 100, 101, 115, 99, 114, 105, 112, 116, 105, 111, 110, 61, 80, 114, 111, 100, 117,
      99, 116, 32, 100, 101, 115, 99, 114, 105, 112, 116, 105, 111, 110, 10, 109, 115, 103, 95, 116,
      105, 116, 108, 101, 61, 84, 105, 116, 108, 101, 10);

   C_1 : aliased constant Ada.Streams.Stream_Element_Array :=
     (109, 115, 103, 95, 100, 101, 115, 99, 114, 105, 112, 116, 105, 111, 110, 61, 68, 101, 115, 99, 114,
      105, 112, 116, 105, 111, 110, 32, 112, 114, 111, 100, 117, 105, 116, 10, 109, 115, 103, 95, 116,
      105, 116, 108, 101, 61, 84, 105, 116, 114, 101, 10);



   type Name_Access is access constant String;
   type Keyword_Array is array (Natural range <>) of Name_Access;

   K_0             : aliased constant String := "msg.properties";
   K_1             : aliased constant String := "msg_fr.properties";

   Keywords : constant Keyword_Array := (
      K_0'Access, K_1'Access);

   type Content_Array is array (Natural range <>) of Content_Access;
   Contents : constant Content_Array := (
      C_0'Access, C_1'Access);

   function Get_Content (Name : String) return Content_Access is
      H : constant Natural := Hash (Name);
   begin
      return (if Keywords (H).all = Name then Contents (H) else null);
   end Get_Content;

end Bundle;
