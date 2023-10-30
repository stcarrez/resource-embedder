--  Advanced Resource Embedder 1.3.0
with Interfaces; use Interfaces;

package body Bundle is
   function Hash (S : String) return Natural;

   P : constant array (0 .. 1) of Natural :=
     (1, 4);

   T1 : constant array (0 .. 1) of Unsigned_8 :=
     (0, 4);

   T2 : constant array (0 .. 1) of Unsigned_8 :=
     (1, 3);

   G : constant array (0 .. 5) of Unsigned_8 :=
     (0, 0, 0, 0, 1, 0);

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
     (1 => 109, 2 => 115, 3 => 103, 4 => 95, 5 => 100, 6 => 101, 7 => 115,
      8 => 99, 9 => 114, 10 => 105, 11 => 112, 12 => 116, 13 => 105,
      14 => 111, 15 => 110, 16 => 61, 17 => 80, 18 => 114, 19 => 111,
      20 => 100, 21 => 117, 22 => 99, 23 => 116, 24 => 32, 25 => 100,
      26 => 101, 27 => 115, 28 => 99, 29 => 114, 30 => 105, 31 => 112,
      32 => 116, 33 => 105, 34 => 111, 35 => 110, 36 => 10, 37 => 109,
      38 => 115, 39 => 103, 40 => 95, 41 => 116, 42 => 105, 43 => 116,
      44 => 108, 45 => 101, 46 => 61, 47 => 84, 48 => 105, 49 => 116,
      50 => 108, 51 => 101, 52 => 10);

   C_1 : aliased constant Ada.Streams.Stream_Element_Array :=
     (1 => 109, 2 => 115, 3 => 103, 4 => 95, 5 => 100, 6 => 101, 7 => 115,
      8 => 99, 9 => 114, 10 => 105, 11 => 112, 12 => 116, 13 => 105,
      14 => 111, 15 => 110, 16 => 61, 17 => 68, 18 => 101, 19 => 115,
      20 => 99, 21 => 114, 22 => 105, 23 => 112, 24 => 116, 25 => 105,
      26 => 111, 27 => 110, 28 => 32, 29 => 112, 30 => 114, 31 => 111,
      32 => 100, 33 => 117, 34 => 105, 35 => 116, 36 => 10, 37 => 109,
      38 => 115, 39 => 103, 40 => 95, 41 => 116, 42 => 105, 43 => 116,
      44 => 108, 45 => 101, 46 => 61, 47 => 84, 48 => 105, 49 => 116,
      50 => 114, 51 => 101, 52 => 10);

   type Name_Access is access constant String;
   type Name_Array is array (Natural range <>) of Name_Access;


   K_0             : aliased constant String := "msg";
   K_1             : aliased constant String := "msg_fr";

   Names : constant Name_Array := (
      K_0'Access, K_1'Access);

   type Content_List_Array is array (Natural range <>) of Content_Access;
   Contents : constant Content_List_Array := (
      C_0'Access, C_1'Access);

   function Get_Content (Name : String) return Content_Access is
      H : constant Natural := Hash (Name);
   begin
      return (if Names (H).all = Name then Contents (H) else null);
   end Get_Content;

end Bundle;
