--  Advanced Resource Embedder 1.2.0
with Interfaces; use Interfaces;

package body Concat is
   function Hash (S : String) return Natural;

   P : constant array (0 .. 0) of Natural :=
     (0 .. 0 => 1);

   T1 : constant array (0 .. 0) of Unsigned_8 :=
     (0 .. 0 => 0);

   T2 : constant array (0 .. 0) of Unsigned_8 :=
     (0 .. 0 => 1);

   G : constant array (0 .. 4) of Unsigned_8 :=
     (0, 1, 0, 0, 0);

   function Hash (S : String) return Natural is
      F : constant Natural := S'First - 1;
      L : constant Natural := S'Length;
      F1, F2 : Natural := 0;
      J : Natural;
   begin
      for K in P'Range loop
         exit when L < P (K);
         J  := Character'Pos (S (P (K) + F));
         F1 := (F1 + Natural (T1 (K)) * J) mod 5;
         F2 := (F2 + Natural (T2 (K)) * J) mod 5;
      end loop;
      return (Natural (G (F1)) + Natural (G (F2))) mod 2;
   end Hash;

   C_0 : aliased constant Ada.Streams.Stream_Element_Array :=
     (98, 111, 100, 121, 32, 123, 10, 32, 32, 32, 32, 98, 97, 99, 107, 103, 114, 111, 117, 110, 100,
      58, 32, 35, 101, 101, 101, 59, 32, 32, 10, 125, 10, 112, 32, 123, 10, 32, 32, 32, 32,
      99, 111, 108, 111, 114, 58, 32, 35, 50, 97, 50, 97, 50, 97, 59, 32, 32, 10, 125, 98,
      111, 100, 121, 32, 123, 10, 32, 32, 32, 32, 98, 97, 99, 107, 103, 114, 111, 117, 110, 100,
      58, 32, 35, 101, 101, 101, 59, 32, 32, 10, 125, 10, 112, 32, 123, 10, 32, 32, 32, 32,
      99, 111, 108, 111, 114, 58, 32, 35, 50, 97, 50, 97, 50, 97, 59, 32, 32, 10, 125, 10,
      112, 114, 101, 32, 123, 10, 32, 32, 32, 32, 99, 111, 108, 111, 114, 58, 32, 35, 52, 97,
      52, 97, 52, 97, 59, 32, 32, 10, 125, 10, 98, 32, 123, 10, 32, 32, 32, 32, 99, 111,
      108, 111, 114, 58, 32, 35, 48, 97, 48, 97, 48, 97, 59, 32, 32, 10, 125, 10, 100, 105,
      118, 32, 123, 10, 32, 32, 32, 32, 99, 111, 108, 111, 114, 58, 32, 35, 50, 48, 50, 48,
      50, 48, 59, 32, 32, 10, 125, 10, 98, 111, 100, 121, 32, 123, 10, 32, 32, 32, 32, 98,
      97, 99, 107, 103, 114, 111, 117, 110, 100, 58, 32, 35, 101, 101, 101, 59, 32, 32, 10, 125,
      10, 112, 32, 123, 10, 32, 32, 32, 32, 99, 111, 108, 111, 114, 58, 32, 35, 50, 97, 50,
      97, 50, 97, 59, 32, 32, 10, 125, 10, 112, 114, 101, 32, 123, 10, 32, 32, 32, 32, 99,
      111, 108, 111, 114, 58, 32, 35, 52, 97, 52, 97, 52, 97, 59, 32, 32, 10, 125, 10, 98,
      32, 123, 10, 32, 32, 32, 32, 99, 111, 108, 111, 114, 58, 32, 35, 48, 97, 48, 97, 48,
      97, 59, 32, 32, 10, 125, 10, 100, 105, 118, 32, 123, 10, 32, 32, 32, 32, 99, 111, 108,
      111, 114, 58, 32, 35, 50, 48, 50, 48, 50, 48, 59, 32, 32, 10, 125, 10, 98, 111, 100,
      121, 32, 123, 10, 32, 32, 32, 32, 98, 97, 99, 107, 103, 114, 111, 117, 110, 100, 58, 32,
      35, 101, 101, 101, 59, 32, 32, 10, 125, 10, 112, 32, 123, 10, 32, 32, 32, 32, 99, 111,
      108, 111, 114, 58, 32, 35, 50, 97, 50, 97, 50, 97, 59, 32, 32, 10, 125);

   C_1 : aliased constant Ada.Streams.Stream_Element_Array :=
     (118, 97, 114, 32, 101, 108, 101, 99, 32, 61, 32, 123, 10, 32, 32, 32, 32, 101, 49, 50, 58,
      32, 91, 32, 49, 46, 48, 44, 32, 49, 46, 50, 44, 32, 49, 46, 53, 44, 32, 49, 46,
      56, 44, 32, 50, 46, 50, 44, 32, 50, 46, 55, 44, 32, 51, 46, 51, 44, 32, 51, 46,
      57, 44, 32, 52, 46, 55, 44, 32, 53, 46, 54, 44, 32, 54, 46, 56, 44, 32, 56, 46,
      50, 32, 93, 10, 125, 10, 118, 97, 114, 32, 101, 108, 101, 99, 32, 61, 32, 123, 10, 32,
      32, 32, 32, 101, 49, 50, 58, 32, 91, 32, 49, 46, 48, 44, 32, 49, 46, 50, 44, 32,
      49, 46, 53, 44, 32, 49, 46, 56, 44, 32, 50, 46, 50, 44, 32, 50, 46, 55, 44, 32,
      51, 46, 51, 44, 32, 51, 46, 57, 44, 32, 52, 46, 55, 44, 32, 53, 46, 54, 44, 32,
      54, 46, 56, 44, 32, 56, 46, 50, 32, 93, 10, 125, 10, 118, 97, 114, 32, 101, 108, 101,
      99, 32, 61, 32, 123, 10, 32, 32, 32, 32, 101, 49, 50, 58, 32, 91, 32, 49, 46, 48,
      44, 32, 49, 46, 50, 44, 32, 49, 46, 53, 44, 32, 49, 46, 56, 44, 32, 50, 46, 50,
      44, 32, 50, 46, 55, 44, 32, 51, 46, 51, 44, 32, 51, 46, 57, 44, 32, 52, 46, 55,
      44, 32, 53, 46, 54, 44, 32, 54, 46, 56, 44, 32, 56, 46, 50, 32, 93, 10, 125, 10,
      118, 97, 114, 32, 101, 108, 101, 99, 32, 61, 32, 123, 10, 32, 32, 32, 32, 101, 49, 50,
      58, 32, 91, 32, 49, 46, 48, 44, 32, 49, 46, 50, 44, 32, 49, 46, 53, 44, 32, 49,
      46, 56, 44, 32, 50, 46, 50, 44, 32, 50, 46, 55, 44, 32, 51, 46, 51, 44, 32, 51,
      46, 57, 44, 32, 52, 46, 55, 44, 32, 53, 46, 54, 44, 32, 54, 46, 56, 44, 32, 56,
      46, 50, 32, 93, 10, 125, 10);

   type Name_Access is access constant String;
   type Name_Array is array (Natural range <>) of Name_Access;


   K_0             : aliased constant String := "css/css/main.css";
   K_1             : aliased constant String := "js/js/main.js";

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

end Concat;
