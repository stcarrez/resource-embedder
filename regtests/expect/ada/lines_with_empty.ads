--  Advanced Resource Embedder 1.5.1
package Lines_With_Empty is

   type Content_Array is array (Positive range <>) of access constant String;
   type Content_Access is access constant Content_Array;

   Id_multiple_txt : aliased constant Content_Array;

private

   L_1   : aliased constant String := "line 1";
   L_2   : aliased constant String := "";
   L_3   : aliased constant String := "line 2";
   L_4   : aliased constant String := "";
   L_5   : aliased constant String := "line 3";
   L_6   : aliased constant String := "line 4";
   Id_multiple_txt : aliased constant Content_Array :=
     (L_1'Access,
      L_2'Access,
      L_3'Access,
      L_4'Access,
      L_5'Access,
      L_6'Access);

end Lines_With_Empty;
