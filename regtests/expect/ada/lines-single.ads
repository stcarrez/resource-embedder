--  Advanced Resource Embedder 1.5.0
--  Header test line 1
--  Header test line 2
--  Header spec test line 3
package Lines is

   type Content_Array is array (Positive range <>) of access constant String;
   type Content_Access is access constant Content_Array;

   Id_single_txt : aliased constant Content_Array;

private

   L_1   : aliased constant String := "single line";
   Id_single_txt : aliased constant Content_Array := (1 => L_0'Access);

end Lines;
