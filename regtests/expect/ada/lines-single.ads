-- Advanced Resource Embedder 1.1.0
package Lines is

   type Content_Array is array (Natural range <>) of access constant String;

   Id_single_txt : aliased constant Content_Array;

private

   L_0   : aliased constant String := "single line";
   Id_single_txt : aliased constant Content_Array := (1 => L_0'Access);

end Lines;
