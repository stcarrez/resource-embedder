-- Advanced Resource Embedder 1.1.0
package Lines is

   type Content_Array is array (Natural range <>) of access constant String;
   type Content_Access is access constant Content_Array;

   Id_empty_txt : aliased constant Content_Array;

private

   Id_empty_txt : aliased constant Content_Array(1 .. 0) := (others => <>);

end Lines;