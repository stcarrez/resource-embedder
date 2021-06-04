with Ada.Text_IO;
with Ada.Command_Line;
with Config;
procedure Show_Config is
   use Config;

   C : constant Content_Type := Get_Content ("example.conf");
begin
   if C.Content = null then
      Ada.Text_IO.Put_Line ("FAIL: No configuration file 'example.conf'");
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      return;
   end if;

   for Val of C.Content.all loop
      Ada.Text_IO.Put (Character'Val (Val));
   end loop;
end Show_Config;
