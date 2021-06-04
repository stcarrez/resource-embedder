with Ada.Text_IO;
with Ada.Command_Line;
with Bundle;
procedure Show_Message is
   use Bundle;

   C : constant Content_Access := Get_Content ("msg_fr.properties");
begin
   if C = null then
      Ada.Text_IO.Put_Line ("FAIL: No bundle file 'msg_fr.properties'");
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      return;
   end if;

   for Val of C.all loop
      Ada.Text_IO.Put (Character'Val (Val));
   end loop;
end Show_Message;
