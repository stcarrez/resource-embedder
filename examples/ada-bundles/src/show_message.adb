with Ada.Text_IO;
with Ada.Command_Line;
with Bundle;
procedure Show_Message is
   use Bundle;

   Count : constant Natural := Ada.Command_Line.Argument_Count;
begin
   if Count = 0 then
      Ada.Text_IO.Put_Line ("Missing argument 'msg' or 'msg_fr'");
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      return;
   end if;

   for I in 1 .. Count loop
      declare
         Name : constant String := Ada.Command_Line.Argument (I);
         C    : constant Content_Access := Get_Content (Name);
      begin
         if C = null then
            Ada.Text_IO.Put_Line ("FAIL: No bundle file " & Name);
            Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
            return;
         end if;

         for Val of C.all loop
            Ada.Text_IO.Put (Character'Val (Val));
         end loop;
      end;
   end loop;
end Show_Message;
