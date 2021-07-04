with Resources7;
with Ada.Command_Line;
with Ada.Text_IO;
procedure Test7 is
   use Resources7;

   C : Content_Access := Get_Content ("config/test7.xml");
begin
   if C = null then
      Ada.Text_IO.Put_Line ("FAIL: No content 'config/test7.xml'");
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      return;
   end if;
   if C'Length /= 22 then
      Ada.Text_IO.Put_Line ("FAIL: Invalid length for 'config/test7.xml'");
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end if;

   C := Get_Content ("CONFIG/TEST7.XML");
   if C = null then
      Ada.Text_IO.Put_Line ("FAIL: No content 'config/test7.xml'");
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      return;
   end if;

   C := Get_Content ("CoNfIg/TeSt7.XmL");
   if C = null then
      Ada.Text_IO.Put_Line ("FAIL: No content 'config/test7.xml'");
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      return;
   end if;

   Ada.Text_IO.Put ("PASS: ");
   Ada.Text_IO.Put_Line (C.all);
end Test7;
