with Resources8;
with Ada.Command_Line;
with Ada.Text_IO;
procedure Test8 is
   use Resources8;

   C : Content_Access := Get_Content ("config/test8.xml");
begin
   if C = null then
      Ada.Text_IO.Put_Line ("FAIL: No content 'config/test8.xml'");
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      return;
   end if;
   if C'Length /= 37 then
      Ada.Text_IO.Put_Line ("FAIL: Invalid length for 'config/test8.xml'");
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end if;

   C := Get_Content ("CONFIG/TEST8.XML");
   if C = null then
      Ada.Text_IO.Put_Line ("FAIL: No content 'config/test8.xml'");
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      return;
   end if;

   C := Get_Content ("CoNfIg/TeSt8.XmL");
   if C = null then
      Ada.Text_IO.Put_Line ("FAIL: No content 'config/test8.xml'");
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      return;
   end if;

   Ada.Text_IO.Put ("PASS: ");
   Ada.Text_IO.Put_Line (C.all);
end Test8;
