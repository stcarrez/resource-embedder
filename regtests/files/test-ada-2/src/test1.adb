with Resources1;
with Ada.Command_Line;
with Ada.Text_IO;
procedure Test1 is
   use Resources1;

   C : Content_Access := Get_Content ("main.html");
begin
   if C = null then
      Ada.Text_IO.Put_Line ("FAIL: No content 'main.html'");
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      return;
   end if;

   C := Get_Content ("js/main.js");
   if C = null then
      Ada.Text_IO.Put_Line ("FAIL: No content 'js/main.js'");
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      return;
   end if;

   C := Get_Content ("css/main.css");
   if C = null then
      Ada.Text_IO.Put_Line ("FAIL: No content 'css/main.css'");
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      return;
   end if;

   Ada.Text_IO.Put_Line ("PASS");
end Test1;