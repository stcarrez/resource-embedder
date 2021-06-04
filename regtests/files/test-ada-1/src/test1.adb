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
   if C'Length /= 356 then
      Ada.Text_IO.Put_Line ("FAIL: Invalid length for 'main.html'");
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end if;

   C := Get_Content ("js/main.js");
   if C = null then
      Ada.Text_IO.Put_Line ("FAIL: No content 'js/main.js'");
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      return;
   end if;
   if C'Length /= 87 then
      Ada.Text_IO.Put_Line ("FAIL: Invalid length for 'js/main.js'");
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end if;

   C := Get_Content ("css/main.css");
   if C = null then
      Ada.Text_IO.Put_Line ("FAIL: No content 'css/main.css'");
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      return;
   end if;
   if C'Length /= 60 then
      Ada.Text_IO.Put_Line ("FAIL: Invalid length for 'css/main.css'");
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end if;

   Ada.Text_IO.Put ("PASS: ");
   for Val of C.all loop
      if Character'Val (Val) /= ASCII.LF then
         Ada.Text_IO.Put (Character'Val (Val));
      end if;
   end loop;
   Ada.Text_IO.New_Line;
end Test1;