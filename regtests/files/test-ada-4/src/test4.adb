with Resource4;
with Ada.Command_Line;
with Ada.Text_IO;
procedure Test4 is
   use Resource4;

   C : Content_Access := Get_Content ("web/main.html");
begin
   if C = null then
      Ada.Text_IO.Put_Line ("FAIL: No content 'web/main.html'");
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      return;
   end if;
   if C'Length /= 360 then
      Ada.Text_IO.Put_Line ("FAIL: Invalid length for 'web/main.html'");
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end if;

   C := Get_Content ("web/images/wiki-create.png");
   if C = null then
      Ada.Text_IO.Put_Line ("FAIL: No content 'web/images/wiki-create.png'");
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      return;
   end if;
   if C'Length /= 3534 then
      Ada.Text_IO.Put_Line ("FAIL: Invalid length for 'web/images/wiki-create.png'");
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end if;

   C := Get_Content ("not-included.xml");
   if C /= null then
      Ada.Text_IO.Put_Line ("FAIL: Content was included 'not-included.xml'");
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      return;
   end if;

   C := Get_Content ("web/preview/main-not-included.html");
   if C /= null then
      Ada.Text_IO.Put_Line ("FAIL: Content was included 'web/preview/main-not-included.html'");
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      return;
   end if;

   C := Get_Content ("web/js/main.js");
   if C /= null then
      Ada.Text_IO.Put_Line ("FAIL: Content was included 'web/js/main.js'");
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      return;
   end if;

   C := Get_Content ("web/css/main.css");
   if C = null then
      Ada.Text_IO.Put_Line ("FAIL: No content 'web/css/main.css'");
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      return;
   end if;
   if C'Length /= 103 then
      Ada.Text_IO.Put_Line ("FAIL: Invalid length for 'web/css/main.css'");
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end if;

   C := Get_Content ("not-included.txt");
   if C /= null then
      Ada.Text_IO.Put_Line ("FAIL: Content was included 'not-included.txt'");
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      return;
   end if;

   C := Get_Content ("config/test4.xml");
   if C = null then
      Ada.Text_IO.Put_Line ("FAIL: No content 'config/test4.xml'");
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      return;
   end if;
   if C'Length /= 23 then
      Ada.Text_IO.Put_Line ("FAIL: Invalid length for 'config/test4.xml'");
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end if;

   Ada.Text_IO.Put ("PASS: ");
   for Val of C.all loop
      if Character'Val (Val) /= ASCII.LF then
         Ada.Text_IO.Put (Character'Val (Val));
      end if;
   end loop;
   Ada.Text_IO.New_Line;
end Test4;