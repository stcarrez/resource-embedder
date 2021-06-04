with Resource.Web;
with Resource.Config;
with Ada.Command_Line;
with Ada.Text_IO;
procedure Test3 is
   use Resource;

   C : Content_Access := Web.Get_Content ("main.html");
begin
   if C = null then
      Ada.Text_IO.Put_Line ("FAIL: No content 'main.html'");
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      return;
   end if;
   if C'Length /= 360 then
      Ada.Text_IO.Put_Line ("FAIL: Invalid length for 'main.html'");
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end if;

   C := Web.Get_Content ("images/wiki-create.png");
   if C = null then
      Ada.Text_IO.Put_Line ("FAIL: No content 'images/wiki-create.png'");
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      return;
   end if;
   if C'Length /= 3534 then
      Ada.Text_IO.Put_Line ("FAIL: Invalid length for 'images/wiki-create.png'");
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end if;

   C := Web.Get_Content ("not-included.xml");
   if C /= null then
      Ada.Text_IO.Put_Line ("FAIL: Content was included 'not-included.xml'");
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      return;
   end if;

   C := Web.Get_Content ("preview/main-not-included.html");
   if C /= null then
      Ada.Text_IO.Put_Line ("FAIL: Content was included 'preview/main-not-included.html'");
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      return;
   end if;

   C := Web.Get_Content ("js/main.js");
   if C = null then
      Ada.Text_IO.Put_Line ("FAIL: No content 'js/main.js'");
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      return;
   end if;
   if C'Length /= 98 then
      Ada.Text_IO.Put_Line ("FAIL: Invalid length for 'js/main.js'");
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end if;

   C := Web.Get_Content ("css/main.css");
   if C = null then
      Ada.Text_IO.Put_Line ("FAIL: No content 'css/main.css'");
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      return;
   end if;
   if C'Length /= 103 then
      Ada.Text_IO.Put_Line ("FAIL: Invalid length for 'css/main.css'");
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end if;

   C := Web.Get_Content ("not-included.txt");
   if C /= null then
      Ada.Text_IO.Put_Line ("FAIL: Content was included 'not-included.txt'");
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      return;
   end if;

   C := Config.Get_Content ("test3.xml");
   if C = null then
      Ada.Text_IO.Put_Line ("FAIL: No content 'test3.xml'");
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      return;
   end if;
   if C'Length /= 18 then
      Ada.Text_IO.Put_Line ("FAIL: Invalid length for 'test3.xml'");
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end if;

   Ada.Text_IO.Put ("PASS: ");
   for Val of C.all loop
      if Character'Val (Val) /= ASCII.LF then
         Ada.Text_IO.Put (Character'Val (Val));
      end if;
   end loop;
   Ada.Text_IO.New_Line;
end Test3;