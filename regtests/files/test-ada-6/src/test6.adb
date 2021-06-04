with Resources6;
with Ada.Command_Line;
with Ada.Text_IO;
procedure Test6 is
   use Resources6;
begin
   if Id_main_html'Length /= 356 then
      Ada.Text_IO.Put_Line ("FAIL: Invalid length for 'main.html'");
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end if;

   if Id_Jsmain_js'Length /= 87 then
      Ada.Text_IO.Put_Line ("FAIL: Invalid length for 'js/main.js'");
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end if;

   if Id_Cssmain_css'Length /= 60 then
      Ada.Text_IO.Put_Line ("FAIL: Invalid length for 'css/main.css'");
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end if;

   Ada.Text_IO.Put ("PASS: ");
   for Val of Id_Cssmain_css loop
      if Character'Val (Val) /= ASCII.LF then
         Ada.Text_IO.Put (Character'Val (Val));
      end if;
   end loop;
   Ada.Text_IO.New_Line;
end Test6;
