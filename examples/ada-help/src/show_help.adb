with Ada.Text_IO;
with Ada.Command_Line;
with Resources.Help;
with Resources.Man;
procedure Show_Help is
   use Resources;

   procedure Print (Name : in String) is
     C : Content_Type := Man.Get_Content (Name);
   begin
      if C = Null_Content then
         C := Help.Get_Content (Name);
         if C = Null_Content then
            Ada.Text_IO.Put_Line ("FAIL: No help for '" & Name & "'");
            Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
            return;
         end if;
      end if;
      for Val of C.Content.all loop
         Ada.Text_IO.Put (Character'Val (Val));
      end loop;
   end Print;

   Count : constant Natural := Ada.Command_Line.Argument_Count;
begin
   if Count = 0 then
      Ada.Text_IO.Put_Line ("Help names:");
      for Name of Help.Names loop
         Ada.Text_IO.Put_Line ("  " & Name.all);
      end loop;

      Ada.Text_IO.Put_Line ("Man pages:");
      for Name of Man.Names loop
         Ada.Text_IO.Put_Line ("  " & Name.all);
      end loop;
      return;
   end if;

   for I in 1 .. Count loop
      Print (Ada.Command_Line.Argument (I));
   end loop;
end Show_Help;
