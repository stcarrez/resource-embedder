with Ada.Text_IO;
with Ada.Command_Line;
with Resources.Help;
with Resources.Man;
procedure Show_Help is
   use Resources;

   procedure Print (Name : in String) is
      C : constant Content_Access := Man.Get_Content (Name);
      H : access constant String;
   begin
      if C /= null then
         Ada.Text_IO.Put (C.all);
         return;
      end if;

      H := Help.Get_Content (Name);
      if H /= null then
         Ada.Text_IO.Put (H.all);
         return;
      end if;

      Ada.Text_IO.Put_Line ("FAIL: No help for '" & Name & "'");
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
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
