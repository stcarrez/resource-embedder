with Ada.Text_IO.Text_Streams;
with Ada.Command_Line;
with Resources.Help;
with Resources.Man;
with System.Storage_Elements;
procedure Show_Help is
   use Ada.Text_IO;
   use Resources;
   use System.Storage_Elements;

   procedure Print (Name : in String) is
      C : access constant Storage_Array := Man.Get_Content (Name);
   begin
      if C = null then
         C := Help.Get_Content (Name);
         if C = null then
            Ada.Text_IO.Put_Line ("FAIL: No help for '" & Name & "'");
            Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
         end if;
      end if;

      Storage_Array'Write (Text_Streams.Stream (Current_Output), C.all);
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
