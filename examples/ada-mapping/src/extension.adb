with Ada.Text_IO;
with Ada.Command_Line;
with Ada.Directories;
with Extensions_Map;
procedure Extension is
   Count : constant Natural := Ada.Command_Line.Argument_Count;
begin
   if Count = 0 then
      Ada.Text_IO.Put_Line ("Find the language from the extension using a mapping table");
      Ada.Text_IO.Put_Line ("Usage: extension filename.ext");
      return;
   end if;

   for I in 1 .. Count loop
      declare
         Name : constant String := Ada.Command_Line.Argument (I);
	 Ext  : constant String := Ada.Directories.Extension (Name);
	 Kind : access constant String
	    := Extensions_Map.Get_Mapping (Ext);
      begin
         if Kind /= null then
	    Ada.Text_IO.Put_Line (Kind.all);
	 end if;
      end;
   end loop;
end Extension;
