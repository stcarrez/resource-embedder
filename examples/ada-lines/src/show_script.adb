with Ada.Text_IO;
with Scripts;
procedure Show_Script is
begin
   Ada.Text_IO.Put ("Create SQLite");
   Ada.Text_IO.Put (Natural'Image (Scripts.create_database'Length));
   Ada.Text_IO.Put_Line (" lines");
   for Line of Scripts.create_database loop
      Ada.Text_IO.Put_Line (Line.all);
   end loop;

   Ada.Text_IO.Put ("Drop SQLite");
   Ada.Text_IO.Put (Natural'Image (Scripts.drop_database'Length));
   Ada.Text_IO.Put_Line (" lines");
   for Line of Scripts.drop_database loop
      Ada.Text_IO.Put_Line (Line.all);
   end loop;
end Show_Script;
