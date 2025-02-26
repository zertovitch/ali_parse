with ALI_Parse;

with Ada.Command_Line;
with Ada.Text_IO;

procedure Show_Stats is
  use Ada.Command_Line, Ada.Text_IO, ALI_Parse;
  ali : ALI_Obj;
  links, entities : String_to_String_Maps.Map;
  counts : String_to_Integer_Maps.Map;
  curs_sts  : String_to_String_Maps.Cursor;
  curs_sti  : String_to_Integer_Maps.Cursor;
  use String_to_String_Maps, String_to_Integer_Maps;
  count_file : File_Type;
  sep : constant Character := ';';
begin

  ali.Gather_Cross_References
    (ada_root_name => (if Argument_Count = 0 then "show_stats.adb" else Argument (1)),
     object_path   => "obj");

  Put_Line ("Entities:");
  entities := ali.Get_Entities;
  curs_sts := entities.First;
  while curs_sts /= String_to_String_Maps.No_Element loop
    Put_Line (Key (curs_sts) & " : " & Element (curs_sts));
    curs_sts := Next (curs_sts);
  end loop;
  New_Line;

  Put_Line ("Links:");
  links := ali.Get_Links;
  curs_sts := links.First;
  while curs_sts /= String_to_String_Maps.No_Element loop
    Put_Line
      (Key (curs_sts) &
       (if entities.Contains (Key (curs_sts)) then
          " (" & entities.Element (Key (curs_sts)) & ')'
        else "") &
       " --> " &
       Element (curs_sts) &
       " (" & entities.Element (Element (curs_sts)) & ')');
    curs_sts := Next (curs_sts);
  end loop;
  New_Line;

  Create (count_file, Out_File, "references.csv");
  Put_Line (count_file, "Entity" & sep & "Location" & sep & "References");
  Put_Line ("References:");
  counts := ali.Get_Reference_Counts;
  curs_sti := counts.First;
  while curs_sti /= String_to_Integer_Maps.No_Element loop
    Put_Line
      ("Entity at " & Key (curs_sti) &
       " (" & entities.Element (Key (curs_sti)) &
       ") is referenced" &
       Element (curs_sti)'Image &
       " times");
    Put_Line
      (count_file,
       entities.Element (Key (curs_sti)) & sep &
       Key (curs_sti) & sep &
       Element (curs_sti)'Image);
    curs_sti := Next (curs_sti);
  end loop;
  New_Line;
  Close (count_file);

  Put_Line ("Ada files:");
  for an of ali.Get_Ada_File_Names loop
    Put_Line (an);
  end loop;

end Show_Stats;
