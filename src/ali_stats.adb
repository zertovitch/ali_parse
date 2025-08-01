with ALI_Parse;

with Ada.Command_Line,
     Ada.Directories,
     Ada.Strings.Unbounded,
     Ada.Text_IO;

procedure ALI_Stats is
  use Ada.Command_Line, Ada.Directories, Ada.Strings.Unbounded, Ada.Text_IO, ALI_Parse;

  ali : ALI_Obj;
  links, entities : String_to_String_Maps.Map;
  entity_types : String_to_Character_Maps.Map;
  counts : String_to_Integer_Maps.Map;
  curs_sts  : String_to_String_Maps.Cursor;
  curs_sti  : String_to_Integer_Maps.Cursor;
  use String_to_String_Maps, String_to_Integer_Maps;
  count_file : File_Type;
  sep : constant Character := ';';
  myself : constant String := "ali_stats.adb";
  main, path : ALI_Parse.String_Sets.Set;
  all_paths : Unbounded_String;

  procedure Include_From_File (list_name : String) is
    f : File_Type;
  begin
    Open (f, In_File, list_name);
    while not End_Of_File (f) loop
      path.Include (Get_Line (f));
    end loop;
    Close (f);
  end Include_From_File;

  procedure Help is
  begin
    Put_Line (Current_Error, "ALI_Stats - Gather statistics on Ada projects using the .ali files");
    New_Line (Current_Error);
    Put_Line (Current_Error, "Usage:");
    Put_Line (Current_Error, "  ali_stats [switches] main_file1.adb main_file2.adb ...");
    New_Line (Current_Error);
    Put_Line (Current_Error, "Switches:");
    Put_Line (Current_Error, "     -Idir  : Add object directories, separated by ',' or ';'");
    Put_Line (Current_Error, "     -Jfile : Add object directories from a list in a file, one directory per line");
    New_Line (Current_Error);
    Put_Line (Current_Error, "NB: object directories are essential: the *.ali files are searched there for cross-references.");
  end Help;

begin
  if Argument_Count = 0 then
    if Exists ("src/" & myself) then
      Put_Line ("No parameters, but own source found -> demo with " & myself);
      --  Demo with this file
      main.Include (myself);
      path.Include ("obj");
    else
      Help;
      return;
    end if;
  end if;

  for i in 1 .. Argument_Count loop
    declare
      arg : constant String := Argument (i);
      op  : constant String := arg (arg'First + 2 .. arg'Last);
    begin
      case arg (arg'First) is
        when '-' =>
          case arg (arg'First + 1) is
            when 'h'    => Help; return;
            when 'I'    => path.Include (op);
            when 'J'    => Include_From_File (op);
            when others =>
              null;
          end case;
        when others =>
          main.Include (arg);
      end case;
    end;
  end loop;

  --  Concatenate all search paths together.
  for p of path loop
    if all_paths = "" then
      all_paths := To_Unbounded_String (p);
    else
      all_paths := all_paths & ',' & To_Unbounded_String (p);
    end if;
  end loop;

  for m of main loop
    ali.Gather_Cross_References
      (ada_root_name => m,
       object_path   => To_String (all_paths),
       flavor        => gnat_studio);
  end loop;

  Put_Line ("====== Entities:");
  entities := ali.Get_Entities;
  entity_types := ali.Get_Entity_Types;
  curs_sts := entities.First;
  while curs_sts /= String_to_String_Maps.No_Element loop
    Put_Line (Key (curs_sts) & " : " & Element (curs_sts));
    curs_sts := Next (curs_sts);
  end loop;
  New_Line;

  Put_Line ("====== Links:");
  links := ali.Get_Links;
  curs_sts := links.First;
  while curs_sts /= String_to_String_Maps.No_Element loop
    declare
      elem_with_code : constant String := Element (curs_sts);
      elem : constant String := elem_with_code (elem_with_code'First .. elem_with_code'Last - 2);
    begin
      Put_Line
        (Key (curs_sts) &
         (if entities.Contains (Key (curs_sts)) then
            " (" & entities.Element (Key (curs_sts)) & ')'
          else "") &
         " --> " &
         elem_with_code &
         " (" & entities.Element (elem) & ')');
    end;
    curs_sts := Next (curs_sts);
  end loop;
  New_Line;

  Put_Line ("====== References:");
  Create (count_file, Out_File, "references.csv");
  Put_Line (count_file, "Entity" & sep & "Entity Type" & sep & "Location" & sep & "References");
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
       Verbose_Entity_Type (entity_types.Element (Key (curs_sti))) & sep &
       Key (curs_sti) & sep &
       Element (curs_sti)'Image);
    curs_sti := Next (curs_sti);
  end loop;
  New_Line;
  Close (count_file);

  Put_Line ("====== Ada files:");
  for an of ali.Get_Ada_File_Names loop
    Put_Line (an);
  end loop;

end ALI_Stats;
