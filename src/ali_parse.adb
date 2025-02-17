with Ada.Containers.Indefinite_Vectors,
     Ada.Integer_Text_IO,
     Ada.Strings.Fixed,
     Ada.Text_IO;

package body ALI_Parse is

  verbose : constant Boolean := True;

  function To_Digit (c : Character) return Natural is
  (Character'Pos (c) - Character'Pos ('0'));

  procedure Gather_Cross_References
    (ali           : in out ALI_Obj;
     ada_root_name : in     String;
     object_path   : in     String;
     from_scratch  : in     Boolean := True)
  is
    use Ada.Integer_Text_IO, Ada.Strings.Fixed, Ada.Text_IO;
    f : File_Type;
    ali_name : String := ada_root_name;

    package Name_Catalogues is new
      Ada.Containers.Indefinite_Vectors
        (Index_Type   => Positive,
         Element_Type => String,
         "="          => "=");

    c : Character;
    dep : Name_Catalogues.Vector;
    dep_from, dep_to : Positive;
    line_to, col_to, col_from, line_from : Natural;

    function Get_Num (start_value : Natural := 0) return Natural is
      result : Natural := start_value;
    begin
      loop
        Get (f, c);
        exit when c not in '0' .. '9';
        result := result * 10 + To_Digit (c);
        exit when End_Of_Line (f) or End_Of_File (f);
      end loop;
      return result;
    end Get_Num;

    procedure Get_Dep is
    begin
      Get (f, c);  --  Skip space
      declare
        dep_name : constant String := Get_Line (f);
        sp : Natural := Index (dep_name, (ASCII.HT & ""));
      begin
        if sp = 0 then
          sp := dep_name'Last;
        else
          sp := sp - 1;
        end if;
        dep.Append (dep_name (dep_name'First .. sp));
      end;
      if verbose then
        Put_Line ("  Deps:" & dep.Length'Image & ' ' & dep (Integer (dep.Length)));
      end if;
    end Get_Dep;

    procedure Get_Refs is
    begin
      loop
        line_from := Get_Num;
        if c = '|' then
          dep_from := line_from;
          line_from := Get_Num;
        else
          dep_from := dep_to;
        end if;
        col_from := Get_Num;
        ali.links.Include
          (Key      => dep (dep_from) & line_from'Image & col_from'Image,
           New_Item => dep (dep_to)   & line_to'Image   & col_to'Image);
        if verbose then
          Put_Line ("    Ref from: " & dep (dep_from)   & line_from'Image   & col_from'Image);
        end if;
        exit when End_Of_Line (f) or End_Of_File (f);
      end loop;
    end Get_Refs;

    procedure Parse_ALI_File (full_ali_name : String) is
    begin
      Open (f, In_File, full_ali_name);

      while not End_Of_File (f) loop
        Get (f, c);
        case c is
        when 'D' =>
          Get_Dep;
        when 'X' =>  --  XRef header
          Get (f, dep_to);
          Skip_Line (f);
        when '0' .. '9' =>  --  Start of a XRef
          line_to := Get_Num (To_Digit (c));
          col_to  := Get_Num;
          if verbose then
            Put_Line ("  Ref to: " & dep (dep_to) & ',' & line_to'Image & ',' & col_to'Image & " <- target");
          end if;
          Get (f, c);  --  Skip ' ' or '*' after the column.
          loop
            --  Skip the target details:
            Get (f, c);
            exit when c = ' ' or End_Of_Line (f) or End_Of_File (f);
          end loop;
          if not (End_Of_Line (f) or End_Of_File (f)) then
            Get_Refs;
          end if;
        when '.' =>
          --  Continuation of a XRef
          Get_Refs;
        when others =>
          Skip_Line (f);
        end case;
      end loop;
      Close (f);
    exception
      when Name_Error =>
        null;
      when End_Error =>
        Close (f);
    end Parse_ALI_File;

  begin
    if from_scratch then
      ali.visited_alis.Clear;
    end if;

    ali_name (ali_name'Last - 2 .. ali_name'Last) := "ali";

    if ali.visited_alis.Contains (ali_name) then
      --  Already processed.
      null;
    else

      if verbose then
        Put_Line ("Gathering cross references for: " & ali_name);
      end if;

      Parse_ALI_File (ali_name);  --  !! TBD: use object_path here.

      ali.visited_alis.Include (ali_name);

      for dep_ada_name of dep loop

        ali.ada_names.Include (dep_ada_name);

        ali.Gather_Cross_References
          (ada_root_name     => dep_ada_name,
           object_path  => object_path,
           from_scratch => False);

      end loop;

    end if;

  end Gather_Cross_References;

  function Get_Ada_Names (ali : ALI_Obj) return String_Sets.Set is (ali.ada_names);

  function Get_Links (ali : ALI_Obj) return String_Link_Maps.Map is (ali.links);

end ALI_Parse;
