with Ada.Containers.Indefinite_Vectors,
     Ada.Directories,
     Ada.Integer_Text_IO,
     Ada.Strings.Fixed,
     Ada.Text_IO;

package body ALI_Parse is

  verbose : constant Boolean := False;

  function To_Digit (c : Character) return Natural is
  (Character'Pos (c) - Character'Pos ('0'));

  procedure Gather_Cross_References
    (ali           : in out ALI_Obj;
     ada_root_name : in     String;
     object_path   : in     String;
     flavor        : in     Filtering_Flavor;
     from_scratch  : in     Boolean := True;
     recursive     : in     Boolean := True)
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

    no_prefetch : constant := -1;

    function Get_Num (start_value : Integer := no_prefetch) return Natural is
      result : Natural;
    begin
      if start_value = no_prefetch then
        result := 0;
      else
        result := start_value;
        if End_Of_Line (f) then
          return result;
        end if;
      end if;
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
        sp : Natural := Index (dep_name, ASCII.HT & "");
      begin
        if sp = 0 then
          sp := Index (dep_name, " ");
        end if;
        if sp = 0 then
          --  Neither a Tab, nor a Space...
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
      counter : Natural;
      consider : Boolean;
      ref_type : Character;
    begin
      --  Initial c is not pre-fetched!
      loop
        line_from := Get_Num;
        if c = '|' then
          dep_from := line_from;
          line_from := Get_Num;
        end if;
        --  At this point, c contains the type of reference.
        ref_type := c;
        consider :=
          ref_type not in 'e' | 'E' | 'i' | 'o' | 'p' | 'P' | 't' | '>' | '=' | '<' | '^';
        --  Types of references that we will ignore (documented in lib-xref.ads):
        --
        --        e = end of spec (the ';' after END [unit_name]).
        --        E = first private entity (points to the package, not the entity).
        --        i = implicit reference (duplicates another reference).
        --        l = label on END line
        --        o = own variable reference (SPARK only)
        --        p = primitive operation
        --        P = overriding primitive operation
        --        t = end of body
        --        z = generic formal parameter
        --        > = subprogram IN parameter
        --        = = subprogram IN OUT parameter
        --        < = subprogram OUT parameter
        --        ^ = subprogram ACCESS parameter
        --  NB:
        --        m = modification (duplicates often 'r' reference, but not always).

        if flavor = gnat_studio then
          consider := consider and then ref_type not in 'l';
        end if;
        Get (f, c);
        --  We have an optional thing like "<c,__gnat_malloc>" (Import).
        if c = '<' then
          loop
            Get (f, c);
            exit when c = '>';
          end loop;
          Get (f, c);
        end if;
        if c not in '0' .. '9' then
          raise Program_Error with "Reference (from) column number invalid, got " & c;
        end if;
        col_from := Get_Num (To_Digit (c));
        if c = '[' then
          loop
            Get (f, c);
            exit when c = ']';
          end loop;
          --  Assumption: all ']'s are closed at the end...
          while c = ']' and not End_Of_Line (f) loop
            Get (f, c);
          end loop;
        end if;

        if consider then
          declare
            new_key  : constant String := dep (dep_from) & line_from'Image & col_from'Image;
            new_elem : constant String := dep (dep_to)   & line_to'Image   & col_to'Image;
          begin
            if ali.links.Contains (new_key) then
              if verbose then
                Put_Line ("   Duplicate reference (from) " & new_key & ", ref. type: " & ref_type);
              end if;
            else
              --  Add the link:
              ali.links.Include
                (Key      => new_key,
                 New_Item => new_elem & ' ' & ref_type);
              --  Increment the counter:
              counter := ali.ref_counts.Element (new_elem);
              ali.ref_counts.Replace_Element (ali.ref_counts.Find (new_elem), counter + 1);
              if ref_type = 'b' and then flavor = source_browser then
                --  Here we have a body-to-spec link.
                --  We add the reciprocal spec-to-body link too:
                ali.links.Include
                  (Key      => new_elem,
                   New_Item => new_key & ' ' & ref_type);
              end if;
            end if;
            if verbose then
              Put_Line ("    Ref from: " & new_key);
            end if;
          end;

        end if;

        exit when End_Of_Line (f) or End_Of_File (f);
      end loop;
      if End_Of_Line (f) then
        Get (f, c);
      end if;
    end Get_Refs;

    procedure Get_XRef_Target is
      id     : String (1 .. 1000);
      id_len : Natural;
      curly : Natural := 0;
      use String_to_Integer_Maps;
      entity_type : Character;
    begin
      line_to := Get_Num (To_Digit (c));
      if End_Of_Line (f) then
        raise Program_Error
          with "XRef target's line ends after column number, =" & line_to'Image;
      end if;
      entity_type := c;
      col_to  := Get_Num;
      dep_from := dep_to;
      if verbose then
        Put_Line ("  Ref to: " & dep (dep_to) & ',' & line_to'Image & ',' & col_to'Image & " <- target");
      end if;
      --  c = ' ', '+' or '*', level sign after the column number. We ignore it
      id_len := 0;
      Get (f, c);
      if c in 'a' .. 'z' | 'A' .. 'Z' then
        loop
          --  Read the identifier entity name:
          id_len := id_len + 1;
          id (id_len) := c;
          exit when End_Of_Line (f) or End_Of_File (f);
          Get (f, c);
          exit when c not in
            '_' | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9';
        end loop;
      elsif c = '"' then
        loop
          --  Read the operator entity name ("+", "*", ...):
          id_len := id_len + 1;
          id (id_len) := c;
          Get (f, c);
          exit when c = '"';
        end loop;
        id_len := id_len + 1;
        id (id_len) := c;
        Get (f, c);
      else
        raise Program_Error with "Invalid entity name starting with " & c;
      end if;

      declare
        key : constant String := dep (dep_to) & line_to'Image & col_to'Image;
      begin
        ali.entities.Include (Key => key, New_Item => id (1 .. id_len));
        ali.entity_types.Include (Key => key, New_Item => entity_type);
        if ali.ref_counts.Find (key) = No_Element then
          --  Define a new counter for this definition:
          ali.ref_counts.Insert (Key => key, New_Item => 0);
        end if;
      end;

      if verbose then
        Put_Line ("  Entity: " & id (1 .. id_len) & ' ' & entity_type);
      end if;

      loop
        case c is
          when '{' => curly := curly + 1;
          when '}' => curly := curly - 1;
          when others => null;
        end case;
        exit when (c = ' ' and curly = 0) or End_Of_Line (f) or End_Of_File (f);
        --  Skip the target details:
        Get (f, c);
        --  put_line ("Skipped: " & c);
      end loop;

    end Get_XRef_Target;

    procedure Parse_ALI_File (simple_ali_name, full_ali_name : String) is
    begin
      if verbose then
        Put_Line ("Gathering cross references for: " & full_ali_name);
      end if;

      Open (f, In_File, full_ali_name);
      Get (f, c);

      while not End_Of_File (f) loop
        case c is
          when 'D' =>
            Get_Dep;
            Get (f, c);
          when 'X' =>
            --  XRef header
            Get (f, dep_to);
            if verbose then
              Put_Line ("  X File #" & dep_to'Image & " " & dep (dep_to));
            end if;
            Skip_Line (f);
            Get (f, c);
          when '0' .. '9' =>
            --  Start of a XRef
            Get_XRef_Target;
            if End_Of_Line (f) then
              Get (f, c);
            else
              Get_Refs;
            end if;
          when '.' =>
            --  Skip the ' ':
            Get (f, c);
            --  Continuation of a XRef
            Get_Refs;
          when others =>
            Skip_Line (f);
            Get (f, c);
        end case;
      end loop;
      Close (f);
    exception
      when Name_Error =>
        if verbose then
          Put_Line ("Cannot find: " & simple_ali_name);
        end if;
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

      Parse_ALI_File (ali_name, Search_File (ali_name, object_path));
      ali.visited_alis.Include (ali_name);

      --  Support for some exotic naming conventions...

      if ali_name'Last - 5 >= ali_name'First + 1 and then
        ali_name (ali_name'Last - 5) = '.'
        --  x.1.ada, x.2.ada: there are sometimes x.1.ali and x.2.ali !
        --  6543210
      then
        ali_name (ali_name'Last - 4) := '1';
        Parse_ALI_File (ali_name, Search_File (ali_name, object_path));
        ali.visited_alis.Include (ali_name);

        ali_name (ali_name'Last - 4) := '2';
        Parse_ALI_File (ali_name, Search_File (ali_name, object_path));
        ali.visited_alis.Include (ali_name);
      end if;

      if recursive then
        for dep_ada_name of dep loop

          ali.ada_names.Include (dep_ada_name);

          ali.Gather_Cross_References
            (ada_root_name => Ada.Directories.Simple_Name (dep_ada_name),
             object_path   => object_path,
             flavor        => flavor,
             from_scratch  => False,
             recursive     => True);

        end loop;
      end if;

    end if;

  end Gather_Cross_References;

  function Get_Ada_File_Names (ali : ALI_Obj) return String_Sets.Set is (ali.ada_names);

  function Get_Links (ali : ALI_Obj) return String_to_String_Maps.Map is (ali.links);

  function Get_Entities (ali : ALI_Obj) return String_to_String_Maps.Map is (ali.entities);

  function Get_Entity_Types (ali : ALI_Obj) return String_to_Character_Maps.Map is (ali.entity_types);

  function Get_Reference_Counts (ali : ALI_Obj) return String_to_Integer_Maps.Map is (ali.ref_counts);

  function Is_Lib_Name (ada_name : String) return Boolean is
  (ada_name (ada_name'First .. ada_name'First + 1) in "a-" | "g-" | "i-" | "s-" or else
   ada_name in
     "ada.ads" | "gnat.ads" | "interfac.ads" | "machcode.ads" |
     "system.ads" | "text_io.ads" | "unchconv.ads" | "unchdeal.ads");

  function Search_File (simple_file_name, path : String) return String is
    --  Reused from HAC's HAT.

    GNAT_Directory_Separator : constant Character;
    pragma Import (C, GNAT_Directory_Separator, "__gnat_dir_separator");

    sep_pos : Natural := path'First - 1;
    new_sep_pos : Natural;

  begin
    if Ada.Directories.Exists (simple_file_name) then
      return simple_file_name;
    end if;

    for i in path'Range loop
      new_sep_pos := sep_pos;
      if path (i) in ',' | ';' then
        new_sep_pos := i;
      elsif i = path'Last then
        new_sep_pos := i + 1;
      end if;
      if new_sep_pos > sep_pos then
        declare
          full_file_name : constant String :=
            path (sep_pos + 1 .. new_sep_pos - 1) &
            GNAT_Directory_Separator &
            simple_file_name;
        begin
          if Ada.Directories.Exists (full_file_name) then
            return full_file_name;
          end if;
        end;
      end if;
      sep_pos := new_sep_pos;
    end loop;

    return "";
  end Search_File;

  function Verbose_Entity_Type (entity_type : Character) return String is
  (case entity_type is
     when 'A' => "array type",
     when 'B' => "Boolean type",
     when 'C' => "class-wide type",
     when 'D' => "decimal fixed-point type",
     when 'E' => "non_Boolean enumeration type",
     when 'F' => "floating-point type",
     when 'G' => "C/C++ fun-like macro",
     when 'H' => "abstract type",
     when 'I' => "signed integer type",
     when 'J' => "C++ class",
     when 'K' => "package",
     when 'L' => "label on statement",
     when 'M' => "modular integer type",
     when 'N' => "named number",
     when 'O' => "ordinary fixed-point type",
     when 'P' => "access type",
     when 'Q' => "C/C++ include file",
     when 'R' => "record type",
     when 'S' => "string type",
     when 'T' => "task type",
     when 'U' => "procedure",
     when 'V' => "function or operator",
     when 'W' => "protected type",
     when 'X' => "exception",
     when 'Y' => "entry or entry family",
     when 'a' => "array object",
     when 'b' => "Boolean object",
     when 'c' => "class-wide object",
     when 'd' => "decimal fixed-point object",
     when 'e' => "non-Boolean enumeration object",
     when 'f' => "floating-point object",
     when 'g' => "C/C++ macro",
     when 'h' => "Interface (Ada 2005)",
     when 'i' => "signed integer object",
     when 'j' => "C++ class object",
     when 'k' => "generic package",
     when 'l' => "label on loop",
     when 'm' => "modular integer object",
     when 'n' => "enumeration literal",
     when 'o' => "ordinary fixed-point object",
     when 'p' => "access object",
     when 'q' => "label on block",
     when 'r' => "record object",
     when 's' => "string object",
     when 't' => "task object",
     when 'u' => "generic procedure",
     when 'v' => "generic function or operator",
     when 'w' => "protected object",
     when 'x' => "abstract procedure",
     when 'y' => "abstract function",
     when 'z' => "generic formal parameter",
     when others => (1 => entity_type));

end ALI_Parse;
