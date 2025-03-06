with ALI_Parse;

with Ada.Characters.Handling,
     Ada.Command_Line,
     Ada.Directories,
     Ada.Integer_Text_IO,
     Ada.Strings.Fixed,
     Ada.Text_IO;
with Ada.Strings.Unbounded;

procedure GNATHTML is
  ali   : ALI_Parse.ALI_Obj;
  links, entities : ALI_Parse.String_to_String_Maps.Map;
  stdlib : Boolean := False;

  function To_HTML_Name (ada_name : String) return String is (ada_name & ".html");

  use Ada.Characters.Handling;

  function Is_Keyword (s : String) return Boolean is
  (To_Upper (s) in
   "ABORT" | "ABS" | "ABSTRACT" | "ACCEPT" | "ACCESS" | "ALIASED" | "ALL" |
   "AND" | "ARRAY" | "AT" | "BEGIN" | "BODY" | "CASE" | "CONSTANT" |
   "DECLARE" | "DELAY" | "DELTA" | "DIGITS" | "DO" |
   "ELSE" | "ELSIF" | "END" | "ENTRY" | "EXCEPTION" | "EXIT" |
   "FOR" | "FUNCTION" | "GENERIC" | "GOTO" |
   "IF" | "IN" | "INTERFACE" | "IS" | "LIMITED" | "LOOP" |
   "MOD" | "NEW" | "NOT" | "NULL" |
   "OF" | "OR" | "OTHERS" | "OUT" | "OVERRIDING" |
   "PACKAGE" | "PARALLEL" | "PRAGMA" | "PRIVATE" | "PROCEDURE" | "PROTECTED" |
   "RAISE" | "RANGE" | "RECORD" | "REM" | "RENAMES" | "REQUEUE" | "RETURN" | "REVERSE" |
   "SELECT" | "SEPARATE" | "SOME" | "SUBTYPE" | "SYNCHRONIZED" |
   "TAGGED" | "TASK" | "TERMINATE" | "THEN" | "TYPE" | "UNTIL" | "USE" |
   "WHEN" | "WHILE" | "WITH" | "XOR");

  html_dir : constant String := "html/";

  procedure Process_Source (ada_name, full_ada_name : String) is
    use Ada.Integer_Text_IO, Ada.Strings, Ada.Strings.Fixed, Ada.Text_IO;
    ada_source, html : File_Type;

    procedure Scan is
      ln : Integer := 0;
      start, stop : Integer;
      link : Boolean;

      procedure Add_Links is
        key : constant String := ada_name & ln'Image & start'Image;
      begin
        link := False;
        --  Possible links TO here:
        if entities.Contains (key) then
          Put
            (html,
             "<a name=""" &
             Trim (ln'Image, Left) & '_' & Trim (start'Image, Left) & """></a>");
        end if;
        --  Link FROM here to somewhere else.
        if links.Contains (key) then
          declare
            dc : constant String := links.Element (key);
            sp : constant Positive := Ada.Strings.Fixed.Index (dc, " ");
            ada_tgt : constant String := dc (dc'First .. sp - 1);
            target  : constant String := To_HTML_Name (ada_tgt);
            lin_col : String := dc (sp + 1 .. dc'Last);
            is_lib : constant Boolean :=
              (ada_tgt (ada_tgt'First .. ada_tgt'First + 1)
               in "a-" | "g-" | "i-" | "s-") or else
              ada_tgt in "ada.ads" | "gnat.ads" | "interfaces.ads" | "system.ads";
          begin
            if stdlib or not is_lib then
              for c of lin_col loop
                if c = ' ' then c := '_'; end if;
              end loop;
              Put (html, "<a href=""" & target & '#' & lin_col & """>");
              link := True;
            end if;
          end;
        end if;
      end Add_Links;

      procedure Process_Line is
        l : constant String := Get_Line (ada_source);
        x : Integer := l'First;
        c : Character;

        procedure Ada_Code is
        begin
          Add_Links;
          case c is
            when 'a' .. 'z' | 'A' .. 'Z' =>
              stop := x;
              loop
                x := x + 1;
                exit when x > l'Last;
                c := l (x);
                exit when c not in 'a' .. 'z' | 'A' .. 'Z' | '_' | '0' .. '9';
                stop := x;
              end loop;
              if Is_Keyword (l (start .. stop)) then
                Put (html, "<b>" & l (start .. stop) & "</b>");
              else
                Put (html, l (start .. stop));
              end if;
            when '&' => Put (html, "&amp;"); x := x + 1;
            when '<' => Put (html, "&lt;");  x := x + 1;
            when '>' => Put (html, "&gt;");  x := x + 1;
            when '-' => x := x + 1;
            --  !! quotes
            when others =>
              Put (html, c);
              x := x + 1;
          end case;
          if link then
            Put (html, "</a>");
          end if;
        end Ada_Code;

        procedure Comment is
        begin
          Put (html, "<font color=""green""><em>-" & l (x .. l'Last) & "</em></font>");
          x := l'Last + 1;
        end Comment;

        minus_minus : Natural := 0;

      begin
        ln := ln + 1;
        --  Display the line number:
        Put (html, "<font color=""lightgray"">");
        Put (html, ln, 4);
        Put (html, "</font>  ");
        while x <= l'Last loop
          c := l (x);
          start := x;
          if c = '-' then
            minus_minus := minus_minus + 1;
          elsif minus_minus = 1 then
            Put (html, '-');  --  Output the delayed '-'
            minus_minus := 0;
          else
            minus_minus := 0;
          end if;
          if minus_minus = 2 then
            Comment;
          else
            Ada_Code;
          end if;
        end loop;
        New_Line (html);
      end Process_Line;

    begin
      while not End_Of_File (ada_source) loop
        Process_Line;
      end loop;
    exception
      when End_Error =>
        null;
    end Scan;

  begin
    if Index (ada_name, ":") > 0 then
      --  Some weirdo config pragma with absolute path.
      return;
    end if;
    Open (ada_source, In_File, full_ada_name);
    Create (html, Out_File, html_dir & To_HTML_Name (ada_name));
    Put_Line (html, "<html>");
    Put_Line (html, "<head><title>" & ada_name & "</title>");
    --  !! <meta name="keywords" content="...">
    --  !! <link rel="Shortcut Icon" href="...">
    Put_Line (html, "</head><body>");
    Put_Line
       (html, "<div align=""center""><h2><font face=""Calibri, Arial, Trebuchet MS"">source file : " &
        ada_name &
        "</font></h2></div><hr>");
    --  !!  Custom header
    Put_Line (html, "<pre>");
    Put_Line (html, "<font size=""2"" face=""Consolas, Monaco, Lucida Console, Courier New"">");
    Scan;
    Put_Line (html, "</font>");
    Put_Line (html, "</pre>");
    Close (ada_source);
    --  !!  Custom footer
    Put_Line (html, "</body></html>");
    Close (html);
  exception
    when Ada.Text_IO.Name_Error =>
      Put_Line (Current_Error, "File with name """ & ada_name & """ not found.");
  end Process_Source;

  use Ada.Command_Line, Ada.Directories, Ada.Strings.Unbounded, Ada.Text_IO;

  main, path : ALI_Parse.String_Sets.Set;
  all_paths : Unbounded_String;
  myself : constant String := "gnathtml.adb";
  main_html : File_Type;

begin
  if Argument_Count = 0 then
    if Exists ("src/" & myself) then
      --  Demo with this file
      main.Include (myself);
      path.Include ("src,obj");
    else
      Put_Line (Current_Error, "GNATHTML - Create a HTML-based Ada source browser");
      New_Line (Current_Error);
      Put_Line (Current_Error, "Usage:");
      Put_Line (Current_Error, "  gnathml [switches] main_file1.adb main_file2.adb ...");
      Put_Line (Current_Error, "     -Ipath  : Add search path for sources and .ali files (object directories)");
      Put_Line (Current_Error, "     -L      : Links to and within the Ada library (needs path to its sources)");
      return;
    end if;
  end if;

  for i in 1 .. Argument_Count loop
    declare
      arg : constant String := Argument (i);
    begin
      case arg (arg'First) is
        when '-' =>
          case arg (arg'First + 1) is
            when 'I'    => path.Include (arg (arg'First + 2 .. arg'Last));
            when 'L'    => stdlib := True;
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

  Create_Path (html_dir);
  Create (main_html, Out_File, html_dir & "index.html");
  Put_Line (main_html, "<html><head><title>Source Browser</title>");
  --  !! <meta name="keywords" content="...">
  --  !! <link rel="Shortcut Icon" href="...">
  Put_Line (main_html, "</head><body>");
  Put_Line
    (main_html,
     "<div align=""center""><h2><font face=""Calibri, Arial, Trebuchet MS"">" &
     "Source Browser</font></h2></div><hr>");
  Put_Line (main_html, "You can start browsing the following sources:</font>");
  Put_Line (main_html, "<font size=""2"" face=""Consolas, Monaco, Lucida Console, Courier New""><ul>");

  for m of main loop
    ali.Gather_Cross_References
      (ada_root_name => m,
       object_path   => To_String (all_paths));
    Put_Line (main_html, "<li><a href=""" & To_HTML_Name (m) & """>" & m & "</a>");
  end loop;

  Put_Line (main_html, "</ul></font></body>");
  Close (main_html);

  links    := ali.Get_Links;
  entities := ali.Get_Entities;

  for an of ali.Get_Ada_File_Names loop
    Process_Source (an, ALI_Parse.Search_File (an, To_String (all_paths)));
  end loop;

end GNATHTML;
