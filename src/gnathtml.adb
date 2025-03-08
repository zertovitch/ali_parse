with ALI_Parse;

with Ada.Characters.Handling,
     Ada.Command_Line,
     Ada.Directories,
     Ada.Integer_Text_IO,
     Ada.Strings.Fixed,
     Ada.Strings.Unbounded,
     Ada.Text_IO;

procedure GNATHTML is
  ali   : ALI_Parse.ALI_Obj;
  links, entities : ALI_Parse.String_to_String_Maps.Map;
  stdlib : Boolean := False;

  function To_HTML_Name (ada_name : String) return String is (ada_name & ".html");

  function Is_Lib_Name (ada_name : String) return Boolean is
  (ada_name (ada_name'First .. ada_name'First + 1) in "a-" | "g-" | "i-" | "s-" or else
   ada_name in "ada.ads" | "gnat.ads" | "interfac.ads" | "system.ads");

  use Ada.Characters.Handling, Ada.Strings.Unbounded;

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

  function HTML_Escape_Codes (s : String) return String is
    res : Unbounded_String;
  begin
    for c of s loop
      case c is
        when '&' => res := res & "&amp;";
        when '<' => res := res & "&lt;";
        when '>' => res := res & "&gt;";
        when others => res := res & c;
      end case;
    end loop;
    return To_String (res);
  end HTML_Escape_Codes;

  function URL_ize (s : String) return String is
    --  For testing in this comment: https://en.wikipedia.org/wiki/Regular_expression or https://regex101.com/
    res, url : Unbounded_String;
    i : Positive := s'First;
    j : Natural;
    use Ada.Strings.Fixed;
  begin
    while i <= s'Last loop
      j := Integer'Max (Index (s, "http://", i), Index (s, "https://", i));
      if j = 0 then
        res := res & s (i .. s'Last);
        i := s'Last + 1;
        exit;
      else
        res := res & s (i .. j - 1) & "<a target=_blank href=""";
        url := Null_Unbounded_String;
        while j <= s'Last and then
          s (j) in
            'a' .. 'z' | 'A' .. 'Z' | '_' | '0' .. '9' |
            '.' | '/' | '?' | '~' | '=' | '\' | '+' | '-' | ':' | '%'
        loop
          Append (url, s (j));
          j := j + 1;
        end loop;
        i := j;
        res := res & url & """>" & url & "</a>";
      end if;
    end loop;
    return To_String (res);
  end URL_ize;

  procedure Insert_Snippet (snippet_name : Unbounded_String; destination : in out Ada.Text_IO.File_Type) is
    use Ada.Text_IO;
    snippet : File_Type;
  begin
    Open (snippet, In_File, To_String (snippet_name));
    while not End_Of_File (snippet) loop
      Put_Line (destination, Get_Line (snippet));
    end loop;
    Close (snippet);
  exception
    when Name_Error => null;
  end Insert_Snippet;

  font_var_width : constant String := "<font face=""Calibri, Arial, Trebuchet MS"">";
  font_fix_width : constant String := "<font size=""2"" face=""Consolas, Monaco, Lucida Console, Courier New"">";

  html_dir : Unbounded_String := To_Unbounded_String ("html");
  bgcolor, snippet_head, snippet_top, snippet_bottom : Unbounded_String;

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
            is_lib : constant Boolean := Is_Lib_Name (ada_tgt);
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

        procedure Ada_Code_Chunk is
          str_lit : Unbounded_String;
        begin
          case l (x) is

            when 'a' .. 'z' | 'A' .. 'Z' =>
              stop := x;
              loop
                x := x + 1;
                exit when x > l'Last;
                exit when l (x) not in 'a' .. 'z' | 'A' .. 'Z' | '_' | '0' .. '9';
                stop := x;
              end loop;
              if Is_Keyword (l (start .. stop)) then
                Put (html, "<b>" & l (start .. stop) & "</b>");
              else
                Put (html, l (start .. stop));
              end if;

            when '-' =>
              --  We buffer the '-'.
              x := x + 1;

            when '"' =>
              Put (html, '"');
              Put (html, "<font color=""darkred"">");
              loop
                x := x + 1;
                exit when x > l'Last;  --  Unfinished string literal.
                if l (x) = '"' then
                  x := x + 1;
                  exit when x > l'Last;    --  String literal finishes line.
                  exit when l (x) /= '"';  --  String literal followed by something else.
                  --  At this point l (x) = '"' : doubled double-quote.
                  Append (str_lit, l (x));
                end if;
                Append (str_lit, l (x));
              end loop;
              Put (html, URL_ize (HTML_Escape_Codes (To_String (str_lit))));
              Put (html, "</font>");
              Put (html, '"');

            when ''' =>
              --  We process ['] just for the sake of the ['"'] case: avoid
              --  taking the double-quote as the start of a string literal.
              Put (html, ''');
              x := x + 1;
              if x > l'Last then
                null;  --  Unfinished character literal: only "'".
              else
                Put (html, HTML_Escape_Codes ("" & l (x)));
                x := x + 1;
                if x > l'Last then
                  --  Unfinished character literal or attribute: only "'?" at the end of the line.
                  null;
                elsif l (x) = ''' then
                  --  We just had a full character literal, '?'
                  Put (html, ''');
                  x := x + 1;
                else
                  --  An attribute ("'xyz"), a qualified expression ("'(expr"), or something wrong...
                  --  We continue with the main scanning.
                  null;
                end if;
              end if;

            when others =>
              Put (html, HTML_Escape_Codes ("" & l (x)));
              x := x + 1;

          end case;
          if link then
            Put (html, "</a>");
          end if;
        end Ada_Code_Chunk;

        procedure Comment is
        begin
          Put (html, "<font color=""green""><em>-");
          Put (html, URL_ize (HTML_Escape_Codes (l (x .. l'Last))));
          Put (html, "</em></font>");
          x := l'Last + 1;
        end Comment;

        minuses : Natural := 0;

      begin
        ln := ln + 1;
        --  Display the line number:
        Put (html, "<font color=""lightgray"">");
        Put (html, ln, 4);
        Put (html, "</font>  ");
        while x <= l'Last loop
          start := x;
          if l (x) = '-' then
            minuses := minuses + 1;
          else
            if minuses = 1 then
              Put (html, '-');  --  Output the delayed '-'
            end if;
            minuses := 0;
          end if;
          if minuses = 2 then
            Comment;
          else
            Add_Links;
            Ada_Code_Chunk;
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
    if Is_Lib_Name (ada_name) and not stdlib then
      --  Since we don't hyperlink to the run-time library,
      --  we don't looke for the files either.
      return;
    end if;
    Open (ada_source, In_File, full_ada_name);
    Create (html, Out_File, To_String (html_dir) & '/' & To_HTML_Name (ada_name));
    Put_Line (html, "<html>");
    Put_Line (html, "<head><title>" & ada_name & "</title>");
    Insert_Snippet (snippet_head, html);
    Put_Line (html, "</head><body" & (if bgcolor = "" then "" else " bgcolor=" & To_String (bgcolor)) & '>');
    Insert_Snippet (snippet_top, html);
    Put_Line
       (html, "<div align=""center""><h2>" & font_var_width & "Source file : " &
        ada_name &
        "</font></h2></div><hr>");
    Put_Line (html, "<pre>");
    Put_Line (html, font_fix_width);
    Scan;
    Put_Line (html, "</font>");
    Put_Line (html, "</pre>");
    Close (ada_source);
    Insert_Snippet (snippet_bottom, html);
    Put_Line (html, "</body></html>");
    Close (html);
    Put_Line ("Processed """ & ada_name & """.");
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
      New_Line (Current_Error);
      Put_Line (Current_Error, "Switches:");
      Put_Line (Current_Error, "     -bcolor : background color");
      Put_Line (Current_Error, "     -ifile  : HTML snippet to be inserted in the <head> part");
      Put_Line (Current_Error, "     -jfile  : HTML snippet to be inserted at the top of pages");
      Put_Line (Current_Error, "     -kfile  : HTML snippet to be inserted at the bottom of pages");
      Put_Line (Current_Error, "     -Ipath  : Add search path for sources and .ali files (object directories)");
      Put_Line (Current_Error, "     -L      : Show links to and within the Ada library (needs path to its sources)");
      Put_Line (Current_Error, "     -odir   : Name of the directory where the html files will be saved. Default is 'html'");
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
            when 'I'    => path.Include (op);
            when 'L'    => stdlib := True;
            when 'b'    => bgcolor        := To_Unbounded_String (op);
            when 'i'    => snippet_head   := To_Unbounded_String (op);
            when 'j'    => snippet_top    := To_Unbounded_String (op);
            when 'k'    => snippet_bottom := To_Unbounded_String (op);
            when 'o'    => html_dir := To_Unbounded_String (op);
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

  Create_Path (To_String (html_dir));
  Create (main_html, Out_File, To_String (html_dir) & "/index.html");
  Put_Line (main_html, "<html><head><title>Source Browser</title>");
  Insert_Snippet (snippet_head, main_html);
  Put_Line (main_html, "</head><body" & (if bgcolor = "" then "" else " bgcolor=" & To_String (bgcolor)) & '>');
  Insert_Snippet (snippet_top, main_html);
  Put_Line (main_html, "<div align=""center""><h2>" & font_var_width & "Source Browser</font></h2></div><hr>");
  Put_Line (main_html, font_var_width & "You can start browsing the following sources:</font>");
  Put_Line (main_html, font_fix_width & "<ul>");

  for m of main loop
    ali.Gather_Cross_References
      (ada_root_name => m,
       object_path   => To_String (all_paths));
    Put_Line (main_html, "<li><a href=""" & To_HTML_Name (m) & """>" & m & "</a>");
  end loop;

  Put_Line (main_html, "</ul></font>");
  Insert_Snippet (snippet_bottom, main_html);
  Put_Line (main_html, "</body>");
  Close (main_html);

  links    := ali.Get_Links;
  entities := ali.Get_Entities;

  for an of ali.Get_Ada_File_Names loop
    Process_Source (an, ALI_Parse.Search_File (an, To_String (all_paths)));
  end loop;

end GNATHTML;
