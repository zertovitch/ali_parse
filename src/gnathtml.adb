--  W.I.P. !

with ALI_Parse;

with HAC_Sys.Compiler,
     HAC_Sys.Co_Defs,
     HAC_Sys.Defs,
     HAC_Sys.Files,
     HAC_Sys.Scanner;

with Ada.Characters.Handling,
     Ada.Command_Line,
     Ada.Directories,
     Ada.Integer_Text_IO,
     Ada.Strings.Fixed,
     Ada.Text_IO.Text_Streams;

procedure GNATHTML is

  ali   : ALI_Parse.ALI_Obj;
  links, entities : ALI_Parse.String_to_String_Maps.Map;

  function To_HTML_Name (ada_name : String) return String is (ada_name & ".html");

  procedure Process_Source (ada_name, full_ada_name : String) is
    use Ada.Text_IO;
    ada_source, html : File_Type;

    procedure Scan is
      CD : HAC_Sys.Co_Defs.Compiler_Data;
      use
        Ada.Characters.Handling, Ada.Strings, Ada.Strings.Fixed,
        HAC_Sys.Compiler, HAC_Sys.Co_Defs, HAC_Sys.Defs, HAC_Sys.Scanner;
      source_stream : constant HAC_Sys.Co_Defs.Source_Stream_Access :=
        HAC_Sys.Files.Root_Stream_Class_Access
          (Ada.Text_IO.Text_Streams.Stream (ada_source));

      s_col, s_line : Integer := 1;

      procedure Process_Symbol is
        link : Boolean := False;
      begin
        while s_line < CD.CUD.location.line loop
          New_Line (html);
          s_line := s_line + 1;
          s_col  := 1;
        end loop;
        while s_col < CD.CUD.location.column_start loop
          Put (html, ' ');
          s_col := s_col + 1;
        end loop;
        declare
          key : constant String := ada_name & s_line'Image & s_col'Image;
        begin
          if entities.Contains (key) then
            Put
              (html, "<a name=""" & Trim (s_line'Image, Left) & '_' & Trim (s_col'Image, Left) & """></a>");
          end if;
          if links.Contains (key) then
            declare
              dc : constant String := links.Element (key);
              sp : constant Positive := Ada.Strings.Fixed.Index (dc, " ");
              target : constant String := To_HTML_Name (dc (dc'First .. sp - 1));
              lin_col : String := dc (sp + 1 .. dc'Last);
            begin
              for c of lin_col loop
                if c = ' ' then c := '_'; end if;
              end loop;
              Put (html, "<a href=""" & target & '#' & lin_col & """>");
              link := True;
            end;
          end if;
        end;
        case CD.Sy is
          when ABORT_Symbol .. XOR_Symbol =>
            Put (html, "<b>" & A2S (CD.Id_with_case) & "</b>");
          when others =>
            Put (html, CD.CUD.input_line (CD.CUD.location.column_start .. CD.CUD.location.column_stop));
        end case;
        s_col := s_col + CD.CUD.location.column_stop - CD.CUD.location.column_start + 1;
        if link then
          Put (html, "</a>");
        end if;
      end Process_Symbol;

    begin
      Set_Source_Stream (CD.CUD, source_stream, ada_name);
      Init_for_new_Build (CD);
      Process_Symbol;
      while Source_Buffer_has_Data (CD.CUD) loop
        In_Symbol (CD);
        Process_Symbol;
      end loop;
    exception
      when End_Error =>
        null;
    end Scan;

  begin
    Open (ada_source, In_File, full_ada_name);
    Create (html, Out_File, To_HTML_Name (ada_name));
    Put_Line (html, "<html>");
    Put_Line (html, "<head><title>" & ada_name & "</title>");
    --  !! <meta name="keywords" content="...">
    --  !! <link rel="Shortcut Icon" href="...">
    Put_Line
       (html, "<div align=""center""><h2><font face=""arial, trebuchet ms"">source file : " &
        ada_name &
        "</font></h2></div><hr>");
    Put_Line (html, "</head>");
    Put_Line (html, "<body>");
    --  !!  Custom header
    Put_Line (html, "<pre>");

    Scan;
    Put_Line (html, "</pre>");
    Close (ada_source);
    --  !!  Custom footer
    Put_Line (html, "</body></html>");
    Close (html);
  exception
    when Name_Error =>
      Put_Line (Current_Error, "File with name """ & ada_name & """ not found.");
  end Process_Source;

begin

  ali.Gather_Cross_References
    (ada_root_name => "p.adb",
     object_path   => "");

  links    := ali.Get_Links;
  entities := ali.Get_Entities;

  for an of ali.Get_Ada_File_Names loop
    Process_Source (an, an);  --  !! Find name in path
  end loop;

end GNATHTML;
