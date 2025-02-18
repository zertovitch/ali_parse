--  W.I.P. !

with ALI_Parse;

with Ada.Text_IO;

procedure GNATHTML is
  ali   : ALI_Parse.ALI_Obj;
  links : ALI_Parse.String_to_String_Maps.Map;
begin

  ali.Gather_Cross_References
    (ada_root_name => "p.adb",
     object_path   => "");

  links := ali.Get_Links;

  for an of ali.Get_Ada_File_Names loop
    null;
  end loop;

end GNATHTML;
