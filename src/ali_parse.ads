with Ada.Containers.Indefinite_Ordered_Maps,
     Ada.Containers.Indefinite_Ordered_Sets;

package ALI_Parse is

  type ALI_Obj is tagged private;

  procedure Gather_Cross_References
    (ali           : in out ALI_Obj;
     ada_root_name : in     String;
     object_path   : in     String;
     from_scratch  : in     Boolean := True);

  package String_Link_Maps is new
    Ada.Containers.Indefinite_Ordered_Maps
      (Key_Type        => String,   --  From (a reference)
       Element_Type    => String);  --  To (the definition)

  package String_Sets is new
    Ada.Containers.Indefinite_Ordered_Sets (String);

  --  Get all Ada file names in the library, as found by Gather_Cross_References.
  --
  function Get_Ada_Names (ali : ALI_Obj) return String_Sets.Set;

  --  Get links under the form "file_name line col", as found by Gather_Cross_References.
  --
  function Get_Links (ali : ALI_Obj) return String_Link_Maps.Map;

private

  type ALI_Obj is tagged record
    ada_names    : String_Sets.Set;
    links        : String_Link_Maps.Map;
    visited_alis : String_Sets.Set;
  end record;

end ALI_Parse;
