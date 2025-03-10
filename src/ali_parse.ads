with Ada.Containers.Indefinite_Ordered_Maps,
     Ada.Containers.Indefinite_Ordered_Sets;

package ALI_Parse is

  type ALI_Obj is tagged private;

  type Filtering_Flavor is
    (source_browser,  --  For GNATHTML or any "hypertext"-like application.
     gnat_studio);    --  Try to be the closest to "Find All References" in GNAT Studio.

  procedure Gather_Cross_References
    (ali           : in out ALI_Obj;
     ada_root_name : in     String;
     object_path   : in     String;
     flavor        : in     Filtering_Flavor;
     from_scratch  : in     Boolean := True;
     recursive     : in     Boolean := True);

  package String_to_Integer_Maps is new
    Ada.Containers.Indefinite_Ordered_Maps
      (Key_Type     => String,
       Element_Type => Integer);

  package String_to_String_Maps is new
    Ada.Containers.Indefinite_Ordered_Maps
      (Key_Type     => String,
       Element_Type => String);

  package String_Sets is new
    Ada.Containers.Indefinite_Ordered_Sets (String);

  --  Get all Ada file names in the library, as found by Gather_Cross_References.
  --
  function Get_Ada_File_Names (ali : ALI_Obj) return String_Sets.Set;

  --  Get links under the locator form "file_name line col", as found by Gather_Cross_References.
  --  Key = From (a reference), Element = To = "locator code".
  --    locator is for the definition.
  --    code is the link code.
  --
  function Get_Links (ali : ALI_Obj) return String_to_String_Maps.Map;

  --  Get entities as found by Gather_Cross_References.
  --  Key = Definition locator, Element = "id code".
  --  id = the entity's identifier, code = entity code (1 character).
  --
  function Get_Entities (ali : ALI_Obj) return String_to_String_Maps.Map;

  --  Get reference counts as found by Gather_Cross_References.
  --  Key = Definition locator, Element = number of references.
  --
  function Get_Reference_Counts (ali : ALI_Obj) return String_to_Integer_Maps.Map;

  -------------------------------
  --  Miscellaneous utilities  --
  -------------------------------

  --  Search a file using a search path.
  --  If the file exists in current directory, `simple_file_name` is returned.
  --
  function Search_File (simple_file_name, path : String) return String;

private

  --  Locator convention: "file_name line col".

  type ALI_Obj is tagged record
    ada_names    : String_Sets.Set;
    links        : String_to_String_Maps.Map;   --  Key = From (a reference), Element = To (the definition locator)
    entities     : String_to_String_Maps.Map;   --  Key = Definition locator, Element = Entity identifier
    ref_counts   : String_to_Integer_Maps.Map;  --  Key = Definition locator, Element = Number of references
    visited_alis : String_Sets.Set;
  end record;

end ALI_Parse;
