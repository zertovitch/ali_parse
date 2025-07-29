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

  package String_to_Character_Maps is new
    Ada.Containers.Indefinite_Ordered_Maps
      (Key_Type     => String,
       Element_Type => Character);

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
  --  Key = Definition locator, Element = entity's identifier.
  --
  function Get_Entities (ali : ALI_Obj) return String_to_String_Maps.Map;

  --  Get entity types as found by Gather_Cross_References.
  --  Key = Definition locator, Element = entity code (1 character).
  --
  function Get_Entity_Types (ali : ALI_Obj) return String_to_Character_Maps.Map;

  --  Get reference counts as found by Gather_Cross_References.
  --  Key = Definition locator, Element = number of references.
  --
  function Get_Reference_Counts (ali : ALI_Obj) return String_to_Integer_Maps.Map;

  -------------------------------
  --  Miscellaneous utilities  --
  -------------------------------

  function Is_Lib_Name (ada_name : String) return Boolean;

  --  Search a file using a search path.
  --  If the file exists in current directory, `simple_file_name` is returned.
  --  If the file doesn't exist, an empty string is returned.
  --
  function Search_File (simple_file_name, path : String) return String;

  function Verbose_Entity_Type (entity_type : Character) return String;

  -----------------------------------------------------------------
  --  Information about this package - e.g., for an "about" box  --
  -----------------------------------------------------------------

  version : constant String := "1.0";
  --  Hopefully the latest version can be acquired from one of those URLs:
  web1 : constant String := "https://alire.ada.dev/crates/ali_parse";
  web2 : constant String := "https://github.com/zertovitch/ali_parse";
  web3 : constant String := "https://sourceforge.net/projects/ali_parse/";

private

  --  Locator convention: "file_name line col".

  type ALI_Obj is tagged record
    ada_names    : String_Sets.Set;
    links        : String_to_String_Maps.Map;     --  Key = From (a reference), Element = To (the definition locator)
    entities     : String_to_String_Maps.Map;     --  Key = Definition locator, Element = Entity identifier
    entity_types : String_to_Character_Maps.Map;  --  Key = Definition locator, Element = Entity type
    ref_counts   : String_to_Integer_Maps.Map;    --  Key = Definition locator, Element = Number of references
    visited_alis : String_Sets.Set;
  end record;

end ALI_Parse;
