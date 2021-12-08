with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

with Json.Parsers;
with Json.Types;

package Hellish_Web.Bencoder is
   type Bencode_Value is abstract tagged record
      Encoded : Unbounded_String;
   end record;

   package Json_Types is new JSON.Types
     (Integer_Type => Integer, Float_Type => Float);
   package Json_Parsers is new Json.Parsers
     (Json_Types);

   function Encode(Value : Json_Types.Json_value) return Bencode_Value'Class;
   function Image(Value : Bencode_Value) return String;

   type Bencode_Integer is new Bencode_Value with record
      Value : Integer;
   end record;

   type Bencode_String is new Bencode_Value with record
      Value : Unbounded_String;
   end record;

   type Bencode_List is new Bencode_Value with record
      Value : Json_Types.Json_Value (Json_Types.Array_Kind);
   end record;

   type Bencode_Dictionary is new Bencode_Value with record
      Value : Json_Types.Json_Value (Json_Types.Object_Kind);
   end record;

   function Encode(Value : String) return Bencode_String;
   function Encode(Value : Integer) return Bencode_Integer;
end Hellish_Web.Bencoder;
