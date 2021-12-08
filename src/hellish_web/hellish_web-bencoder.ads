with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

with Json.Parsers;
with Json.Types;

package Hellish_Web.Bencoder is
   package Json_Types is new JSON.Types
     (Integer_Type => Integer, Float_Type => Float);
   package Json_Parsers is new Json.Parsers
     (Json_Types);

   use Json_Types;

   type Bencode_Value ( Kind : Value_kind )is tagged record
      Encoded : Unbounded_String;
      Value : Json_Value (Kind);
   end record;

   function Encode(Value : Json_Types.Json_value) return Bencode_Value;
   function Image(Value : Bencode_Value) return String;

   Encode_Error : exception;
end Hellish_Web.Bencoder;
