with Ada.Strings.Hash;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Containers.Vectors;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Containers.Indefinite_Holders;

package Hellish_Web.Bencoder is
   type Bencode_Value is abstract tagged record
      Encoded : Unbounded_String;
   end record;

   package Bencode_Value_Holders is new Ada.Containers.Indefinite_Holders(Bencode_Value'Class);
   use Bencode_Value_Holders;

   package Bencode_Vectors is new Ada.Containers.Vectors
     (Index_Type => Natural, Element_Type => Bencode_Value_Holders.Holder);
   use Bencode_Vectors;

   package Bencode_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type => String,
      Element_Type => Bencode_Value_Holders.Holder,
      Hash => Ada.Strings.Hash,
      Equivalent_Keys => "=");
   use Bencode_Maps;

   type Bencode_String is new Bencode_Value with record
      Value : Unbounded_String;
   end record;

   type Bencode_Integer is new Bencode_Value with record
      Value : Natural;
   end record;

   type Bencode_List is new Bencode_Value with record
      Value : Bencode_Vectors.Vector;
   end record;

   type Bencode_Dict is new Bencode_Value with record
      Value : Bencode_Maps.Map;
   end record;

   function Encode(Value : String) return Holder;
   function Encode(Value : Natural) return Holder;
   function Encode(Value : Bencode_Vectors.Vector) return Holder;
   function Encode(The_Map : Bencode_Maps.Map) return Holder;

   procedure Include(The_Map : in out Bencode_Dict; Key : String; Value : Bencode_Value_Holders.Holder);

   function With_Failure_Reason(reason : String) return Holder;

   Encode_Error : exception;
end Hellish_Web.Bencoder;
