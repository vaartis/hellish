with Ada.Strings.Hash;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Containers.Vectors;

with Hellish_Web.Bencoder; use Hellish_Web.Bencoder;

package Hellish_Web.Peers is
   type Peer is record
      Peer_Id: Unbounded_String;
      Ip: Unbounded_String;
      Port: Positive;
      Uploaded: Natural;
      Downloaded: Natural;
      Left: Natural;
   end record;

   package Peer_Vectors is new Ada.Containers.Vectors
     (Index_Type => Natural, Element_Type => Peer);
   use Peer_Vectors;

   package Torrent_Hashed_Maps is new
     Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type        => String,
        Element_Type    => Vector,
        Hash            => Ada.Strings.Hash,
        Equivalent_Keys => "=");
   use Torrent_Hashed_Maps;

   protected Protected_Map is
      procedure Add(Info_Hash : String; Joined_Peer : Peer);
      procedure Remove(Info_Hash : String; Peer_id : Unbounded_String);

      function Contains(Info_Hash : String) return Boolean;
      function Constant_Reference(Info_Hash : String) return Torrent_Hashed_Maps.Constant_Reference_Type;
   private
      Torrent_Map : Map;
   end;

   function Encode_Hash_Peers_Response(Info_Hash : String) return Bencode_Value_Holders.Holder;
end Hellish_Web.Peers;
