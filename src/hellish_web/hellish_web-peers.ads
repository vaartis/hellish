with Ada.Strings.Hash;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Indefinite_Hashed_Maps;

with Hellish_Web.Bencoder; use Hellish_Web.Bencoder;

with Orm; use Orm;

package Hellish_Web.Peers is
   type Peer is record
      Peer_Id: Unbounded_String;
      Ip: Unbounded_String;
      Port: Positive;
      Uploaded: Long_Long_Integer;
      Downloaded: Long_Long_Integer;
      Left: Long_Long_Integer;
   end record;

   package Peer_Maps is new
     Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type        => String,
        Element_Type    => Peer,
        Hash            => Ada.Strings.Hash,
        Equivalent_Keys => "=");
   use Peer_Maps;

   package Torrent_Maps is new
     Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type        => String,
        Element_Type    => Peer_Maps.Map,
        Hash            => Ada.Strings.Hash,
        Equivalent_Keys => "=");
   use Torrent_Maps;

   type Scrape_Stat_Data is record
      Complete: Natural := 0;
      Downloaded: Natural := 0;
      Incomplete: Natural := 0;
   end record;

   type Response_Options is record
      Compact: Boolean;
      Num_Want: Natural;
   end record;

   type Saved_Stats is record
      Downloaded: Natural;
   end record;

   package Saved_Stats_Maps is new
     Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type        => String,
        Element_Type    => saved_stats,
        Hash            => Ada.Strings.Hash,
        Equivalent_Keys => "=");
   use Saved_Stats_Maps;

   type Total_Stats is record
      Known: Natural := 0;
      Downloaded: Natural := 0;
      Seeders: Natural := 0;
      Leechers: Natural := 0;
   end record;

   protected Protected_Map is
      procedure Add(Info_Hash : String; Joined_Peer : Peer; The_User : Detached_User'Class);
      procedure Remove(Info_Hash : String; Peer_id : Unbounded_String);

      function Encode_Hash_Peers_Response(Info_Hash : String; From_Id : String;
                                          Options : Response_Options) return Bencode_Value_Holders.Holder;

      function Scrape_Stats(Info_Hash : String) return Scrape_Stat_Data;

      procedure Downloaded(Info_Hash : String);
      function Total_Stat_Data return Total_Stats;
   private
      function Ip_Port_Bytes(From_Peer : Peer) return String;

      Torrent_Map : Torrent_Maps.Map;
      Saved_Stats_Map : Saved_Stats_Maps.Map;
   end Protected_Map;
end Hellish_Web.Peers;
