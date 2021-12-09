with Ada.Text_Io; use Ada.Text_Io;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

package body Hellish_Web.Peers is
   protected body Protected_Map is
      procedure Add(Info_Hash : String; Joined_Peer : Peer) is
         Peer_Id : String := To_String(Joined_Peer.Peer_Id);
      begin
         if not Torrent_Map.Contains(Info_Hash) then
            declare
               Peer_Map : Peer_Maps.Map;
            begin
               Peer_Map.Include(Peer_Id, Joined_Peer);
               Torrent_Map.Include(Info_Hash, Peer_Map);
            end;
         else
            if not Torrent_Map(Info_Hash).Contains(Peer_Id) then
               Torrent_Map(Info_Hash).Include(Peer_Id, Joined_Peer);
            else
               Torrent_Map(Info_Hash).Replace(Peer_Id, Joined_Peer);
            end if;
         end if;

         Put_Line("Peer " & To_String(Joined_Peer.Peer_Id) & " joined " & Info_Hash);
         Put_Line("There are now " & Trim(Torrent_Map(Info_Hash).Length'Image, Ada.Strings.Left) & " peers");
      end Add;

      procedure Remove(Info_Hash : String; Peer_Id : Unbounded_String) is
      begin
         if Torrent_Map.Contains(Info_Hash) then
            declare
               Peer_Map : Torrent_Maps.Reference_Type := Torrent_Map.Reference(Info_Hash);
            begin
               Peer_Map.Exclude(To_String(Peer_Id));

               Put_Line("Peer " & To_String(Peer_Id) & " LEFT " & Info_Hash);
               Put_Line("There are now " & Trim(Peer_Map.Length'Image, Ada.Strings.Left) & " peers");

               if Natural(Length(Peer_Map)) = 0 then
                  Torrent_Map.Delete(Info_Hash);
               end if;
            end;
         end if;
      end Remove;

      function Contains(Info_Hash : String) return Boolean is (Torrent_Map.Contains(Info_Hash));
      function Constant_Reference(Info_Hash : String) return Torrent_Maps.Constant_Reference_Type is
        (Torrent_Map.Constant_Reference(Info_Hash));
   end Protected_Map;

   function Encode_Hash_Peers_Response(Info_Hash : String) return Bencode_Value_Holders.Holder is
      Result_Map : Bencode_Maps.Map;
      Peer_Bencodes : Bencode_Vectors.Vector;
   begin
      if Protected_Map.Contains(Info_Hash) then
         for The_Peer of Protected_Map.Constant_Reference(Info_Hash) loop
            declare
               Peer_Bencode : Bencode_Maps.Map;
            begin
               Bencode_Maps.Include(Peer_Bencode, "peer id", Encode(To_String(The_Peer.Peer_id)));
               Bencode_Maps.Include(Peer_Bencode, "port", Encode(The_Peer.Port));
               Bencode_Maps.Include(Peer_Bencode, "ip", Encode(To_String(The_Peer.Ip)));

               Peer_Bencodes.Append(Encode(Peer_Bencode));
            end;
         end loop;
      end if;

      Bencode_Maps.Include(Result_Map, "interval", Encode(30));
      Bencode_Maps.Include(Result_Map, "peers", Encode(Peer_Bencodes));

      return Encode(Result_Map);
   end;
end Hellish_Web.Peers;
