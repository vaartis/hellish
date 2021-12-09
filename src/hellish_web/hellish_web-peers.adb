with Ada.Text_Io; use Ada.Text_Io;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

package body Hellish_Web.Peers is
   protected body Protected_Map is
      procedure Add(Info_Hash : String; Joined_Peer : Peer) is
      begin
         if not Torrent_Map.Contains(Info_Hash) then
            declare
               Peer_Vector : Peer_Vectors.Vector;
            begin
               Peer_Vector.Append(Joined_Peer);

               Torrent_Map.Include(Info_Hash, Peer_Vector);
            end;
         else
            for Peer of Torrent_Map(Info_Hash) loop
               if Peer.Peer_Id = Joined_Peer.Peer_Id then
                  goto Do_Not_Add;
               end if;

               Torrent_Map(Info_Hash).Append(Joined_Peer);
               <<Do_Not_Add>>
            end loop;
         end if;

         Put_Line("Peer IP: " & To_String(Joined_Peer.Ip));

         Put_Line("Peer " & To_String(Joined_Peer.Peer_Id) & " joined " & Info_Hash);
         Put_Line("There are now " & Trim(Torrent_Map(Info_Hash).Length'Image, Ada.Strings.Left) & " peers");
      end Add;

      procedure Remove(Info_Hash : String; Peer_id : Unbounded_String) is
      begin
         if Torrent_Map.Contains(Info_Hash) then
            declare
               Peer_Vector : Torrent_Hashed_Maps.Reference_Type := Torrent_Map.Reference(Info_Hash);
            begin
               for I in Peer_Vector.Iterate loop
                  if Peer_Vector(I).Peer_Id = Peer_Id then
                     Peer_Vector.Delete(Peer_Vectors.To_Index(I));

                     Put_Line("Peer " & To_String(Peer_Id) & " left " & Info_Hash);
                     Put_Line("There are now " & Trim(Peer_Vector.Length'Image, Ada.Strings.Left) & " peers");

                     exit;
                  end if;
               end loop;

               if Natural(Peer_Vector.Length) = 0 then
                  Torrent_Map.Delete(Info_Hash);
               end if;
            end;
         end if;
      end Remove;

      function Contains(Info_Hash : String) return Boolean is (Torrent_Map.Contains(Info_Hash));
      function Constant_Reference(Info_Hash : String) return Torrent_Hashed_Maps.Constant_Reference_Type is
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

      Bencode_Maps.Include(Result_Map, "interval", Encode(10));
      Bencode_Maps.Include(Result_Map, "peers", Encode(Peer_Bencodes));

      Put_Line("Sending " & To_String(Encode(Result_Map).Element.Encoded));

      return Encode(Result_Map);
   end;
end Hellish_Web.Peers;
