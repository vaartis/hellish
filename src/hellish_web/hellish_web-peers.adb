with Ada.Text_Io; use Ada.Text_Io;
with Ada.Integer_Text_Io; use Ada.Integer_Text_Io;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Interfaces; use Interfaces;
with System; use System;

with Hellish_Web.Database;

package body Hellish_Web.Peers is
   protected body Protected_Map is
      procedure Add(Info_Hash : String; Joined_Peer : Peer; The_User : Detached_User'Class) is
         Peer_Id : String := To_String(Joined_Peer.Peer_Id);

         Uploaded_Diff : Natural := 0;
         Downloaded_Diff : Natural := 0;
      begin
         if not Torrent_Map.Contains(Info_Hash) then
            declare
               Peer_Map : Peer_Maps.Map;
            begin
               Peer_Map.Include(Peer_Id, Joined_Peer);
               Torrent_Map.Include(Info_Hash, Peer_Map);
               -- Also add it to the stats map
               Saved_Stats_Map.Include(Info_Hash, (Downloaded => 0));

               -- Initial upload/download data
               Uploaded_Diff := Joined_Peer.Uploaded;
               Downloaded_Diff := Joined_Peer.Downloaded;
            end;
         else
            if not Torrent_Map(Info_Hash).Contains(Peer_Id) then
               -- Just joined
               Uploaded_Diff := Joined_Peer.Uploaded;
               Downloaded_Diff := Joined_Peer.Downloaded;

               Torrent_Map(Info_Hash).Include(Peer_Id, Joined_Peer);
            else
               -- Update from existing
               Uploaded_Diff := Joined_Peer.Uploaded - Torrent_Map(Info_Hash)(Peer_Id).Uploaded;
               Downloaded_Diff := Joined_Peer.Downloaded - Torrent_Map(Info_Hash)(Peer_Id).Downloaded;

               Torrent_Map(Info_Hash).Replace(Peer_Id, Joined_Peer);
            end if;
         end if;

         Database.Update_Torrent_Up_Down(The_User, Info_Hash, Uploaded_Diff, Downloaded_Diff);

         Put_Line("Peer """ & To_String(Joined_Peer.Peer_Id) & """ JOINED """ & Info_Hash & """ from " &
                    To_String(Joined_Peer.Ip) & ":" & Trim(Joined_Peer.Port'Image, Ada.Strings.Left));
         Put_Line("There are now " & Trim(Torrent_Map(Info_Hash).Length'Image, Ada.Strings.Left) & " peers");
      end Add;

      procedure Remove(Info_Hash : String; Peer_Id : Unbounded_String) is
      begin
         if Torrent_Map.Contains(Info_Hash) then
            declare
               Peer_Map : Torrent_Maps.Reference_Type := Torrent_Map.Reference(Info_Hash);
            begin
               Peer_Map.Exclude(To_String(Peer_Id));

               Put_Line("Peer """ & To_String(Peer_Id) & """ LEFT """ & Info_Hash & """");
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

      function Encode_Hash_Peers_Response(Info_Hash : String; From_Id : String;
                                          Options : Response_Options) return Bencode_Value_Holders.Holder is
         Result_Map : Bencode_Maps.Map;
      begin
         if Torrent_Map.Contains(Info_Hash) then
            if Options.Compact then
               -- Compact format
               declare
                  Compact_String : Unbounded_String;
                  Peer_Counter : Natural := 0;
               begin
                  for The_Peer of Torrent_Map.Constant_Reference(Info_Hash) loop
                     -- No need to send the peer to itself
                     if To_String(The_Peer.Peer_Id) /= From_Id then
                        Append(Compact_String, Ip_Port_Bytes(The_Peer));

                        Peer_Counter := Peer_Counter + 1;
                        exit when Peer_Counter >= Options.Num_Want;
                     end if;
                  end loop;

                  Result_Map.Include(To_Unbounded_String("peers"), Encode(To_String(Compact_String)));
               end;
            else
               -- Full format
               declare
                  Peer_Bencodes : Bencode_Vectors.Vector;
               begin
                  for The_Peer of Torrent_Map.Constant_Reference(Info_Hash) loop
                     -- No need to send the peer to itself
                     if To_String(The_Peer.Peer_Id) /= From_Id then
                        declare
                           Peer_Bencode : Bencode_Maps.Map;
                           Test : String := Ip_Port_Bytes(The_Peer);
                        begin
                           Peer_Bencode.Include(To_Unbounded_String("peer id"), Encode(To_String(The_Peer.Peer_id)));
                           Peer_Bencode.Include(To_Unbounded_String("port"), Encode(The_Peer.Port));
                           Peer_Bencode.Include(To_Unbounded_String("ip"), Encode(To_String(The_Peer.Ip)));

                           Peer_Bencodes.Append(Encode(Peer_Bencode));
                           exit when Natural(Peer_Bencodes.Length) > Options.Num_Want;
                        end;
                     end if;
                  end loop;

                  Result_Map.Include(To_Unbounded_String("peers"), Encode(Peer_Bencodes));
               end;
            end if;

            declare
               Stats : Scrape_Stat_Data := Scrape_Stats(Info_Hash);
            begin
               Result_Map.Include(To_Unbounded_String("complete"), Encode(Stats.Complete));
               Result_Map.Include(To_Unbounded_String("incomplete"), Encode(Stats.Incomplete));
               Result_Map.Include(To_Unbounded_String("downloaded"),
                                  Encode((if Saved_Stats_Map.Contains(Info_Hash) then Saved_Stats_Map(Info_Hash).Downloaded else 0)));
            end;
         end if;

         Result_Map.Include(To_Unbounded_String("interval"), Encode(60 * 5));

         return Encode(Result_Map);
      end Encode_Hash_Peers_Response;

      function Scrape_Stats(Info_Hash : String) return Scrape_Stat_Data is
         Result : Scrape_Stat_Data;
      begin
         if Torrent_Map.Contains(Info_Hash) then
            for Peer of Torrent_Map(Info_Hash) loop
               if Peer.Left = 0 then
                  Result.Complete := Result.Complete + 1;
               else
                  Result.Incomplete := Result.Incomplete + 1;
               end if;
            end loop;
         end if;
         if Saved_Stats_Map.Contains(Info_Hash) then
            Result.Downloaded := Saved_Stats_Map(Info_Hash).Downloaded;
         end if;

         return Result;
      end Scrape_Stats;

      function Ip_Port_Bytes(From_Peer : Peer) return String is
         Ip_Numbers : array (1..4) of Natural;
         Last : Positive;
         Ip : constant String := To_String(From_Peer.Ip);

         Port_First : Unsigned_16 := Shift_Right(Unsigned_16(From_Peer.Port), 8 * 0) and 16#FF#;
         Port_Second : Unsigned_16 := Shift_Right(Unsigned_16(From_Peer.Port), 8 * 1) and 16#FF#;

         Result : String (1..6);
      begin
         Get(Ip, Ip_Numbers(1), Last);
         Get(Ip(Last + 2 .. Ip'Length), Ip_Numbers(2), Last);
         Get(Ip(Last + 2 .. Ip'Length), Ip_Numbers(3), Last);
         Get(Ip(Last + 2 .. Ip'Length), Ip_Numbers(4), Last);

         for I in 1..4 loop
            Result(I) := Character'Val(Ip_Numbers(I));
         end loop;

         if System.Default_Bit_Order = System.Low_Order_First then
            -- This is how it is on x86
            Result(5) := Character'Val(Port_Second);
            Result(6) := Character'Val(Port_First);
         else
            Result(5) := Character'Val(Port_First);
            Result(6) := Character'Val(Port_Second);
         end if;

         return Result;
      end Ip_Port_Bytes;

      procedure Downloaded(Info_Hash : String) is
      begin
         if Saved_Stats_Map.Contains(Info_Hash) then
            Saved_Stats_Map(Info_Hash).Downloaded := Saved_Stats_Map(Info_Hash).Downloaded + 1;
         else
            Saved_Stats_Map.Include(Info_Hash, (Downloaded => 1));
         end if;
      end;

      function Total_Stat_Data return Total_Stats is
         Stats : Total_Stats;
      begin
         Stats.Known := Natural(Saved_Stats_Map.Length);
         for Stat of Saved_Stats_Map loop
            Stats.Downloaded := Stats.Downloaded + Stat.Downloaded;
         end loop;
         for Torrent in Torrent_Map.Iterate loop
            declare
               Torrent_Stats : Scrape_Stat_Data := Scrape_Stats(Key(Torrent));
            begin
               Stats.Seeders := Stats.Seeders + Torrent_Stats.Complete;
               Stats.Leechers := Stats.Leechers + Torrent_Stats.Incomplete;
            end;
         end loop;

         return Stats;
      end Total_Stat_Data;
   end Protected_Map;
end Hellish_Web.Peers;
