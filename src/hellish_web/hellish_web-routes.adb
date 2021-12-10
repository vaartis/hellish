with Ada.Text_Io; use Ada.Text_Io;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;
with Ada.Integer_Text_Io; use Ada.Integer_Text_Io;

with Aws.Server.Log;
with Aws.Config;
with Aws.Services.Dispatchers.URI;
with AWS.Mime;
with Aws.Parameters;

with Hellish_Web.Bencoder;
with Hellish_Web.Peers;

package body Hellish_Web.Routes is
   function To_Hex_string(Input : String) return String is
      Result : Unbounded_String;
   begin
      for Char of Input loop
         declare
            Hex_Prefix_Length : constant := 3;
            Hexa : String (1 .. Hex_Prefix_Length + 3 + 1);
            Temp : String (1 .. 2);
            Start : Natural;
         begin
            -- A ridiculously difficult way of translating a decimal into hex without 16# and #
            Put(Hexa, Character'Pos(Char), 16);
            Start := Ada.Strings.Fixed.Index(Source => Hexa, Pattern => "#");
            Ada.Strings.Fixed.Move(Source  => Hexa (Start + 1 .. Hexa'Last - 1),
                                   Target  => Temp,
                                   Justify => Ada.Strings.Right,
                                   Pad => '0');

            Append(Result, Trim(Temp, Ada.Strings.Both));
         end;
      end loop;
      -- Translate to lowercase to match what Transmission shows
      Translate(Result, Lower_Case_Map);

      return To_String(Result);
   end;

   function Dispatch
     (Handler : in Announce_Handler;
      Request : in Status.Data) return Response.Data is
      Params : constant Parameters.List := AWS.Status.Parameters(Request);

      Result : Bencoder.Bencode_Value_Holders.Holder;
   begin
      declare
         Required_Params : array (Natural range <>) of Unbounded_String :=
           (To_Unbounded_String("info_hash"), To_Unbounded_String("peer_id"),
            To_Unbounded_String("port"), To_Unbounded_String("uploaded"),
            To_Unbounded_String("downloaded"), To_Unbounded_String("left"));
         -- ip and event are optional
      begin
         for Name of Required_Params loop
            if not Params.Exist(To_String(Name)) then
               Result := Bencoder.With_Failure_Reason(To_String(Name & " is missing"));
               goto Finish;
            end if;
         end loop;
      end;

      declare
         Info_Hash : String := Params.Get("info_hash");
         Info_Hash_Hex : String := To_Hex_String(info_hash);
         Ip : Unbounded_String := To_Unbounded_String(if Params.Exist("ip")
                                                      then Params.Get("ip")
                                                      else Aws.Status.Peername(Request));
      begin
         if Params.Get("event") = "stopped" then
               Peers.Protected_Map.Remove(To_Hex_String(info_hash), To_Unbounded_String(Params.Get("peer_id")));
            else
               Peers.Protected_Map.Add(Info_Hash_Hex,
                      (Peer_Id => To_Unbounded_String(Params.Get("peer_id")),
                       Ip => Ip,
                       Port => Positive'Value(Params.Get("port")),
                       Uploaded => Natural'Value(Params.Get("uploaded")),
                       Downloaded => Natural'Value(Params.Get("downloaded")),
                       Left => Natural'Value(Params.Get("left"))));
         end if;

         declare
            Compact : Boolean := not Params.Exist("compact") or Params.Get("compact") = "1";
         begin
            Result :=
              Peers.Protected_Map.Encode_Hash_Peers_Response(Info_Hash_Hex, Params.Get("peer_id"),
                                                             Compact => Compact);
         end;
      end;

      -- Put_Line(Status.Parameters(Request).URI_Format);

      <<Finish>>
      return Response.Build(Mime.Text_Plain, Result.Element.Encoded);
   end Dispatch;

   function Dispatch
     (Handler : in Scrape_Handler;
      Request : in Status.Data) return Response.Data is
      Params : constant Parameters.List := AWS.Status.Parameters(Request);

      Info_Hashes : Parameters.VString_Array := Params.Get_Values("info_hash");

      Files : Bencoder.Bencode_Vectors.Vector;
      Result_Map : Bencoder.Bencode_Maps.Map;
   begin
      for Hash of Info_Hashes loop
         declare
            Info_Hash_Hex : String := To_Hex_String(To_String(Hash));
            Stats : Peers.Scrape_Stat_Data := Peers.Protected_Map.Scrape_Stats(Info_Hash_Hex);

            File_Stats : Bencoder.Bencode_Maps.Map;
         begin
            File_Stats.Include("complete", Bencoder.Encode(Stats.Complete));
            File_Stats.Include("incomlete", Bencoder.Encode(Stats.Incomplete));
            File_Stats.Include("downloaded", Bencoder.Encode(Stats.Downloaded));

            Files.Append(Bencoder.Encode(File_Stats));
         end;
      end loop;
      Result_Map.Include("files", Bencoder.Encode(Files));

      return Response.Build(Mime.Text_Plain, Bencoder.Encode(Result_Map).Element.Encoded);
   end Dispatch;

   procedure Run_Server is
   begin
      Services.Dispatchers.Uri.Register(Root, "/announce", Announce);
      Services.Dispatchers.Uri.Register(Root, "/scrape", Scrape);

      Server.Start(Hellish_Web.Routes.Http, Root, Conf);
      Server.Log.Start(Http, Put_Line'Access, "hellish");

      Put_Line("Started on http://" & Config.Server_Host(Conf)
                 -- Trim the number string on the left because it has a space for some reason
                 & ":" & Trim(Config.Server_Port(Conf)'Image, Ada.Strings.Left));
      Server.Wait(Server.Q_Key_Pressed);

      Server.Shutdown(Http);
   end Run_Server;
end Hellish_Web.Routes;
