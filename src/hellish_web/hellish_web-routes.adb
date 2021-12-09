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
   function Clone(Element : in Announce_Handler) return Announce_Handler is
   begin
      return Element;
   end Clone;

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
      begin
         if Params.Get("event") = "started" then
            Peers.Protected_Map.Add(Info_Hash_Hex,
                      (Peer_Id => To_Unbounded_String(Params.Get("peer_id")),
                       Ip => To_Unbounded_String(Params.Get("ip")),
                       Port => Positive'Value(Params.Get("port")),
                       Uploaded => Natural'Value(Params.Get("uploaded")),
                       Downloaded => Natural'Value(Params.Get("downloaded")),
                       Left => Natural'Value(Params.Get("left"))));
         elsif Params.Get("event") = "stopped" then
            Peers.Protected_Map.Remove(To_Hex_String(info_hash), To_Unbounded_String(Params.Get("peer_id")));
         end if;

         Result := Peers.Encode_Hash_Peers_Response(Info_Hash_Hex);
      end;

      Put_Line(Status.Parameters(Request).URI_Format);

      <<Finish>>
      return Response.Build(Mime.Text_Html, Result.Element.Encoded);
   end Dispatch;

   procedure Run_Server is
   begin
      Services.Dispatchers.Uri.Register(Root, "/announce", Announce);
      Server.Start(Hellish_Web.Routes.Http, Root, Conf);
      Server.Log.Start(Http, Put_Line'Access, "hellish");

      Put_Line("Started on http://" & Config.Server_Host(Conf)
                 -- Trim the number string on the left because it has a space for some reason
                 & ":" & Trim(Config.Server_Port(Conf)'Image, Ada.Strings.Left));
      Server.Wait(Server.Q_Key_Pressed);

      Server.Shutdown(Http);
   end Run_Server;
end Hellish_Web.Routes;
