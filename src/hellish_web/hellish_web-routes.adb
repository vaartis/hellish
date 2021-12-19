with Ada.Text_Io; use Ada.Text_Io;
with Ada.Integer_Text_Io; use Ada.Integer_Text_Io;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;
with Ada.Directories;
with Ada.Containers.Indefinite_Holders;
with Ada.Calendar;

with GNAT.Regpat; use GNAT.Regpat;
with Gnat.SHA1;

with
  Aws.Cookie,
  Aws.Session,
  Aws.Server.Log,
  Aws.Config,
  Aws.Services.Dispatchers.Uri,
  AWS.Mime,
  Aws.Parameters,
  Aws.Messages;

with Templates_Parser; use Templates_Parser;

with Gnatcoll.Json;

with Hellish_Web.Bencoder;
with Hellish_Web.Peers;
with Hellish_Web.Database;

with Orm; use Orm;

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

   function Request_Session(Request : Status.Data) return Session.Id is
   begin
      if Cookie.Exists(Request, Server.Session_Name) then
         return Session.Value(Cookie.Get(Request, Server.Session_Name));
      else
         return Status.Session(Request);
      end if;
   end Request_Session;

   Announce_Passkey_Matcher : constant Pattern_Matcher := Compile("/(\w+)/announce");

   function Dispatch
     (Handler : in Announce_Handler;
      Request : in Status.Data) return Response.Data is
      Params : constant Parameters.List := AWS.Status.Parameters(Request);

      Result : Bencoder.Bencode_Value_Holders.Holder;

      Matches : Match_Array (0..1);
      Uri : String := Status.Uri(Request);

      User : Detached_User;
   begin
      Match(Announce_Passkey_Matcher, Uri, Matches);
      declare
         Match : Match_Location := Matches(1);
         Passkey : String := Uri(Matches(1).First..Matches(1).Last);
      begin
         User := Detached_User(Database.Get_User_By_Passkey(Passkey));

         if User = No_Detached_User then
            Result := Bencoder.With_Failure_Reason("Invalid passkey");
            goto Finish;
         end if;
      end;

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
         if Detached_Torrent(Database.Get_Torrent_By_Hash(Info_Hash_Hex)) = No_Detached_Torrent then
            Result := Bencoder.With_Failure_Reason("Unregistered torrent");
            goto Finish;
         end if;

         if Params.Get("event") = "stopped" then
               Peers.Protected_Map.Remove(To_Hex_String(info_hash), To_Unbounded_String(Params.Get("peer_id")));
         else
            if Params.Get("event") = "completed" then
               -- Increment the downloaded count
               Peers.Protected_Map.Downloaded(Info_Hash_Hex);
            end if;
            Peers.Protected_Map.Add(Info_Hash_Hex,
                                    (Peer_Id => To_Unbounded_String(Params.Get("peer_id")),
                                     Ip => Ip,
                                     Port => Positive'Value(Params.Get("port")),
                                     Uploaded => Natural'Value(Params.Get("uploaded")),
                                     Downloaded => Natural'Value(Params.Get("downloaded")),
                                     Left => Natural'Value(Params.Get("left"))),
                                    User);
         end if;

         declare
            Compact : Boolean := not Params.Exist("compact") or Params.Get("compact") = "1";
            Num_Want : Natural := 50;

            package Indefinite_String_Holders is new Ada.Containers.Indefinite_Holders(String);
            use Indefinite_String_Holders;

            Warning : Indefinite_String_Holders.Holder;
            procedure Include_Warning(Dict : in out Bencoder.Bencode_Value'Class) is
            begin
               -- Add the warning to the held result
               Bencoder.Bencode_Dict(Dict).Include("warning message", Bencoder.Encode(Warning.Element));
            end Include_Warning;
         begin
            if Params.Exist("numwant") then
               begin
                  Num_Want := Natural'Value(Params.Get("numwant"));
               exception
                  when Constraint_Error =>
                     -- Don't fail just because the number wasn't properly provided, just use the default
                     Warning := To_Holder("numwant was expected to be a positive number, but was " & Params.Get("numwant"));
               end;
            end if;

            Result :=
              Peers.Protected_Map.Encode_Hash_Peers_Response(Info_Hash_Hex, Params.Get("peer_id"),
                                                             (Compact => Compact, Num_Want => Num_Want));
            if not Warning.Is_Empty then
               Result.Update_Element(Include_Warning'Access);
            end if;
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
            File_Stats.Include(To_Unbounded_String("complete"), Bencoder.Encode(Stats.Complete));
            File_Stats.Include(To_Unbounded_String("incomplete"), Bencoder.Encode(Stats.Incomplete));
            File_Stats.Include(To_Unbounded_String("downloaded"), Bencoder.Encode(Stats.Downloaded));

            Files.Append(Bencoder.Encode(File_Stats));
         end;
      end loop;
      Result_Map.Include(To_Unbounded_String("files"), Bencoder.Encode(Files));

      return Response.Build(Mime.Text_Plain, Bencoder.Encode(Result_Map).Element.Encoded);
   end Dispatch;

   function Dispatch
     (Handler : in Index_Handler;
      Request : in Status.Data) return Response.Data is
      Total_Stats : Peers.Total_Stats := Peers.Protected_Map.Total_Stat_Data;
      Translations : Translate_Set;

      Session_Id : Session.Id := Request_Session(Request);
      Username : String := Session.Get(Session_Id, "username");
   begin
      Insert(Translations, Assoc("total_known", Total_Stats.Known));
      Insert(Translations, Assoc("total_downloaded", Total_Stats.Downloaded));
      Insert(Translations, Assoc("current_seeders", Total_Stats.Seeders));
      Insert(Translations, Assoc("current_leechers", Total_Stats.Leechers));

      if Username'Length > 0 then
         declare
            The_User : Detached_User'Class := Database.Get_User(Username);
         begin
            Insert(Translations,
                   Assoc("host", Aws.Config.Server_Host(Conf) & ":" & Trim(Aws.Config.Server_Port(Conf)'Image, Ada.Strings.Left)));
            Insert(Translations, Assoc("username", The_User.Username));
            Insert(Translations, Assoc("passkey", The_User.Passkey));
         end;
      end if;

      return Response.Build(Mime.Text_Html,
                            String'(Templates_Parser.Parse("assets/index.html", Translations)));
   end Dispatch;

   -- API

   function Dispatch
     (Handler : in Api_Upload_Handler;
      Request : in Status.Data) return Response.Data is
      Params : constant Parameters.List := Status.Parameters(Request);
      File_Path : String := Params.Get("file");

      use Ada.Directories;
      Uploads_Path : constant String := "uploads/torrents/";

      use Gnatcoll.Json;
      Result : Json_Value := Create_Object;

      use Orm;
      Session_Id : Session.Id := Request_Session(Request);
      Username : String := Session.Get(Session_Id, "username");
   begin
      if Username'Length = 0 then
         return Response.Acknowledge(Messages.S403, "Forbidden");
      end if;

      declare
         use Bencoder;
         File : File_Type;

         Decoded : Bencode_Value_Holders.Holder;
      begin
         Open(File, Mode => In_File, Name => File_Path);
         Decoded := Decode(File);
         Close(File);

         declare
            Bencoded_Info : Unbounded_String :=
              Bencode_Dict(Decoded.Element).Value(To_Unbounded_String("info")).Element.Element.Encoded;
            Sha1_Hash : String := Gnat.Sha1.Digest(To_String(Bencoded_Info));
            New_Name : String := Compose(Name => Sha1_Hash, Extension => "torrent");
            Uploaded_Path : String := Compose(Containing_Directory => Uploads_Path,Name => New_Name);
         begin
            Create_Path(Uploads_Path);
            Copy_File(File_Path, Uploaded_Path);

            -- Only really need to save the hash, since it's the filename. Things like actual file name and such
            -- are encoded in the torrent file itself
            Database.Create_Torrent(Username => Username, Info_Hash => Sha1_Hash);
         end;
      end;

      Result.Set_Field("ok", True);
      return Response.Build(Mime.Application_Json, String'(Result.write));
   end Dispatch;

   function Dispatch(Handler : in Api_User_Register_Handler;
                     Request : in Status.Data) return Response.Data is
      Params : constant Parameters.List := Status.Parameters(Request);
      Username : String := Params.Get("username");
      Password : String := Params.Get("password");

      Min_Name_Length : constant Positive := 1;
      Min_Pwd_Length : constant Positive := 8;

      use Gnatcoll.Json;
      Result : Json_Value := Create_Object;
   begin
      if Username'Length < Min_Name_Length then
         Result.Set_Field("ok", False);
         Result.Set_Field("error", "Username must be at least" & Min_Name_Length'Image & " characters long");

         goto Finish;
      end if;
      if Password'Length < Min_Pwd_Length then
         Result.Set_Field("ok", False);
         Result.Set_Field("error", "Password must be at least" & Min_Pwd_Length'Image & " characters long");

         goto Finish;
      end if;
      declare
         Created : Boolean := Database.Create_User(Username, Password);
      begin
         if not Created then
            Result.Set_Field("ok", False);
            Result.Set_Field("error", "Username already taken");

            goto Finish;
         end if;
      end;

      Result.Set_Field("ok", True);

      <<Finish>>
      return Response.Build(Mime.Application_Json, String'(Result.Write));
   end Dispatch;

   overriding function Dispatch(Handler : in Api_User_Login_Handler;
                                Request : in Status.Data) return Response.Data is
      Params : constant Parameters.List := Status.Parameters(Request);
      Username : String := Params.Get("username");
      Password : String := Params.Get("password");

      Success : Boolean := Database.Verify_User_Credentials(Username, Password);

      use Gnatcoll.Json;
      Result : Json_Value := Create_Object;

      -- Always associate a new session with the request on login,
      -- doesn't seem to work well otherwise
      Session_Id : Session.ID := Status.Session(Request);
   begin
      Result.Set_Field("ok", Success);
      if Success then
         declare
            User : Detached_User'Class := Database.Get_User(Username);
         begin
            Session.Set(Session_Id, "username", User.Username);
            Session.Save(Session_File_Name);
         end;
      end if;

      return Response.Build(Mime.Application_Json, String'(Result.Write));
   end Dispatch;

   -- Entrypoint

   procedure Run_Server is
   begin
      Database.Init;

      if Ada.Directories.Exists(Session_File_Name) then
         Session.Load(Session_File_Name);
      end if;

      Services.Dispatchers.Uri.Register(Root, "/", Index);
      Services.Dispatchers.Uri.Register_Regexp(Root, "/(\w+)/announce", Announce);
      Services.Dispatchers.Uri.Register(Root, "/(\w+)/scrape", Scrape);

      --Services.Dispatchers.Uri.Register(Root, "/register", Register);

      Services.Dispatchers.Uri.Register(Root, "/api/user/register", Api_User_Register);
      Services.Dispatchers.Uri.Register(Root, "/api/user/login", Api_User_Login);
      Services.Dispatchers.Uri.Register(Root, "/api/upload", Api_Upload);

      Server.Start(Hellish_Web.Routes.Http, Root, Conf);
      Server.Log.Start(Http, Put_Line'Access, "hellish");

      Put_Line("Started on http://" & Aws.Config.Server_Host(Conf)
                 -- Trim the number string on the left because it has a space for some reason
                 & ":" & Trim(Aws.Config.Server_Port(Conf)'Image, Ada.Strings.Left));
      Server.Wait(Server.Q_Key_Pressed);

      Server.Shutdown(Http);
   end Run_Server;
end Hellish_Web.Routes;
