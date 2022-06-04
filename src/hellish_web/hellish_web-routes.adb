with Ada.Text_Io; use Ada.Text_Io;
with Ada.Text_IO.Text_Streams;  use Ada.Text_IO.Text_Streams;
with Ada.Integer_Text_Io; use Ada.Integer_Text_Io;
with Ada.Float_Text_Io; use Ada.Float_Text_Io;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;
with Ada.Strings.Equal_Case_Insensitive; use Ada.Strings;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Containers.Indefinite_Holders;
with Ada.Directories;
with Ada.Containers.Indefinite_Ordered_Maps;
with
  Ada.Calendar,
  Ada.Calendar.Formatting,
  Ada.Calendar.Time_Zones;

with
  Gnat.Regpat,
  Gnat.Sha1,
  Gnat.Command_Line,
  Gnat.Traceback.Symbolic,
  Gnat.Calendar.Time_Io;
use Gnat.Regpat;

with Markdown; use Markdown;

with
  Aws.Cookie,
  Aws.Session,
  Aws.Server.Log,
  AWS.Mime,
  Aws.Parameters,
  Aws.Messages,
  Aws.Headers,
  Aws.Resources,
  Aws.Resources.Streams.Memory,
  Aws.Translator,
  Aws.Response.Set,
  Aws.Log,
  Aws.Exceptions,
  Aws.Url,
  Aws.Utils.Streams,
  Aws.Smtp;

with
  Templates_Parser,
  Templates_Parser.Utils;
use Templates_Parser;

with Gnatcoll.Json, Gnatcoll.Sql.Exec, Gnatcoll.Config;

with Sodium.Functions;

with
  Dom.Core,
  Dom.Core.Documents,
  Dom.Core.Elements,
  Dom.Core.Nodes,
  Dom.Core.Attrs;

with
  Hellish_Web,
  Hellish_Web.Bencoder,
  Hellish_Web.Peers,
  Hellish_Web.Database;
with Hellish_Irc;
with Hellish_Mail;

with Orm; use Orm;

package body Hellish_Web.Routes is
   Default_Md_Flags : Markdown.Parser_Flag := Md_Flag_No_Html_Blocks
     or Md_Flag_No_Html_Spans
     or Md_Flag_Permissive_Url_Autolinks
     or Md_Flag_Strikethrough;

   package Post_Subscriptions is new Database.Subscriptions
     (T => Detached_Post, Meta => Meta, Set_Meta => Set_Meta);
   package Torrent_Subscriptions is new Database.Subscriptions
     (T => Detached_Torrent, Meta => Meta, Set_Meta => Set_Meta);

   package String_Holders is new Ada.Containers.Indefinite_Holders(String);
   use String_Holders;

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

   function Decoded_Torrent(The_Torrent : Detached_Torrent'Class) return Bencoder.Bencode_Value_Holders.Holder is
      use Ada.Directories;
      use Bencoder;

      File_Path : String := Compose(Containing_Directory => Uploads_Path,
                                    Name => The_Torrent.Info_Hash,
                                    Extension => "torrent");
      File : File_Type;
      Decoded : Bencode_Value_Holders.Holder;
      Bencoded_Info : Bencode_Value_Holders.Holder;
   begin
      Open(File, Mode => In_File, Name => File_Path);
      Decoded := Decode(File);
      Close(File);

      return Decoded;
   end Decoded_Torrent;

   function Bytes_To_Printable(Bytes : Long_Long_Integer) return String is
      Float_Repr : Float;
      Unit : String (1..3);
      Formatted_Str : String (1 .. 16);
   begin
      if Bytes >= (1024 * 1024 * 1024) then
         Float_Repr := Float(Bytes) / 1024.0 / 1024.0 / 1024.0;
         Unit := "GiB";
      elsif Bytes >= (1024 * 1024) then
         Float_Repr := Float(Bytes) / 1024.0 / 1024.0;
         Unit := "MiB";
      elsif Bytes >= 1024 then
         Float_Repr := Float(Bytes) / 1024.0;
         Unit := "KiB";
      else
         Float_Repr := Float(Bytes);
         Unit := "B  ";
      end if;
      Put(Formatted_Str, Float_Repr, Aft => 2, Exp => 0);

      return Trim(Formatted_Str, Ada.Strings.Both) & Trim(Unit, Ada.Strings.Right);
   end Bytes_To_Printable;

   function User_Announce_Url(The_User : Detached_User'Class) return String is
     ((if Https then "https://" else "http://") &
        (if Server_Host /= "" then To_String(Server_Host) else Host) & "/" & The_User.Passkey & "/announce");


   package Int_String_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (Key_Type => Integer,
      Element_Type => String);
   use Int_String_Maps;
   Torrent_Categories : constant Int_String_Maps.Map :=
     (0 => "Other", 1 => "Music", 2 => "Applications",
      3 => "Anime", 4 => "Movies", 5 => "TV",
      6 => "Games - PC", 7 => "Games - Other",
      8 => "Books", 9 => "Comics");
   Post_Flags : constant Int_String_Maps.Map :=
     (1 => "News", 2 => "Request / Offer");

   type Meta_Link_Type is (Prefix, Contains);
   type Meta_Link is record
      Name : String_Holders.Holder;
      Json_Name : String_Holders.Holder;
      Link : String_Holders.Holder;
      Link_Type : Meta_Link_Type := Prefix;
      Category : Integer;
      Autofind_Image : Boolean := False;
   end record;
   Possible_Meta_Links : array (Natural range <>) of Meta_Link :=
     (1 => (Name => To_Holder("Wikipedia"), Json_Name => To_Holder("wikipedia"),
            Link => To_Holder("https://en.wikipedia.org/wiki/"), Category => -1, others => <>),
      2 => (Name => To_Holder("MAL"), Json_Name => To_Holder("mal"),
            Link => To_Holder("https://myanimelist.net/anime/"), Category => 3, others => <>),
      3 => (Name => To_Holder("MAL"), Json_Name => To_Holder("mal"),
            Link => To_Holder("https://myanimelist.net/manga/"), Category => 9, others => <>),
      4 => (Name => To_Holder("MusicBrainz"), Json_Name => To_Holder("musicbrainz"),
            Link => To_Holder("https://musicbrainz.org/release/"), Category => 1, others => <>),
      5 => (Name => To_Holder("Discogs"), Json_Name => To_Holder("discogs"),
            Link => To_Holder("https://www.discogs.com/release/"), Category => 1,
            Autofind_Image => True, others => <>),
      6 => (Name => To_Holder("Bandcamp"), Json_Name => To_Holder("bandcamp"),
            Link_Type => Contains, Link => To_Holder(".bandcamp.com"), Category => 1,
            Autofind_Image => True));

   function Page_Parameters(Params : Parameters.List; Result_Page_Size, Page_Offset : out Natural) return Natural is
      Page_Size : constant Natural := 25;
      Page : Natural := (if Params.Exist("page") then Integer'Value(Params.Get("page")) else 1);
   begin
      Result_Page_Size := Page_Size;
      Page_Offset := (Page - 1) * Page_Size;

      return Page;
   end;

   User_Mention_Matcher : constant Pattern_Matcher := Compile("@(?:\w|\d)+");
   function Process_Content(Content : String) return String is
      use Gnatcoll.Json;

      Processed_Content : Unbounded_String := To_Unbounded_String(Content);

      Mentioned_Match : Match_Array(0..0);
      Current : Natural := Content'First;
   begin
      -- Process user mentions
      loop
         Match(User_Mention_Matcher, To_String(Processed_Content), Mentioned_Match, Current);
         exit when Mentioned_Match(0) = No_Match;

         declare
            Match_Name : String := To_String(Processed_Content)(Mentioned_Match(0).First + 1..Mentioned_Match(0).Last);
            Maybe_User : Detached_User'Class := Database.Get_User(Match_Name);
         begin
            if Maybe_User /= Detached_User'Class(No_Detached_User) then
               Replace_Slice(Processed_Content, Mentioned_Match(0).First, Mentioned_Match(0).Last,
                             "[@" & Maybe_User.Username & "](/profile/" & Maybe_User.Username & ")");
            end if;
         end;

      <<Skip>>
         Current := Mentioned_Match(0).Last + 1;
      end loop;

      declare
         -- This both translates markdown to html and escapes whatever html the text might've had,
         -- so should be safe
         Post_Content : String := Markdown.To_Html(To_String(Processed_Content), Default_Md_Flags);
      begin
         return Post_Content;
      end;
   end Process_Content;

   procedure Page_Translations(Request : Status.Data;
                               Total_Count : Natural;
                               Translations : in out Translate_Set) is
      Params : Parameters.List := Status.Parameters(Request);

      Page_Size, Page_Offset : Natural;
      Page : Integer := Page_Parameters(Params, Page_Size, Page_Offset);

      -- Round up
      Page_Count : Natural := Natural(Float'Ceiling(Float(Total_Count) / Float(Page_Size)));

      Pages, Page_Addresses : Vector_Tag;
   begin
      if Page_Count > 1 then
         Pages := @ & 1;
         Params.Update(To_Unbounded_String("page"), To_Unbounded_String("1"), Decode => False);
         Page_Addresses := @ & String'(Status.Uri(Request) & Params.Uri_Format);

         if Page_Count > 3 and Page > 4 then
            Pages := @ & "...";
            Page_Addresses := @ & "";
         end if;

         for P in 2..Page_Count - 1 loop
            if P >= Page - 2 and P <= Page + 2 then
               Pages := @ & P;
               Params.Update(To_Unbounded_String("page"),
                             To_Unbounded_String(Trim(P'Image, Ada.Strings.Left)),
                             Decode => False);
               Page_Addresses := @ & String'(Status.Uri(Request) & Params.Uri_Format);
            end if;
         end loop;

         if Page_Count > 3 and Page < Page_Count - 4 then
            Pages := @ & "...";
            Page_Addresses := @ & "";
         end if;

         Pages := @ & Page_Count;
         Params.Update(To_Unbounded_String("page"), To_Unbounded_String(Trim(Page_Count'Image, Ada.Strings.Left)), Decode => False);
         Page_Addresses := @ & String'(Status.Uri(Request) & Params.Uri_Format);

         Insert(Translations, Assoc("page", Pages));
         Insert(Translations, Assoc("page_address", Page_Addresses));
      end if;

      Insert(Translations, Assoc("current_page", Page));
   end Page_Translations;

   procedure Replies_Translations(Parent : Integer;
                                  The_User : Detached_User'Class;
                                  Translations : in out Translate_Set;
                                  Fetch_Function : access function(Parent : Integer;
                                                                   Offset : Integer;
                                                                   Limit : Integer;
                                                                   Total_Count : out Integer) return Post_List;
                                  Request : Status.Data
                                 ) is
      Page_Size, Page_Offset : Natural;
      Page : Integer := Page_Parameters(Status.Parameters(Request), Page_Size, Page_Offset);

      Total_Count : Natural;
      Replies : Post_List := Fetch_Function(Parent, Page_Offset, Page_Size, Total_Count);

      Reply : Orm.Post;
      Reply_Author : Detached_User'Class := No_Detached_User;
      Reply_Ids, Replies_Authors, Replies_Content,
        Replies_Is_Author, Replies_Profile_Picture,
        Replies_Created_At : Vector_Tag;
   begin
      while Replies.Has_row loop
         Reply := Replies.Element;
         Reply_Author := Database.Get_User(Reply.By_User);

         Reply_Ids := @ & Reply.Id;
         Replies_Authors := @ & Reply_Author.Username;
         Replies_Content := @ & Process_Content(Reply.Content);
         Replies_Is_Author := @ & (Reply_Author.Id = The_User.Id or The_User.Role = 1);

         declare
            use Gnatcoll.Json;
            Profile_Json : Json_Value := Read(Reply_Author.Profile);

            Profile_Picture : String := (if Profile_Json.Has_Field("profile_picture")
                                         then Profile_Json.Get("profile_picture")
                                         else "");

            Post_Meta : Json_Value := Read(Reply.Meta);
            Created_At : String := (if Post_Meta.Has_Field("created_at")
                                   then Post_Meta.Get("created_at")
                                   else "");
         begin
            Replies_Profile_Picture := @ & Templates_Parser.Utils.Web_Escape(Profile_Picture);

            Replies_Created_At := @ & Created_At;
         end;

         Replies.Next;
      end loop;
      Insert(Translations, Assoc("replies_total", Total_Count));
      Insert(Translations, Assoc("reply_id", Reply_Ids));
      Insert(Translations, Assoc("reply_author", Replies_Authors));
      Insert(Translations, Assoc("reply_content", Replies_Content));
      Insert(Translations, Assoc("reply_is_author", Replies_Is_Author));
      Insert(Translations, Assoc("reply_profile_picture", Replies_Profile_Picture));
      Insert(Translations, Assoc("reply_created_at", Replies_Created_At));

      Page_Translations(Request, Total_Count, Translations);
   end Replies_Translations;

   procedure Userinfo_Translations(The_User : Detached_User'Class; Translations : in out Translate_Set) is
   begin
      Insert(Translations, Assoc("userinfo_uploaded", Bytes_To_Printable(The_User.Uploaded)));
      Insert(Translations, Assoc("userinfo_downloaded", Bytes_To_printable(The_User.Downloaded)));
      Insert(Translations, Assoc("userinfo_username", The_User.Username));

      declare
         use Gnatcoll.Json;
         The_Profile : Json_Value := Read(The_User.Profile);
         Notifications : Json_Array :=  (if Has_Field(The_Profile, "notifications")
                                         then Get(The_Profile, "notifications")
                                         else Empty_Array);
         Count : Natural := Length(Notifications);
      begin
         Insert(Translations, Assoc("userinfo_has_notifications", Count > 0));
         Insert(Translations, Assoc("userinfo_notifications", Count));
      end;
      Insert(Translations, Assoc("userinfo_is_admin", The_User.Role = 1));
   end Userinfo_Translations;

   procedure Torrent_Table_Translations(Found_Torrents : in out Direct_Torrent_List; Translations : in out Translate_Set;
                                        The_User : Detached_User'Class;
                                        Show_Ul_Dl : Boolean := False) is
      Torrent_Names, Torrent_Ids,
        Torrent_Uploaders, Torrent_Comments, The_Torrent_Categories,
        The_Torrent_Stats, Torrent_Uls, Torrent_Dls: Vector_Tag;
   begin
      while Found_Torrents.Has_Row loop
         Torrent_Ids := @ & Found_Torrents.Element.Id;
         Torrent_Names := @ & Found_Torrents.Element.Display_Name;
         Torrent_Uploaders := @ & Database.Get_User(Found_Torrents.Element.Created_By).Username;

         declare
            Total_Comments : Integer;
            Searched_Replies : Post_List := Database.Torrent_Comments(Found_Torrents.Element.Id, 0, 0, Total_Comments);
         begin
            Torrent_Comments := @ & Total_Comments;
         end;

         The_Torrent_Categories := @ & Torrent_Categories(Found_Torrents.Element.Category);

         declare
            Torrent_Stats : Peers.Scrape_Stat_Data := Peers.Protected_Map.Scrape_Stats(Found_Torrents.Element.Info_Hash);
         begin
            The_Torrent_Stats := @ & (Trim(Torrent_Stats.Complete'Image, Ada.Strings.Left) & " /" & Torrent_Stats.Incomplete'Image);
         end;

         if Show_Ul_Dl then
            declare
               Stats : Detached_User_Torrent_Stat'Class := Database.Get_User_Stats_For_Torrent(The_User, Found_Torrents.Element.Id);
            begin
               if Stats /= Detached_User_Torrent_Stat'Class(No_Detached_User_Torrent_Stat) then
                  Torrent_Uls := @ & Bytes_To_Printable(Stats.Uploaded);
                  Torrent_Dls := @ & Bytes_To_Printable(Stats.Downloaded);
               else
                  Torrent_Uls := @ & "";
                  Torrent_Dls := @ & "";
               end if;
            end;
         end if;

         Found_Torrents.Next;
      end loop;

      Insert(Translations, Assoc("torrent_id", Torrent_Ids));
      Insert(Translations, Assoc("torrent_name", Torrent_Names));
      Insert(Translations, Assoc("torrent_uploader", Torrent_Uploaders));
      Insert(Translations, Assoc("torrent_comments", Torrent_Comments));
      Insert(Translations, Assoc("torrent_category", The_Torrent_Categories));
      Insert(Translations, Assoc("torrent_stats", The_Torrent_Stats));
      if Show_Ul_Dl then
         Insert(Translations, Assoc("torrent_ul", Torrent_Uls));
         Insert(Translations, Assoc("torrent_dl", Torrent_Dls));
      end if;
   end Torrent_Table_Translations;

   function Uri_Group_Match(Request : in Status.Data;
                            Matcher : Pattern_Matcher;
                            Group : Natural) return String is
      Matches : Match_Array (0..Group);
      Uri : String := Status.Uri(Request);
   begin
      Match(Matcher, Uri, Matches);

      return (if Matches(Group) /= No_Match
              then Uri(Matches(Group).First..Matches(Group).Last)
              else "");
   end Uri_Group_Match;

   function Rss_Feed_Base(Title, Description, Page_Link : String; Channel : out Dom.Core.Element;
                          Correlating_Page : String := "") return Dom.Core.Document is
      use
        Dom.Core,
        Dom.Core.Documents,
        Dom.Core.Elements,
        Dom.Core.Nodes,
        Dom.Core.Attrs;
      package Dc renames Dom.Core;

      Impl : Dom_Implementation;
      Doc : Document := Create_Document(Impl);

      Rss : Dc.Element := Append_Child(Doc, Create_Element(Doc, "rss"));

      Channel_Elem : Dc.Element := Append_Child(Rss, Create_Element(Doc, "channel"));

      Title_Elem : Dc.Element := Append_Child(Channel_Elem, Create_Element(Doc, "title"));
      Title_Text : Text := Append_Child(Title_Elem, Create_Text_Node(Doc, Title));

      Desc : Dc.Element := Append_Child(Channel_Elem, Create_Element(Doc, "description"));
      Desc_Text : Text := Append_Child(Desc, Create_Text_Node(Doc, Description));

      Link : Dc.Element := Append_Child(Channel_Elem, Create_Element(Doc, "link"));
      Link_Text : Text := Append_Child(Link, Create_Text_Node(Doc, (if Https then "https://" else "http://") & Host_Name & Correlating_Page));

      W3_Atom : String := "http://www.w3.org/2005/Atom";
      Atom_Link : Dc.Element :=
        Append_Child(Channel_Elem, Create_Element_Ns(Doc, Namespace_URI => W3_Atom, Qualified_Name => "atom:link"));
   begin
      Set_Attribute(Rss, "version", "2.0");
      Set_Attribute_Ns(Rss, W3_Atom, "xmlns:atom", W3_Atom);

      Set_Attribute(Atom_Link, "href", Page_Link);
      Set_Attribute(Atom_Link, "rel", "self");
      Set_Attribute(Atom_Link, "type", "application/atom+xml");

      Channel := Channel_Elem;
      return Doc;
   end Rss_Feed_Base;

   procedure Rss_Feed_Torrent_List(Channel : in out Dom.Core.Element;
                                   Found_Torrents : in out Direct_Torrent_List;
                                   Passkey : String) is
      use
        Dom.Core,
        Dom.Core.Documents,
        Dom.Core.Elements,
        Dom.Core.Nodes,
        Dom.Core.Attrs;
      use Gnatcoll.Json;

      package Dc renames Dom.Core;

      Full_Host : String := (if Https then "https://" else "http://") & Host_Name_Website;
      Doc : Dc.Document := Owner_Document(Channel);
   begin
      while Found_Torrents.Has_Row loop
         declare
            Item : Dc.Element := Append_Child(Channel, Create_Element(Doc, "item"));

            Title : Dc.Element := Append_Child(Item, Create_Element(Doc, "title"));
            Title_Text : Dc.Element := Append_Child(Title, Create_Text_Node(Doc, Found_Torrents.Element.Display_Name));

            Link : Dc.Element := Append_Child(Item, Create_Element(Doc, "link"));
            Link_Text : Dc.Element :=
              Append_Child(Link,
                           Create_Text_Node(Doc, Full_Host & "/download/" &
                                               Trim(Found_Torrents.Element.Id'Image, Ada.Strings.Left) & "?passkey=" & Passkey));
            Guid : Dc.Element := Append_Child(Item, Create_Element(Doc, "guid"));
            Guid_Text : Dc.Element := Append_Child(Guid,
                                                   Create_Text_Node(Doc, Full_Host & "/view/" &
                                                                      Trim(Found_Torrents.Element.Id'Image, Ada.Strings.Left)));

            Desc : Dc.Element := Append_Child(Item, Create_Element(Doc, "description"));
            Desc_Text : Dc.Element :=
              Append_Child(Desc,
                           Create_Text_Node(Doc, Markdown.To_Html(Found_Torrents.Element.Description, Default_Md_Flags)));

            Torrent_Meta : Json_Value := Read(Found_Torrents.Element.Meta);
            Created_At : String := (if Torrent_Meta.Has_Field("created_at")
                                    then Torrent_Meta.Get("created_at")
                                    else "");
         begin
            Set_Attribute(Guid, "isPermaLink", "true");
            Set_Attribute(Desc, "type", "html");

            if Created_At /= "" then
               declare
                  use Ada.Calendar, Ada.Calendar.Formatting, Ada.Calendar.Time_Zones;

                  Created_Time : Time := Value(Created_At) - Duration(Utc_Time_Offset * 60);

                  Pub_Date : Dc.Element := Append_Child(Item, Create_Element(Doc, "pubDate"));
                  Pub_Date_Text : Text :=
                    Append_Child(Pub_Date,
                                 Create_Text_Node(Doc, Gnat.Calendar.Time_Io.Image(Created_Time, "%a, %d %b %Y %T") & " -0000"));
               begin
                  null;
               end;
            end if;
         end;

         Found_Torrents.Next;
      end loop;
   end Rss_Feed_Torrent_List;

   function Referer_With_Error(Request : Status.Data; Error : String) return Response.Data is
      Referer : Url.Object := Url.Parse(Status.Header(Request).Get_Values("Referer"));
      Referer_Params : Parameters.List := Url.Parameters(Referer);
   begin
      Referer_Params.Update(To_Unbounded_String("error"), To_Unbounded_String(Error));
      return Response.Url(Location => Url.Abs_Path(Referer) & Referer_Params.Uri_Format);
   end Referer_With_Error;

   function Validate_Password(Password, Confirm_Password : String) return String_Holders.Holder is
      Min_Pwd_Length : constant Positive := 8;
      Max_Pwd_Length : constant Positive := 128;
   begin
      if Password'Length < Min_Pwd_Length or Password'Length > Max_Pwd_Length then
         return To_Holder("Password must be at least" & Min_Pwd_Length'Image & " characters long and at most"
                            & Max_Pwd_Length'Image & " characters long");
      end if;
      if Password /= Confirm_Password then
         return To_Holder("Password and confirmation must match");
      end if;

      return Empty_Holder;
   end Validate_Password;

   package body Images is separate;
   package body Posts is separate;
   package body Search is separate;
   package body Groups is separate;

   Announce_Passkey_Matcher : constant Pattern_Matcher := Compile("/(\w+)/announce");
   function Dispatch
     (Handler : in Announce_Handler;
      Request : in Status.Data) return Response.Data is
      Params : constant Parameters.List := AWS.Status.Parameters(Request);

      Result : Bencoder.Bencode_Value_Holders.Holder;

      Passkey : String := Uri_Group_Match(Request, Announce_Passkey_Matcher, 1);
      User : Detached_User'Class := Detached_User(Database.Get_User_By_Passkey(Passkey));
   begin
      if User = Detached_User'Class(No_Detached_User) then
         Result := Bencoder.With_Failure_Reason("Invalid passkey");
         goto Finish;
      end if;

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
         use Ada.Calendar;

         Info_Hash : String := Params.Get("info_hash");
         Info_Hash_Hex : String := To_Hex_String(info_hash);
         Ip : Unbounded_String := To_Unbounded_String(if Params.Exist("ip")
                                                      then Params.Get("ip")
                                                      else Aws.Status.Peername(Request));
         Headers : Aws.Headers.List := Status.Header(Request);
      begin
         if Headers.Get_Values("X-Forwarded-For") /= "" then
            -- When behind a proxy, use X-Forwarded-For
            Ip := To_Unbounded_String(Headers.Get_Values("X-Forwarded-For"));
         end if;

         if Detached_Torrent(Database.Get_Torrent_By_Hash(Info_Hash_Hex)) = No_Detached_Torrent then
            Result := Bencoder.With_Failure_Reason("Unregistered torrent");
            goto Finish;
         end if;

         if Params.Get("event") = "completed" then
            -- Increment the downloaded count
            Database.Snatch_Torrent(Info_Hash_Hex, User);
         end if;

         Peers.Protected_Map.Add(Info_Hash_Hex,
                                 (Peer_Id => To_Unbounded_String(Params.Get("peer_id")),
                                  Ip => Ip,
                                  Port => Positive'Value(Params.Get("port")),
                                  Uploaded => Long_Long_Integer'Value(Params.Get("uploaded")),
                                  Downloaded => Long_Long_Integer'Value(Params.Get("downloaded")),
                                  Left => Long_Long_Integer'Value(Params.Get("left")),
                                  Last_Seen => Clock,
                                  Last_Event => To_Unbounded_String(Params.Get("event")),
                                  User => Detached_User(User)));
         declare
            Compact : Boolean := not Params.Exist("compact") or Params.Get("compact") = "1";
            Num_Want : Natural := 50;

            Warning : String_Holders.Holder;
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
      if not Database.User_Exists(Username) then
         return Response.Url(Location => "/login");
      end if;

      Insert(Translations, Assoc("current_seeders", Total_Stats.Seeders));
      Insert(Translations, Assoc("current_leechers", Total_Stats.Leechers));

      Userinfo_Translations(Database.Get_User(Username), Translations);

      declare
         Latest_News : Detached_Post'Class := Database.Get_Latest_News;
         News_Author : Detached_User'Class := No_Detached_User;
      begin
         if Latest_News /= Detached_Post'Class(No_Detached_Post) then
            News_Author := Database.Get_User(Latest_News.By_User);
            Insert(Translations, Assoc("news_id", Latest_News.Id));
            Insert(Translations, Assoc("news_title", Templates_Parser.Utils.Web_Escape(Latest_news.Title)));
            Insert(Translations, Assoc("news_content", Process_Content(Latest_News.Content)));
            Insert(Translations, Assoc("news_author", News_Author.Username));
         end if;
      end;

      return Response.Build(Mime.Text_Html,
                            String'(Templates_Parser.Parse("assets/index.html", Translations)));
   end Dispatch;

   function Dispatch
     (Handler : in Login_Handler;
      Request : in Status.Data) return Response.Data is
      Session_Id : Session.Id := Request_Session(Request);
      Username : String := Session.Get(Session_Id, "username");

      Params : constant Parameters.List := Status.Parameters(Request);
      Error_Param : String := Params.Get("error");
      Translations : Translate_Set;
   begin
      if Database.User_Exists(Username) then
         -- Redirect to the main page
         return Response.Url(Location => "/");
      end if;


      if Error_Param'Length > 0 then
         Insert(Translations, Assoc("error", Error_Param));
      end if;

      return Response.Build(Mime.Text_Html,
                            String'(Templates_Parser.Parse("assets/login.html", Translations)));
   end Dispatch;

   function Dispatch
     (Handler : in Register_Handler;
      Request : in Status.Data) return Response.Data is

      Session_Id : Session.Id := Request_Session(Request);
      Username : String := Session.Get(Session_Id, "username");

      Params : constant Parameters.List := Status.Parameters(Request);
      Error_Param : String := Params.Get("error");
      Translations : Translate_Set;
   begin
      if Database.User_Exists(Username) then
         -- Redirect to the main page
         return Response.Url(Location => "/");
      end if;
      if Error_Param'Length > 0 then
         Insert(Translations, Assoc("error", Error_Param));
      end if;

      return Response.Build(Mime.Text_Html,
                            String'(Templates_Parser.Parse("assets/register.html", Translations)));
   end Dispatch;

   Download_Id_Matcher : constant Pattern_Matcher := Compile("/download/(\d+)");
   function Dispatch
     (Handler : in Download_Handler;
      Request : in Status.Data) return Response.Data is

      Params : constant Parameters.List := Status.Parameters(Request);
      Passkey : String := Params.Get("passkey");

      User : Detached_User'class := Database.Get_User_By_Passkey(Passkey);
   begin
      If User = Detached_User'Class(No_Detached_User) then
         -- Redirect to the login page
         return Response.Url(Location => "/login");
      end if;

      declare
         use Ada.Directories;
         use Bencoder;

         Id : Natural := Natural'Value(Uri_Group_Match(Request, Download_Id_Matcher, 1));

         Torrent : Detached_Torrent'Class := Database.Get_Torrent(Id);
         File_Path : String := Compose(Containing_Directory => Uploads_Path,
                                       Name => Torrent.Info_Hash,
                                       Extension => "torrent");
         File : File_Type;
         Decoded : Bencode_Dict;
         Bencoded_Info : Bencode_Dict;

         File_Name : Unbounded_String;
      begin
         Open(File, Mode => In_File, Name => File_Path);
         Decoded := Bencode_Dict(Decode(File).Element);
         Close(File);

         Bencoded_Info := Bencode_Dict(Decoded.Value(To_Unbounded_String("info")).Element.Element);
         File_Name := Bencode_String(Bencoded_Info.Value(To_Unbounded_String("name")).Element.Element).Value;

         Decoded.Include("announce", Encode(User_Announce_Url(User)));

         declare
            use Aws.Resources.Streams.Memory;

            Data : not null access Resources.Streams.Memory.Stream_Type
              := new Resources.Streams.Memory.Stream_Type;

            Sent_Name : String := Compose(Name => Base_Name(To_String(File_Name)), Extension => "torrent");

            -- "Resources.Streams.Stream_Access" supposedly causes invalid access, but actually it works
            pragma Suppress(Accessibility_Check);
         begin
            Append(Data.all,
                   Translator.To_Stream_Element_Array(To_String(Decoded.Encoded)),
                   Trim => False);

            return Response.Stream("application/x-bittorrent",
                                   Resources.Streams.Stream_Access(Data),
                                   Disposition => Response.Attachment,
                                   User_Filename => Sent_Name);
         end;
      end;
   end Dispatch;

   function Dispatch
     (Handler : in Upload_Handler;
      Request : in Status.Data) return Response.Data is

      Session_Id : Session.Id := Request_Session(Request);
      Username : String := Session.Get(Session_Id, "username");

      Translations : Translate_Set;

      Params : constant Parameters.List := Status.Parameters(Request);
      Update : String := Params.Get("update");
      Error : String := Params.Get("error");

      Category_Names, Category_Values : Vector_Tag;
   begin
      if not Database.User_Exists(Username) then
         -- Redirect to the login page
         return Response.Url(Location => "/login");
      end if;
      declare
         User : Detached_User'Class := Database.Get_User(Username);
      begin
         Insert(Translations, Assoc("announce", User_Announce_Url(User)));
         Userinfo_Translations(User, Translations);
      end;
      if Update /= "" then
         declare
            The_Torrent : Detached_Torrent'Class := Database.Get_Torrent(Integer'Value(Update));
            Torrent_Group : Integer := The_Torrent.Group;
         begin
            Insert(Translations, Assoc("update", Update));
            Insert(Translations, Assoc("update_name", The_Torrent.Display_Name));
            Insert(Translations, Assoc("update_desc", The_Torrent.Description));
            Insert(Translations, Assoc("update_category", The_Torrent.Category));

            if Torrent_Group > 0 then
               Insert(Translations, Assoc("update_group", Database.Get_Group(The_Torrent.Group).Name));
            end if;

            declare
               use Gnatcoll.Json;

               Torrent_Meta : Json_Value := Read(The_Torrent.Meta);
               Torrent_Links : Json_Value := (if Torrent_Meta.Has_Field("links")
                                              then Torrent_Meta.Get("links")
                                              else Create_Object);

               Link_Names, Link_Json_Names, Link_Values, Link_Links, Link_Autofinds : Vector_Tag;
            begin
               for Possible_Link of Possible_Meta_Links loop
                  -- Check if the category is right
                  if The_Torrent.Category = Possible_Link.Category or Possible_Link.Category = -1 then
                     declare
                        Maybe_Existing_Value : String := (if Torrent_Links.Has_Field(Possible_Link.Json_Name.Element)
                                                          then Torrent_Links.Get(Possible_Link.Json_Name.Element)
                                                          else "");
                     begin
                        Link_Names := @ & Possible_Link.Name.Element;
                        Link_Json_Names := @ & Possible_Link.Json_Name.Element;
                        Link_Links := @ & Possible_Link.Link.Element;
                        Link_Values := @ & Maybe_Existing_Value;
                        Link_Autofinds := @ & Possible_Link.Autofind_Image;
                     end;
                  end if;
               end loop;

               Insert(Translations, Assoc("link_name", Link_Names));
               Insert(Translations, Assoc("link_json_name", Link_Json_Names));
               Insert(Translations, Assoc("link_link", Link_Links));
               Insert(Translations, Assoc("link_value", Link_Values));
               Insert(Translations, Assoc("link_autofind", Link_Autofinds));

               if Torrent_Meta.Has_Field("image") then
                  Insert(Translations, Assoc("image", String'(Torrent_Meta.Get("image"))));
               end if;
            end;
         end;
      else
         -- This needs to always be set, as textarea uses all whitespace literally
         -- and the template engine can't have anything else be on the same line as statements
         Insert(Translations, Assoc("update_desc", ""));
      end if;

      for Category_Cursor in Torrent_Categories.Iterate loop
         Category_Values := @ & Key(Category_Cursor);
         Category_Names := @ & Element(Category_Cursor);
      end loop;
      Insert(Translations, Assoc("category_name", Category_Names));
      Insert(Translations, Assoc("category_value", Category_Values));

      if Error /= "" then
         Insert(Translations, Assoc("error", error));
      end if;

      return Response.Build(Mime.Text_Html,
                            String'(Templates_Parser.Parse("assets/upload.html", Translations)));
   end Dispatch;

   View_Id_Matcher : constant Pattern_Matcher := Compile("/view/(\d+)");
   function Dispatch
     (Handler : in View_Handler;
      Request : in Status.Data) return Response.Data is

      Session_Id : Session.Id := Request_Session(Request);
      Username : String := Session.Get(Session_Id, "username");

      Params : constant Parameters.List := Status.Parameters(Request);
      Error : String := Params.Get("error");
   begin
      if not Database.User_Exists(Username) then
         -- Redirect to the login page
         return Response.Url(Location => "/login");
      end if;

      declare
         use Bencoder;

         Id : Natural := Natural'Value(Uri_Group_Match(Request, View_Id_Matcher, 1));

         The_Torrent : Detached_Torrent'Class := Database.Get_Torrent(Id);
         Decoded : Bencode_Dict := Bencode_Dict(Decoded_Torrent(The_Torrent).Element);
         Bencoded_Info : Bencode_Dict := Bencode_Dict(Decoded.Value(To_Unbounded_String("info")).Element.Element);
         Uploader : Detached_User'Class := Database.Get_User(The_Torrent.Created_By);

         Original_File_Name : Unbounded_String;

         File_Names : Vector_Tag;
         File_Sizes : Vector_Tag;
         Total_Size : Long_Long_Integer := 0;
         Translations : Translate_Set;

         -- Escape whatever the user inputs as the name
         Html_Name : String := Templates_Parser.Utils.Web_Escape(The_Torrent.Display_Name);
         -- This both translates markdown to html and escapes whatever html the text might've had,
         -- so should be safe
         Html_Desc : String := Process_Content(The_Torrent.Description);

         The_User : Detached_User'Class := Database.Get_User(Username);
      begin
         Original_File_Name :=
           Bencode_String(Bencoded_Info.Value(To_Unbounded_String("name")).Element.Element).Value;

         Insert(Translations, Assoc("id", Id));
         Insert(Translations, Assoc("display_name", Html_Name));
         Insert(Translations, Assoc("description", Html_Desc));
         Insert(Translations, Assoc("category", Torrent_Categories(The_Torrent.Category)));
         Insert(Translations, Assoc("uploader", Uploader.Username));
         Insert(Translations, Assoc("uploader_id", Uploader.Id));
         Insert(Translations, Assoc("is_uploader", Uploader = The_User or The_User.Role = 1));
         Insert(Translations, Assoc("passkey", The_User.Passkey));

         if Bencoded_Info.Value.Contains(To_Unbounded_String("files")) then
            declare
               Files : Bencode_List :=
                 Bencode_List(Bencoded_Info.Value(To_Unbounded_String("files")).Element.Element);
            begin
               for Bencoded_File of Files.Value loop
                  declare
                     File : Bencode_Dict := Bencode_Dict(Bencoded_File.Element);
                     File_Path_List : Bencode_List := Bencode_List(File.Value(To_Unbounded_String("path")).Element.Element);

                     File_Path : Unbounded_String;
                     Size : Long_Long_Integer := Bencode_Integer(File.Value(To_Unbounded_String("length")).Element.Element).Value;
                  begin
                     for Path_Part of File_Path_List.Value loop
                        File_Path := @ & "/" & Bencode_String(Path_Part.Element).Value;
                     end loop;

                     -- Just in case the file name has something funny, escape it. It's user generated data after all.
                     File_Names := @ & Templates_Parser.Utils.Web_Escape(To_String(File_Path));
                     File_Sizes := @ & Bytes_To_Printable(Size);
                     Total_Size := @ + Size;
                  end;
               end loop;
            end;
         else
            declare
               Size : Long_Long_Integer := Bencode_Integer(Bencoded_Info.Value(To_Unbounded_String("length")).Element.Element).Value;
            begin
               File_Names := @ & ("/" & Original_File_Name);
               File_Sizes := @ & Bytes_To_Printable(Size);
               Total_Size := Size;
            end;
         end if;
         Insert(Translations, Assoc("file_name", File_Names));
         Insert(Translations, Assoc("file_size", File_Sizes));
         Insert(Translations, Assoc("total_size", Bytes_To_Printable(Total_Size)));

         declare
            Peer_Data : Peers.Scrape_Stat_data := Peers.Protected_Map.Scrape_Stats(The_Torrent.Info_Hash);
         begin
            Insert(Translations, Assoc("seeding", Peer_Data.Complete));
            Insert(Translations, Assoc("leeching", Peer_Data.Incomplete));
            Insert(Translations, Assoc("snatches", Database.Torrent_Snatches(The_Torrent)));
         end;

         declare
            Stats : Detached_User_Torrent_Stat'Class := Database.Get_User_Stats_For_Torrent(The_User, The_Torrent.Id);
         begin
            if Stats /= Detached_User_Torrent_Stat'Class(No_Detached_User_Torrent_Stat) then
               Insert(Translations, Assoc("user_uploaded", Bytes_To_Printable(Stats.Uploaded)));
               Insert(Translations, Assoc("user_downloaded", Bytes_To_Printable(Stats.Downloaded)));
               Insert(Translations, Assoc("user_snatched", Stats.Snatched));
            end if;
         end;

         Insert(Translations, Assoc("is_subscribed", Torrent_Subscriptions.Subscribed(The_User, Detached_Torrent(The_Torrent))));

         declare
            use type Peers.Torrent_Maps.Map;

            Peers_Users, Peers_Uploaded, Peers_Downloaded, Peers_Percent : Vector_Tag;
            The_Peers : Peers.Peer_Maps.Map := Peers.Protected_Map.Peer_Map_Data(The_Torrent.Info_Hash);
         begin
            for Peer of The_Peers loop
               Peers_Users := @ & Peer.User.Username;
               Peers_Uploaded := @ & Bytes_To_Printable(Peer.Uploaded);
               Peers_Downloaded := @ & Bytes_To_Printable(Peer.Downloaded);

               declare
                  Downloaded_Percent : Long_Long_Float := (Long_Long_Float(Total_Size - Peer.Left) / Long_Long_Float(Total_Size)) * Long_Long_Float(100.0);
                  Formatted_Str : String (1 .. 16);

                  package Long_Long_Float_Text_IO is
                    new Ada.Text_IO.Float_IO (Long_Long_Float);
                  use Long_Long_Float_Text_Io;
               begin
                  Put(Formatted_Str, Downloaded_Percent, Aft => 2, Exp => 0);
                  Peers_Percent := @ & Trim(Formatted_Str, Ada.Strings.Both);
               end;
            end loop;
            Insert(Translations, Assoc("peer_user", Peers_Users));
            Insert(Translations, Assoc("peer_upload", Peers_Uploaded));
            Insert(Translations, Assoc("peer_download", Peers_Downloaded));
            Insert(Translations, Assoc("peer_percent", Peers_Percent));
         end;

         if The_Torrent.Group > 0 then
            declare
               The_Group : Detached_Torrent_Group'Class := Database.Get_Group(The_Torrent.Group);
            begin
               Insert(Translations, Assoc("group_name", The_Group.Name));
               Insert(Translations, Assoc("group_id", The_Group.Id));
            end;
         end if;

         if Error /= "" then
            Insert(Translations, Assoc("error", Error));
         end if;

         Insert(Translations, Assoc("urlencoded_name", Url.Encode(The_Torrent.Display_Name)));

         declare
            use Gnatcoll.Json;
            use Ada.Calendar, Ada.Calendar.Formatting;

            Torrent_Meta : Json_Value := Read(The_Torrent.Meta);
            Created_At : String := (if Torrent_Meta.Has_Field("created_at")
                                    then Torrent_Meta.Get("created_at")
                                    else "");

            Torrent_Links : Json_Value := (if Torrent_Meta.Has_Field("links")
                                           then Torrent_Meta.Get("links")
                                           else Create_Object);
            Link_Names, Link_Values : Vector_Tag;
         begin
            Insert(Translations, Assoc("created_at", Created_At));

            for Possible_Link of Possible_Meta_Links loop
               -- Check if the category is right
               if The_Torrent.Category = Possible_Link.Category or Possible_Link.Category = -1 then
                  if Torrent_Links.Has_Field(Possible_Link.Json_Name.Element) then
                     Link_Names := @ & Possible_Link.Name.Element;
                     Link_Values := @ & String'(Torrent_Links.Get(Possible_Link.Json_Name.Element));
                  end if;
               end if;
            end loop;
            Insert(Translations, Assoc("link_name", Link_Names));
            Insert(Translations, Assoc("link_value", Link_Values));

            if Torrent_Meta.Has_Field("image") then
               Insert(Translations, Assoc("image", String'(Torrent_Meta.Get("image"))));
            end if;
         end;

         Replies_Translations(The_Torrent.Id, The_User, Translations, Database.Torrent_Comments'Access, Request);
         Userinfo_Translations(The_User, Translations);

         return Response.Build(Mime.Text_Html,
                               String'(Templates_Parser.Parse("assets/view.html", Translations)));
      end;
   end Dispatch;

   overriding function Dispatch(Handler : in Invite_Handler;
                                Request : in Status.Data) return Response.Data is
      Session_Id : Session.Id := Request_Session(Request);
      Username : String := Session.Get(Session_Id, "username");
   begin
      if not Database.User_Exists(Username) then
         -- Redirect to the login page
         return Response.Url(Location => "/login");
      end if;

      declare
         The_User : Detached_User'Class := Database.Get_User(Username);
         The_Invite : String := Database.Create_Invite(The_User);

         Invited_Users : Invite_List := Database.Get_Invited_Users(The_User);

         Translations : Translate_Set;
         Invited_User : Detached_User'Class := No_Detached_User;

         Invited_User_Names, Invited_User_Ul, Invited_User_Dl : Vector_Tag;
      begin
         Insert(Translations, Assoc("invite", The_Invite));

         while Invited_Users.Has_row loop
            Invited_User := Invited_Users.Element.For_User.Detach;

            Invited_User_Names := @ & Invited_User.Username;
            Invited_User_Ul := @ & Bytes_To_Printable(Invited_User.Uploaded);
            Invited_User_Dl := @ & Bytes_To_Printable(Invited_User.Downloaded);

            Invited_Users.Next;
         end loop;
         Insert(Translations, Assoc("invited_name", Invited_User_Names));
         Insert(Translations, Assoc("invited_uploaded", Invited_User_Ul));
         Insert(Translations, Assoc("invited_downloaded", Invited_User_Dl));

         Userinfo_Translations(The_User, Translations);

         return Response.Build(Mime.Text_Html,
                               String'(Templates_Parser.Parse("assets/invite.html", Translations)));
      end;
   end Dispatch;

   overriding function Dispatch(Handler : in Confirm_Handler;
                                Request : in Status.Data) return Response.Data is
      Session_Id : Session.Id := Request_Session(Request);
      Username : String := Session.Get(Session_Id, "username");

      Translations : Translate_Set;

      Params : Parameters.List := Status.Parameters(Request);
      Action : String := Params.Get("action");
      Ok : String := Params.Get("ok");
   begin
      if not Database.User_Exists(Username) then
         -- Redirect to the login page
         return Response.Url(Location => "/login");
      end if;

      Insert(Translations, Assoc("action", Action));
      Insert(Translations, Assoc("back", Status.Header(Request).Get_Values("Referer")));
      Insert(Translations, Assoc("ok", Ok));

      Userinfo_Translations(Database.Get_User(Username), Translations);

      return Response.Build(Mime.Text_Html,
                               String'(Templates_Parser.Parse("assets/confirm.html", Translations)));
   end Dispatch;


   Profile_Matcher : constant Pattern_Matcher := Compile("/profile/((\w|\d)+)");
   overriding function Dispatch(Handler : in Profile_Handler;
                                Request : in Status.Data) return Response.Data is
      Session_Id : Session.Id := Request_Session(Request);
      Username : String := Session.Get(Session_Id, "username");

      Params : constant Parameters.List := Status.Parameters(Request);
      Error : String := Params.Get("error");

      Translations : Translate_Set;

      use type Aws.Status.Request_Method;
   begin
      if not Database.User_Exists(Username) then
         -- Redirect to the login page
         return Response.Url(Location => "/login");
      end if;

      if Status.Method(Request) = Status.Post then
         declare
            use Sodium.Functions;
            use Gnatcoll.Json;

            Current_User : Detached_User'Class := Database.Get_User(Username);
            Profile_User : Detached_User'Class := Database.Get_User(Uri_Group_Match(Request, Profile_Matcher, 1));

            The_User : Detached_User'Class := Database.Get_User(Username);
            Profile_Json : Json_Value := Read(The_User.Profile);

            Generate_Irc_Key : String := Params.Get("generate-irc-key");

            Password : String := Params.Get("password");
            Confirm_Password : String := Params.Get("confirm-password");
         begin
            if Current_User /= Profile_User then
               return Response.Acknowledge(Messages.S403, "Forbidden");
            end if;

            if Password /= "" then
               declare
                  Error_String : String_Holders.Holder := Validate_Password(Password, Confirm_Password);
               begin
                  if not Error_String.Is_Empty then
                     return Referer_With_Error(Request, Error_String.Element);
                  end if;
               end;
            end if;

            Profile_Json.Set_Field("profile_picture", Params.Get("profile_picture"));
            Profile_Json.Set_Field("about", Params.Get("about"));
            if Generate_Irc_Key /= "" then
               Profile_Json.Set_Field("irc_key", As_Hexidecimal(Random_Hash_Key(16)));
            end if;

            declare
               Email : String := Params.Get("email");
               Email_Confirmation : String := Params.Get("email-confirmation");
               Email_Notifications : String := Params.Get("email-notifications");

               Profile_Email : Json_Value := (if Profile_Json.Has_Field("email")
                                              then Profile_Json.Get("email")
                                              else Create_Object);
            begin
               if Email /= "" then
                  if Profile_Email.Has_Field("unconfirmed_address") and then (Email = String'(Profile_Email.Get("unconfirmed_address")))
                    and then Email_Confirmation /= "" then
                     if Email_Confirmation = String'(Profile_Email.Get("confirmation_code")) then
                        Profile_Email.Unset_Field("unconfirmed_address");
                        Profile_Email.Unset_Field("confirmation_code");
                        Profile_Email.Set_Field("address", Email);
                     else
                        return Referer_With_Error(Request, "Wrong confirmation code");
                     end if;
                  elsif (Profile_Email.Has_Field("address") and then not (Profile_Email.Get("address") = Email))
                    or else not Profile_Email.Has_Field("address") then
                     declare
                        New_Confirmation_Code : String := As_Hexidecimal(Random_Hash_Key(16));
                     begin
                        Profile_Email.Set_Field("unconfirmed_address", Email);
                        Profile_Email.Set_Field("confirmation_code", New_Confirmation_Code);

                        Hellish_Mail.Send(Smtp.E_Mail(Profile_User.Username, Email),
                                          Subject => "Hellish e-mail confirmation",
                                          Message => "Your confirmation code is **" & New_Confirmation_Code & "**");
                     end;
                  end if;
                  if Email_Notifications /= "" then
                     Profile_Email.Set_Field("notifications", True);
                  else
                     Profile_Email.Set_Field("notifications", False);
                  end if;

                  Profile_Json.Set_Field("email", Profile_Email);
               else
                  Profile_Json.Unset_Field("email");
               end if;
            end;

            The_User.Set_Profile(Profile_Json.Write);
            Database.Update_User(The_User);
         end;

         return Response.Url(Location => Status.Uri(Request));
      end if;

      declare
         Current_User : Detached_User'Class := Database.Get_User(Username);
         Profile_User : Detached_User'Class := Database.Get_User(Uri_Group_Match(Request, Profile_Matcher, 1));
      begin
         Insert(Translations, Assoc("username", Profile_User.Username));
         Insert(Translations, Assoc("user_id", Profile_User.Id));
         Insert(Translations, Assoc("uploaded", Bytes_To_Printable(Profile_User.Uploaded)));
         Insert(Translations, Assoc("downloaded", Bytes_To_Printable(Profile_User.Downloaded)));

         declare
            Total_Uploads, Total_Posts, Total_Snatches : Natural;
            Unused_Upload_List : Direct_Torrent_List := Database.Search_Torrents("",
                                                                                 Uploader => Profile_User.Id,
                                                                                 Category => -1,
                                                                                 Snatched_By => -1,
                                                                                 Offset => 0, Limit => 0,
                                                                                 Total_Count => Total_Uploads);
            Unsused_Post_List : Post_List := Database.Search_Posts("", -1, Profile_User.Id, 0, 0, Total_Posts);
            Unused_Snatch_List : Direct_Torrent_List :=
              Database.Search_Torrents("",
                                       Uploader => 0,
                                       Category => -1,
                                       Snatched_By => Profile_User.Id,
                                       Offset => 0, Limit => 0,
                                       Total_Count => Total_Snatches);
         begin
            Insert(Translations, Assoc("upload_count", Total_Uploads));
            Insert(Translations, Assoc("post_count", Total_Posts));
            Insert(Translations, Assoc("snatch_count", Total_Snatches));
         end;

         declare
            use Gnatcoll.Json;
            Profile_Json : Json_Value := Read(Profile_User.Profile);

            Profile_Picture : String := (if Profile_Json.Has_Field("profile_picture")
                                         then Profile_Json.Get("profile_picture")
                                         else "");
            About_Text : String := (if Profile_Json.Has_Field("about")
                                    then Profile_Json.Get("about")
                                    else "");
            Notifications : Json_Array :=  (if Has_Field(Profile_Json, "notifications")
                                            then Get(Profile_Json, "notifications")
                                            else Empty_Array);
            Html_Notifications : Vector_Tag;
         begin
            Insert(Translations, Assoc("profile_picture", Profile_Picture));
            Insert(Translations, Assoc("encoded_profile_picture", Templates_Parser.Utils.Web_Escape(Profile_Picture)));

            Insert(Translations, Assoc("profile_about", About_Text));
            Insert(Translations, Assoc("rendered_about", Process_Content(About_Text)));

            for Notification of Notifications loop
               Html_Notifications := @ & Markdown.To_Html(Get(Notification), Default_Md_Flags);
            end loop;
            Insert(Translations, Assoc("notification", Html_Notifications));

            if Has_Field(Profile_Json, "irc_key") then
               Insert(Translations, Assoc("irc_key", String'(Get(Profile_Json, "irc_key"))));
               Insert(Translations, Assoc("irc_host", Hellish_Irc.Irc_Host.Element & ":" & Trim(Hellish_Irc.Port'Image, Ada.Strings.Left) &
                                            " (" & Trim(Hellish_Irc.Port_Ssl'Image, Ada.Strings.Left) & " for TLS)"));
            end if;

            if Profile_Json.Has_Field("email") and then Profile_Json.Get("email").Has_Field("unconfirmed_address") then
               Insert(Translations, Assoc("unconfirmed_email", String'(Profile_Json.Get("email").Get("unconfirmed_address"))));
            elsif Profile_Json.Has_Field("email") and then Profile_Json.Get("email").Has_Field("address") then
               Insert(Translations, Assoc("confirmed_email", String'(Profile_Json.Get("email").Get("address"))));
            end if;
            if Profile_Json.Has_Field("email") and then Profile_Json.Get("email").Has_Field("notifications") then
               Insert(Translations, Assoc("email_notifications", Boolean'(Profile_Json.Get("email").Get("notifications"))));
            end if;
         end;
         Insert(Translations, Assoc("is_owner", Current_User = Profile_User));

         if Error /= "" then
            Insert(Translations, Assoc("error", Error));
         end if;

         Userinfo_Translations(Current_User, Translations);
      end;

      return Response.Build(Mime.Text_Html,
                               String'(Templates_Parser.Parse("assets/profile.html", Translations)));
   end Dispatch;

   overriding function Dispatch(Handler : in Admin_Handler;
                                Request : in Status.Data) return Response.Data is
      Session_Id : Session.Id := Request_Session(Request);
      Username : String := Session.Get(Session_Id, "username");

      Translations : Translate_Set;

      The_User : Detached_User'Class := No_Detached_User;
   begin
      if not Database.User_Exists(Username) then
         -- Redirect to the login page
         return Response.Url(Location => "/login");
      end if;

      The_User := Database.Get_User(Username);
      if The_User.Role /= 1 then
         return Response.Url(Location => "/");
      end if;

      declare
         Invited_List : Invite_List := Database.Admin_Recently_Invited(10);

         Invited_Usernames, Inviter_Usernames : Vector_Tag;
      begin
         while Invited_List.Has_Row loop
            Invited_Usernames := @ & Invited_List.Element.For_User.Username;
            Inviter_Usernames := @ & Invited_List.Element.By_User.Username;

            Invited_List.Next;
         end loop;
         Insert(Translations, Assoc("invited_username", Invited_Usernames));
         Insert(Translations, Assoc("inviter_username", Inviter_Usernames));
      end;

      Userinfo_Translations(The_User, Translations);

      return Response.Build(Mime.Text_Html,
                            String'(Templates_Parser.Parse("assets/admin.html", Translations)));
   end Dispatch;

   -- API

   function Api_Upload_Dispatch(Handler : in Api_Upload_Handler;
                                Request : in Status.Data) return Response.Data is separate;
   function Dispatch
     (Handler : in Api_Upload_Handler;
      Request : in Status.Data) return Response.Data renames Api_Upload_Dispatch;

   Api_Delete_Matcher : constant Pattern_Matcher := Compile("/api/delete/(\d+)");
   overriding function Dispatch(Handler : in Api_Delete_Handler;
                                Request : in Status.Data) return Response.Data is
      Session_Id : Session.Id := Request_Session(Request);
      Username : String := Session.Get(Session_Id, "username");
      The_User : Detached_User'Class := No_Detached_User;
   begin
      if not Database.User_Exists(Username) then
         return Response.Acknowledge(Messages.S403, "Forbidden");
      end if;
      The_User := Database.Get_User(Username);

      declare
         use Ada.Directories;

         Id : Natural := Natural'Value(Uri_Group_Match(Request, Api_Delete_Matcher, 1));
         The_Torrent : Detached_Torrent'Class := Database.Get_Torrent(Id);
      begin
         if Natural'(The_Torrent.Created_By) /= The_User.Id and The_User.Role /= 1 then
            return Response.Acknowledge(Messages.S403, "Forbidden");
         end if;

         Peers.Protected_Map.Remove_Torrent (The_Torrent.Info_Hash);
         Database.Delete_Torrent(Id);
         Ada.Directories.Delete_File(Compose(Containing_Directory => Uploads_Path,
                                             Name => The_Torrent.Info_Hash,
                                             Extension => "torrent"));

         return Response.Url("/");
      end;
   end Dispatch;

   Username_Matcher : constant Pattern_Matcher := Compile("(\w|\d)+");
   function Dispatch(Handler : in Api_User_Register_Handler;
                     Request : in Status.Data) return Response.Data is
      Params : constant Parameters.List := Status.Parameters(Request);
      Username : String := Params.Get("username");
      Password : String := Params.Get("password");
      Confirm_Password : String := Params.Get("confirm-password");
      Invite : String := Params.Get("invite");

      Min_Name_Length : constant Positive := 1;
      Max_Name_Length : constant Positive := 32;

      Error_String : String_Holders.Holder;
   begin
      if Username'Length < Min_Name_Length and Username'Length > Max_Name_Length then
         Error_String := To_Holder("Username must be at least" & Min_Name_Length'Image & " characters long and at most"
                                     & Max_Name_Length'Image & " characters long");

         goto Finish;
      end if;

      if not Match(Username_Matcher, Username) then
         Error_String := To_Holder("Username must only contain alphanumeric characters or underscores");

         goto Finish;
      end if;

      Error_String := Validate_Password(Password, Confirm_Password);
      if not Error_String.Is_Empty then
         goto Finish;
      end if;

      if Invite_Required and then not Database.Invite_Valid(Invite) then
         Error_String := To_Holder("Invalid invite");

         goto Finish;
      end if;

      declare
         New_User : Detached_User'Class := No_Detached_User;
         Created : Boolean := Database.Create_User(Username, Password, New_User);
      begin
         if not Created then
            Error_String := To_Holder("Username already taken");

            goto Finish;
         end if;

         if Invite_Required then
            Database.Invite_Use(Invite, New_User);
         end if;
      end;

   <<Finish>>
      if Error_String.Is_Empty then
         declare
            -- Always associate a new session with the request on login,
            -- doesn't seem to work well otherwise
            Session_Id : Session.ID := Status.Session(Request);
         begin
            Session.Set(Session_Id, "username", Username);
            Session.Save(Session_File_Name);

            return Response.Url(Location => "/login");
         end;
      else
         return Response.Url(Location => "/register?error=" & Error_String.Element);
      end if;
   end Dispatch;

   overriding function Dispatch(Handler : in Api_User_Login_Handler;
                                Request : in Status.Data) return Response.Data is
      Params : constant Parameters.List := Status.Parameters(Request);
      Username : String := Params.Get("username");
      Password : String := Params.Get("password");

      Success : Boolean := Database.Verify_User_Credentials(Username, Password);

      -- Always associate a new session with the request on login,
      -- doesn't seem to work well otherwise
      Session_Id : Session.ID := Status.Session(Request);
   begin
      if Success then
         declare
            User : Detached_User'Class := Database.Get_User(Username);
         begin
            Session.Set(Session_Id, "username", User.Username);
            Session.Save(Session_File_Name);

            -- Redirect to login page
            return Response.Url(Location => "/");
         end;
      else
         return Response.Url(Location => "/login?error=Invalid username or password");
      end if;
   end Dispatch;

   overriding function Dispatch(Handler : in Api_User_Logout_Handler;
                                Request : in Status.Data) return Response.Data is
      Session_Id : Session.Id := Request_Session(Request);
   begin
      Session.Delete(Session_Id);
      Session.Save(Session_File_Name);

      return Result : Response.Data := Response.Url("/login") do
         Response.Set.Add_Header(Result, "Set-Cookie", AWS.Server.Session_Name & "=; Path=/");
      end return;
   end Dispatch;

   overriding function Dispatch(Handler : in Api_Subscribe_Handler;
                                Request : in Status.Data) return Response.Data is
      Session_Id : Session.Id := Request_Session(Request);
      Username : String := Session.Get(Session_Id, "username");
      The_User : Detached_User'Class := No_Detached_User;

      Params : constant Parameters.List := Status.Parameters(Request);
      Is_Unsub : Boolean := Params.Get("unsubscribe") /= "";
      Parent_Type : String := Params.Get("type");
      Parent_Id : Natural := Natural'Value(Params.Get("id"));
   begin
      if not Database.User_Exists(Username) then
         return Response.Acknowledge(Messages.S403, "Forbidden");
      end if;

      The_User := Database.Get_User(Username);
      if Parent_Type = "post" then
         if Is_Unsub then
            Post_Subscriptions.Unsubscribe(The_User, Detached_Post(Database.Get_Post(Parent_Id)));
         else
            Post_Subscriptions.Subscribe(The_User, Detached_Post(Database.Get_Post(Parent_Id)));
         end if;
      elsif Parent_Type = "torrent" then
         if Is_unsub then
            Torrent_Subscriptions.Unsubscribe(The_User, Detached_Torrent(Database.Get_Torrent(Parent_Id)));
         else
            Torrent_Subscriptions.Subscribe(The_User, Detached_Torrent(Database.Get_Torrent(Parent_Id)));
         end if;
      end if;

      return Response.Url(Location => Status.Header(Request).Get_Values("Referer"));
   end Dispatch;

   overriding function Dispatch(Handler : in Api_Notifications_Clear_Handler;
                                Request : in Status.Data) return Response.Data is
      Session_Id : Session.Id := Request_Session(Request);
      Username : String := Session.Get(Session_Id, "username");

   begin
      if not Database.User_Exists(Username) then
         return Response.Acknowledge(Messages.S403, "Forbidden");
      end if;

      declare
         use Gnatcoll.Json;
         The_User : Detached_User'Class := Database.Get_User(Username);
         The_Profile : Json_Value := Read(The_User.Profile);
      begin
         Unset_Field(The_Profile, "notifications");
         The_User.Set_Profile(The_Profile.Write);

         Database.Update_User(The_User);
      end;

      return Response.Url(Location => Status.Header(Request).Get_Values("Referer"));
   end Dispatch;

   -- Uploads

   Uploads_Images_Matcher : constant Pattern_Matcher := Compile("/uploads/images/(\w+\.\w+)");
   overriding function Dispatch(Handler : in Uploads_Images_Handler;
                                Request : in Status.Data) return Response.Data is
      use Ada.Directories;

      Filename : String := Uri_Group_Match(Request, Uploads_Images_Matcher, 1);
      Full_Path : String := Compose(Containing_Directory => Image_Uploads_Path, Name => Filename);
   begin
      if Exists(Full_Path) then
         return Response.File(Mime.Content_Type(Filename), Full_Path);
      else
         return Response.Acknowledge(Messages.S404, "Image not found");
      end if;
   end Dispatch;

   -- Entrypoint

   procedure Exception_Handler(E : Exception_Occurrence;
                               Log    : in out Aws.Log.Object;
                               Error  : Aws.Exceptions.Data;
                               Answer : in out Response.Data) is
      Response_Html : String := "<!DOCTYPE HTML><body>" &
        "<b>" & Exception_Name(E) & "</b>" &
        "<pre>" & Exception_Information(E) & "</pre>" &
        "<pre>" & GNAT.Traceback.Symbolic.Symbolic_Traceback(E) & "</pre></body>";
   begin
      Answer := Response.Build(Mime.Text_Html, Response_Html);
   end Exception_Handler;

   procedure Run_Server is
      use GNAT.Command_Line;

      Config_File : String_Holders.Holder := To_Holder("hellish_config.ini");
   begin
      Server.Set_Unexpected_Exception_Handler(Http, Exception_Handler'Access);

      loop
      case Getopt("-invite-not-required -https -server-host= -ssl-cert= -ssl-privkey= -config=") is
            when '-' =>
               if Full_Switch = "-invite-not-required" then
                  Invite_Required := False;
               elsif Full_Switch = "-config" then
                  Config_File := To_Holder(Parameter);
               end if;
            when others =>
               exit;
         end case;
      end loop;

      declare
         use Gnatcoll.Config;

         Conf_Parser : Ini_Parser;
         Config : Config_Pool;
      begin
         Open(Conf_Parser, Config_File.Element);
         Fill(Config, Conf_Parser);

         if Config.Get("http.https") /= "" and then Config.Get_Boolean("http.https") then
            Https := True;
         end if;
         if Config.Get("http.server_host") /= "" then
            Server_Host := To_Unbounded_String(Config.Get("http.server_host"));
         end if;
         if Config.Get("irc.ssl_cert") /= "" then
            Hellish_Irc.Ssl_Cert_Path := Hellish_Irc.String_Holders.To_Holder(Config.Get("irc.ssl_cert"));
            Hellish_Irc.Ssl_Privkey_Path := Hellish_Irc.String_Holders.To_Holder(Config.Get("irc.ssl_privkey"));
         end if;
         if Config.Get("mail.smtp_server") /= "" then
            Hellish_Mail.Smtp_Server := Hellish_Mail.String_Holders.To_Holder(Config.Get("mail.smtp_server"));
            Hellish_Mail.Smtp_Port := Config.Get_Integer("mail.smtp_port");
            Hellish_Mail.From_Address := Hellish_Mail.String_Holders.To_Holder(Config.Get("mail.from_address"));
         end if;
      end;

      Database.Init;
      Peers.Protected_Map.Load_Persisted_Peers;

      if Ada.Directories.Exists(Session_File_Name) then
         Session.Load(Session_File_Name);
      end if;

      Services.Dispatchers.Uri.Register(Root, "/", Index);
      Services.Dispatchers.Uri.Register(Root, "/login", Login);
      Services.Dispatchers.Uri.Register(Root, "/register", Register);
      Services.Dispatchers.Uri.Register_Regexp(Root, "/(\w+)/announce", Announce);
      Services.Dispatchers.Uri.Register_Regexp(Root, "/(\w+)/scrape", Scrape);
      Services.Dispatchers.Uri.Register_Regexp(Root, "/download/(\d+)", Download);
      Services.Dispatchers.Uri.Register(Root, "/upload", Upload);
      Services.Dispatchers.Uri.Register_Regexp(Root, "/view/(\d+)", View);
      Services.Dispatchers.Uri.Register(Root, "/invite", Invite);
      Services.Dispatchers.Uri.Register(Root, "/search", Search.Search);
      Services.Dispatchers.Uri.Register(Root, "/search.rss", Search.Search_Rss);
      Services.Dispatchers.Uri.Register(Root, "/post/create", Posts.Post_Create);
      Services.Dispatchers.Uri.Register(Root, "/post/search", Posts.Post_Search);
      Services.Dispatchers.Uri.Register_Regexp(Root, "/post/(\d+)", Posts.Post);
      Services.Dispatchers.Uri.Register(Root, "/confirm", Confirm);
      Services.Dispatchers.Uri.Register(Root, "/images", Images.Images);
      Services.Dispatchers.Uri.Register_Regexp(Root, "/profile/((\w|\d)+)", Profile);
      Services.Dispatchers.Uri.Register(Root, "/admin", Admin);
      Services.Dispatchers.Uri.Register(Root, "/group/create", Groups.Group_Create);
      Services.Dispatchers.Uri.Register_Regexp(Root, "/group/(\d+).rss", Groups.Group_Rss);
      Services.Dispatchers.Uri.Register_Regexp(Root, "/group/(\d+)", Groups.Group);
      Services.Dispatchers.Uri.Register(Root, "/group/search", Groups.Group_Search);

      -- API

      Services.Dispatchers.Uri.Register(Root, "/api/user/register", Api_User_Register);
      Services.Dispatchers.Uri.Register(Root, "/api/user/login", Api_User_Login);
      Services.Dispatchers.Uri.Register(Root, "/api/user/logout", Api_User_Logout);
      Services.Dispatchers.Uri.Register(Root, "/api/upload", Api_Upload);
      Services.Dispatchers.Uri.Register_Regexp(Root, "/api/delete/(\d+)", Api_Delete);
      Services.Dispatchers.Uri.Register(Root, "/api/post/create", Posts.Api_Post_Create);
      Services.Dispatchers.Uri.Register(Root, "/api/upload/image", Images.Api_Image_Upload);
      Services.Dispatchers.Uri.Register_Regexp(Root, "/api/delete/image/(\d+)", Images.Api_Image_Delete);
      Services.Dispatchers.Uri.Register(Root, "/api/subscribe", Api_Subscribe);
      Services.Dispatchers.Uri.Register(Root, "/api/notifications/clear", Api_Notifications_Clear);
      Services.Dispatchers.Uri.Register(Root, "/api/group/create", Groups.Api_Group_Create);

      -- Uploads

      Services.Dispatchers.Uri.Register_Regexp(Root, "/uploads/images/(\w+\.\w+)", Uploads_Images);

      Server.Start(Hellish_Web.Routes.Http, Root, Conf);
      Server.Log.Start(Http, Put_Line'Access, "hellish");

      Hellish_Irc.Start;
      Hellish_Mail.Start;

      Put_Line("Started on http://" & Aws.Config.Server_Host(Conf)
                 -- Trim the number string on the left because it has a space for some reason
                 & ":" & Trim(Aws.Config.Server_Port(Conf)'Image, Ada.Strings.Left));
      Server.Wait(Server.Q_Key_Pressed);

      Server.Shutdown(Http);
   end Run_Server;

   procedure Shutdown_Server is
   begin
      Server.Shutdown(Http);
   end Shutdown_Server;
end Hellish_Web.Routes;
