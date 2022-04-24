with Gnatcoll.Sql; use Gnatcoll.Sql;
with Gnatcoll.Sql.Sessions;

separate (Hellish_Web.Routes)
function Api_Upload_Dispatch(Handler : in Api_Upload_Handler;
                             Request : in Status.Data) return Response.Data is
   use Ada.Directories;

   Params : constant Parameters.List := Status.Parameters(Request);

   File_Path : String := Params.Get("file");
   Display_Name : String := Params.Get("name");
   Description : String := Params.Get("description");
   Category : Integer := Integer'Value(Params.Get("category"));
   Update : String := Params.Get("update");
   Group : String := Params.Get("group");

   Session_Id : Session.Id := Request_Session(Request);
   Username : String := Session.Get(Session_Id, "username");

   procedure Set_Updatable_Fields_And_Create(The_Torrent : in out Detached_Torrent'class) is
   begin
      The_Torrent.Set_Display_Name(Display_Name);
      The_Torrent.Set_Description(Description);

      if Group /= "" then
         The_Torrent.Set_Group(Database.Get_Group(Group));
      else
         The_Torrent.Set_Group(-1);
      end if;

      if Torrent_Categories.Contains(Category) then
         The_Torrent.Set_Category(Category);
      end if;

      Database.Create_Torrent(The_Torrent);
   end Set_Updatable_Fields_And_Create;
begin
   if not Database.User_Exists(Username) then
      return Response.Acknowledge(Messages.S403, "Forbidden");
   end if;

   if Group /= "" and then Database.Get_Group(Group) = Detached_Torrent_Group'Class(No_Detached_Torrent_Group) then
      return Response.Url("/upload?error=The group """ & Url.Encode(Group) & """ does not exist.");
   end if;

   if Update /= "" then
      declare
         Update_Id : Integer := Integer'Value(Update);
         The_Torrent : Detached_Torrent'Class := Database.Get_Torrent(Update_Id);
         The_User : Detached_User'Class := Database.Get_User(Username);
      begin
         -- Only the creator and the admins can update the torrent
         if Integer'(The_Torrent.Created_By) /= The_User.Id and The_User.Role /= 1 then
            return Response.Acknowledge(Messages.S403, "Forbidden");
         end if;

         Set_Updatable_Fields_And_Create(The_Torrent);

         return Response.Url("/view/" & Update);
      end;
   end if;

   declare
      use Bencoder;
      File : File_Type;

      Decoded : Bencode_Dict;
      Decoded_Info : Bencode_Dict;

      Error_String : Unbounded_String;
   begin
      Open(File, Mode => In_File, Name => File_Path);
      Decoded := Bencode_Dict(Decode(File).Element);
      Close(File);

      Decoded_Info := Bencode_Dict(Decoded.Value(To_Unbounded_String("info")).Element.Element);

      if Bencode_Integer(Decoded_Info.Value(To_Unbounded_String("private")).Element.Element).Value /= 1 then
         Decoded_Info.Include("private", Encode(Natural'(1)));
         Decoded.Include("info", Bencode_Value_Holders.To_Holder(Decoded_Info));
         Error_String := To_Unbounded_String("The torrent wasn't set as private, you have to redownload it from this page for it to work.");
      elsif Decoded_Info.Value.Contains(To_Unbounded_String("meta version")) then
         return
           Response.Url("/upload?error=You have uploaded a V2/Hybrid torrent, these are not allowed, as some clients still don't support them.");
      end if;

      declare
         Bencoded_Info : Unbounded_String := Decoded_Info.Encoded;
         Sha1_Hash : String := Gnat.Sha1.Digest(To_String(Bencoded_Info));
         New_Name : String := Compose(Name => Sha1_Hash, Extension => "torrent");
         Uploaded_Path : String := Compose(Containing_Directory => Uploads_Path,Name => New_Name);

         Created_By : Detached_User'Class := Database.Get_User(Username);
         Maybe_Existing : Detached_Torrent'Class := Database.Get_Torrent_By_Hash(Sha1_Hash);
         The_Torrent : Detached_Torrent'Class := New_Torrent;

         Created_File : File_Type;
      begin
         if Detached_Torrent(Maybe_Existing) /= No_Detached_Torrent then
            return Response.Url("/view/" & Trim(Maybe_Existing.Id'Image, Ada.Strings.Left)
                                  & "?error=You have uploaded a torrent that already exists.");
         end if;

         Create_Path(Uploads_Path);

         Create(Created_File, Mode => Out_File, Name => Uploaded_Path);
         Put(Created_File, To_String(Decoded.Encoded));
         Close(Created_File);

         declare
            use Gnatcoll.Json;
            use Ada.Calendar, Ada.Calendar.Formatting;

            Torrent_Meta : Json_Value := Read(The_Torrent.Meta);
         begin
            -- Set date to right now
            Torrent_Meta.Set_Field("created_at", Image(Clock));
            The_Torrent.Set_Meta(Write(Torrent_Meta));
         end;

         -- Only really need to save the hash, since it's the filename. Things like actual file name and such
         -- are encoded in the torrent file itself.
         -- The fields mentioned here explicitly can't be updated
         The_Torrent.Set_Info_Hash(Sha1_Hash);
         The_Torrent.Set_Created_By(Created_By);
         Set_Updatable_Fields_And_Create(The_Torrent);

         Torrent_Subscriptions.Subscribe(Created_By, Detached_Torrent(The_Torrent));

         if Error_String = "" then
            return Response.Url("/view/" & Trim(The_Torrent.Id'Image, Ada.Strings.Left));
         else
            return Response.Url("/view/" & Trim(The_Torrent.Id'Image, Ada.Strings.Left) & "?error=" & To_String(Error_String));
         end if;
      end;
   end;
end Api_Upload_Dispatch;
