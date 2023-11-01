with Ada.Streams;

with Gnatcoll.Sql; use Gnatcoll.Sql;

with Aws.Client;

with Lexbor;

separate (Hellish_Web.Routes)
function Api_Upload_Dispatch(Handler_Unused : in Api_Upload_Handler;
                             Request : in Status.Data) return Response.Data is
   use Ada.Directories;

   Params : constant Parameters.List := Status.Parameters(Request);

   File_Path : String := Params.Get("file");
   Display_Name : String := Trim(Params.Get("name"), Ada.Strings.Both);
   Description : String := Params.Get("description");
   Category : Integer := Integer'Value(Params.Get("category"));
   Update : String := Params.Get("update");
   Group : String := Trim(Params.Get("group"), Ada.Strings.Both);
   Torrent_Image : String := Params.Get("image");
   Autofind_Image : String := Params.Get("autofind-image");

   Session_Id : Session.Id := Request_Session(Request);
   Username : String := Session.Get(Session_Id, "username");

   function Set_Updatable_Fields_And_Create(The_Torrent : in out Detached_Torrent'class) return String_Holders.Holder is
   begin
      The_Torrent.Set_Display_Name(Display_Name);
      The_Torrent.Set_Description(Description);

      if Torrent_Categories.Contains(Category) then
         The_Torrent.Set_Category(Category);
      end if;

      declare
         use Gnatcoll.Json;

         Torrent_Meta : Json_Value := Read(The_Torrent.Meta);

         Link_Meta : Json_Value := Create_Object;
      begin
         for Possible_Link of Possible_Meta_Links loop
            -- Check if the category is right
            if Category = Possible_Link.Category or Possible_Link.Category = -1 then
               declare
                  Maybe_Meta_Link : String := Trim(Params.Get("meta-" & Possible_Link.Json_Name.Element), Ada.Strings.Both);
               begin
                  -- Check if the strings starts with prefix and isn't empty
                  if Maybe_Meta_Link /= "" then
                     declare
                        Condition : Boolean :=
                          (case Possible_Link.Link_Type is
                              when Prefix => Ada.Strings.Fixed.Index(Maybe_Meta_link, Possible_Link.Link.Element) = 1,
                              when Contains => Ada.Strings.Fixed.Index(Maybe_Meta_link, Possible_Link.Link.Element) /= 0);
                        Fail_String : String_Holders.Holder :=
                          To_Holder("Link for "
                                      & Possible_Link.Name.Element
                                      & " must "
                                      & (case Possible_Link.Link_Type is
                                            when Prefix => "start with ",
                                            when Contains => "contain ")
                                      & Possible_Link.Link.Element);
                     begin
                        if Condition then
                           Link_Meta.Set_Field(Possible_Link.Json_Name.Element, Maybe_Meta_Link);
                        else
                           return Fail_String;
                        end if;
                     end;
                  end if;
               end;
            end if;
         end loop;
         if Link_Meta.Is_Empty then
            Torrent_Meta.Unset_Field("links");
         else
            Torrent_Meta.Set_Field("links", Link_Meta);
         end if;

         if Autofind_Image /= "" then
            for Possible_Link of Possible_Meta_Links loop
               if Possible_Link.Autofind_Image and Link_Meta.Has_Field(Possible_Link.Json_Name.Element) then
                  declare
                     use Aws.Response;
                     use type Ada.Streams.Stream_Element_Offset;
                     use Lexbor;

                     Max_Page_Size : constant := 1024 * 1024;

                     Timeouts : Aws.Client.Timeouts_Values :=
                       Aws.Client.Timeouts(Connect => 5.0, Send => 5.0, Receive => 5.0, Response => 5.0);
                     Page_Data : Response.Data :=
                       Aws.Client.Get(Link_Meta.Get(Possible_Link.Json_Name.Element), Timeouts => Timeouts, Follow_Redirection => True);

                     Doc : Html_Document;
                     Doc_Head : Dom_Element'Class := No_Dom_Element;
                     Col : Dom_Collection'Class := Make_Dom_Collection(Doc, 1);
                  begin
                     if Content_Length(Page_Data) <= Max_Page_Size
                       and Ada.Strings.Fixed.Index(Content_Type(Page_Data), Mime.Text_Html) = 1 then
                        Parse(Doc, Message_Body(Page_Data));
                        Doc_Head := Document_Head(Doc);

                        Dom_Elements_By_Attr(Doc_Head, Col, "property", "og:image", False);

                        if Length(Col) > 0 then
                           declare
                              Image_Elem : Dom_Element'Class := Element(Col, 0);
                              Image_Url : String := Get_Attribute(Image_Elem, "content");
                              Image_Data : Response.Data :=
                                Aws.Client.Get(Image_Url, Timeouts => Timeouts, Follow_Redirection => True);

                              Temp_File_Path : String := Compose(Containing_Directory => Image_Uploads_Path,
                                                                 Name => "temp_" & Simple_Name(Image_Url));
                              Temp_File : File_Type;
                           begin
                              Create(File => Temp_File, Name => Temp_File_Path);
                              Put(Temp_File, Message_Body(Image_Data));
                              Close(Temp_File);

                              declare
                                 Success : Boolean;
                                 Upload_Result : String := Images.Image_From_Path(Temp_File_Path, Username, Success);
                              begin
                                 Delete_File(Temp_File_Path);
                                 if Success then
                                    Torrent_Meta.Set_Field("image",
                                                           "/uploads/images/" & Database.Get_Image(Integer'Value(Upload_Result)).Filename);
                                    exit;
                                 end if;
                              end;
                           end;
                        end if;
                     end if;
                  end;
               end if;
            end loop;
         elsif Torrent_Image /= "" then
            Torrent_Meta.Set_Field("image", Torrent_Image);
         else
            Torrent_Meta.Unset_Field("image");
         end if;

         The_Torrent.Set_Meta(Torrent_Meta.Write);
      end;

      Database.Create_Torrent(The_Torrent);

      Database.Set_Torrent_Group(The_Torrent, (if Group /= ""
                                               then Database.Get_Group(Group).Id
                                               else -1));

      return Empty_Holder;
   end Set_Updatable_Fields_And_Create;
begin
   if not Database.User_Exists(Username) then
      return Response.Acknowledge(Messages.S403, "Forbidden");
   end if;

   if Group /= "" and then Database.Get_Group(Group) = Detached_Torrent_Group'Class(No_Detached_Torrent_Group) then
      return Referer_With_Error(Request, "The group """ & Url.Encode(Group) & """ does not exist.");
   elsif Display_Name = "" then
      return Referer_With_Error(Request, "Name cannot be empty");
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

         declare
            Maybe_Error : String_Holders.Holder := Set_Updatable_Fields_And_Create(The_Torrent);
         begin
            if not Maybe_Error.Is_Empty then
               return Referer_With_Error(Request, Maybe_Error.Element);
            end if;
         end;

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

         declare
            Maybe_Error : String_Holders.Holder := Set_Updatable_Fields_And_Create(The_Torrent);
         begin
            if not Maybe_Error.Is_Empty then
               return Referer_With_Error(Request, Maybe_Error.Element);
            end if;
         end;

         Torrent_Subscriptions.Subscribe(Created_By, Detached_Torrent(The_Torrent));

         if Error_String = "" then
            return Response.Url("/view/" & Trim(The_Torrent.Id'Image, Ada.Strings.Left));
         else
            return Response.Url("/view/" & Trim(The_Torrent.Id'Image, Ada.Strings.Left) & "?error=" & To_String(Error_String));
         end if;
      end;
   end;
end Api_Upload_Dispatch;
