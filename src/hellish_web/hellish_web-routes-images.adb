separate (Hellish_Web.Routes)
package body Images is
   overriding function Dispatch(Handler : in Images_Handler;
                                Request : in Status.Data) return Response.Data is
      Session_Id : Session.Id := Request_Session(Request);
      Username : String := Session.Get(Session_Id, "username");

      Params : constant Parameters.List := Status.Parameters(Request);
      Error : String := Params.Get("error");

      Translations : Translate_Set;
   begin
      if not Database.User_Exists(Username) then
         -- Redirect to the login page
         return Response.Url(Location => "/login");
      end if;

      declare
         User_Images : Image_Upload_List := Database.User_Images(Username);

         Image_Names, Image_Ids : Vector_Tag;
      begin
         while User_Images.Has_Row loop
            Image_Names := @ & User_Images.Element.Filename;
            Image_Ids := @ & User_Images.Element.Id;

            User_Images.Next;
         end loop;

         Insert(Translations, Assoc("image_name", Image_Names));
         Insert(Translations, Assoc("image_id", Image_Ids));
      end;
      if Error /= "" then
         Insert(Translations, Assoc("error", Error));
      end if;

      return Response.Build(Mime.Text_Html,
                            String'(Templates_Parser.Parse("assets/images.html", Translations)));
   end Dispatch;

   -- API

   function Dispatch
     (Handler : in Api_Image_Upload_Handler;
      Request : in Status.Data) return Response.Data is
      use Ada.Directories;

      Params : constant Parameters.List := Status.Parameters(Request);

      Session_Id : Session.Id := Request_Session(Request);
      Username : String := Session.Get(Session_Id, "username");

      File_Path : String := Params.Get("file");

      -- Approx. 1MB
      Max_Size : constant Natural := 1024 * 1024;

      Mime_Type : String := Mime.Content_Type(File_Path);
   begin
      if not Database.User_Exists(Username) then
         return Response.Acknowledge(Messages.S403, "Forbidden");
      end if;

      if Size(File_Path) > File_Size(Max_Size) then
         return Response.Url(Location => "/images?error=Image too big (max size is "
                               & Bytes_To_Printable(Long_Long_Integer(Max_Size)) & ")");
      end if;
      if not MIME.Is_Image(Mime_Type) then
         return Response.Url(Location => "/images?error=File is not an image, but " & Mime.Content_Type(Mime_Type));
      end if;

      Create_Path(Image_Uploads_Path);

      declare
         File : File_Type;
         File_Stream : Stream_Access;
         Content : Unbounded_String;

         New_Name : Unbounded_String;
         Upload_Path : Unbounded_String;
      begin
         Open(File, Mode => In_File, Name => File_Path);
         File_Stream := Stream(File);
         while not End_Of_File(File) loop
            Content := @ & Character'Input(File_Stream);
         end loop;
         Close(File);

         declare
            Sha1_Digest : String := Gnat.Sha1.Digest(To_String(Content));
            New_Name : String :=  Compose(Name => Sha1_Digest, Extension => Ada.Directories.Extension(File_Path));
            Upload_Path : String := Compose(Containing_Directory => Image_Uploads_Path, Name => New_Name);

            The_Image : Detached_Image_Upload'Class := Database.Add_Uploaded_Image(Username, New_Name);
         begin
            -- No need to copy it again..
            if not Exists(Upload_Path) then
               Copy_File(File_Path, Upload_Path);
            end if;

            return Response.Url(Location => "/images#image-" & Trim(The_Image.Id'Image, Ada.Strings.Left));
         end;
      end;
   end Dispatch;

   Api_Image_Delete_Matcher : constant Pattern_Matcher := Compile("/api/delete/image/(\d+)");
   function Dispatch
     (Handler : in Api_Image_Delete_Handler;
      Request : in Status.Data) return Response.Data is
      Params : constant Parameters.List := Status.Parameters(Request);

      Session_Id : Session.Id := Request_Session(Request);
      Username : String := Session.Get(Session_Id, "username");
   begin
      if not Database.User_Exists(Username) then
         return Response.Acknowledge(Messages.S403, "Forbidden");
      end if;

      declare
         use Ada.Directories;

         Id : Natural := Natural'Value(Uri_Group_Match(Request, Api_Image_Delete_Matcher, 1));
         The_Image : Detached_Image_Upload'Class := Database.Get_Image(Id);
         The_User : Detached_User'class := Database.Get_User(Username);
      begin
         if The_Image.By_User /= The_User.Id and The_User.Role /= 1 then
            return Response.Acknowledge(Messages.S403, "Forbidden");
         end if;

         if not Database.Delete_Uploaded_Image(Id) then
            Delete_File(Compose(Containing_Directory => Image_Uploads_Path, Name => The_Image.Filename));
         end if;

         return Response.Url(Location => "/images");
      end;
   end Dispatch;
end Images;
