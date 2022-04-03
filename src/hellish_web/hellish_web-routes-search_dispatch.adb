separate (Hellish_Web.Routes) 
function Search_Dispatch(Handler : in Search_Handler;
                         Request : in Status.Data) return Response.Data is
   Session_Id : Session.Id := Request_Session(Request);
   Username : String := Session.Get(Session_Id, "username");
   
   Params : Parameters.List := Status.Parameters(Request);
   Query : String := Params.Get("query");
   Uploader : Natural := (if Params.Exist("uploader") then Natural'Value(Params.Get("uploader")) else 0);
   Page : Natural := (if Params.Exist("page") then Integer'Value(Params.Get("page")) else 1);
   Category : Integer := (if Params.Exist("category") then Integer'Value(Params.Get("category")) else -1);

   Translations : Translate_Set;
begin
   if not Database.User_Exists(Username) then
      -- Redirect to the login page
      return Response.Url(Location => "/login");
   end if;

   if Query /= "" then
      Insert(Translations, Assoc("query", Query));
   end if;
   if Uploader /= 0 then
      declare
         Uploader_User : Detached_User'Class := Database.Get_User(Uploader);
      begin
         if Detached_User(Uploader_User) /= No_Detached_User then
            Insert(Translations, Assoc("query_uploader", Uploader_User.Username));
         end if;
      end;
   end if;

   declare
      Page_Size : constant Natural := 25;
      Page_Offset : constant Natural := (Page - 1) * Page_Size;
      Total_Count : Natural;
      Found_Torrents : Torrent_List := Database.Search_Torrents(Query, Uploader, Category, 
                                                                Page_Offset, Page_Size, Total_Count);
      -- Round up
      Page_Count : Natural := Natural(Float'Ceiling(Float(Total_Count) / Float(Page_Size)));

      Torrent_Names, Torrent_Ids,
        Torrent_Uploaders, Torrent_Uploader_Ids, Torrent_Comments,
        The_Torrent_Categories: Vector_Tag;
      Pages, Page_Addresses : Vector_Tag;
   begin
      while Found_Torrents.Has_Row loop
         Torrent_Ids := @ & Found_Torrents.Element.Id;
         Torrent_Names := @ & Found_Torrents.Element.Display_Name;
         Torrent_Uploaders := @ & Database.Get_User(Found_Torrents.Element.Created_By).Username;
         Torrent_Uploader_Ids := @ & Integer'(Found_Torrents.Element.Created_By);

         declare
            Total_Comments : Integer;
            Searched_Replies : Post_List := Database.Torrent_Comments(Found_Torrents.Element.Id, 0, 0, Total_Comments);
         begin
            Torrent_Comments := @ & Total_Comments;
         end;

         The_Torrent_Categories := @ & Torrent_Categories(Found_Torrents.Element.Category);

         Found_Torrents.Next;
      end loop;

      Insert(Translations, Assoc("torrent_id", Torrent_Ids));
      Insert(Translations, Assoc("torrent_name", Torrent_Names));
      Insert(Translations, Assoc("torrent_uploader", Torrent_Uploaders));
      Insert(Translations, Assoc("torrent_uploader_id", Torrent_Uploader_Ids));
      Insert(Translations, Assoc("torrent_comments", Torrent_Comments));
      Insert(Translations, Assoc("torrent_category", The_Torrent_Categories));

      if Page_Count > 1 then
         for P in 1..Page_Count loop
            if P <= 10 or P = Page_Count then
               if P = Page_Count and Page_Count > 11 then
                  -- Insert a ... before the last page
                  Pages := @ & "...";
                  Page_Addresses := @ & "";
               end if;

               Pages := @ & P;

               Params.Update(To_Unbounded_String("page"),
                             To_Unbounded_String(Trim(P'Image, Ada.Strings.Left)),
                             Decode => False);
               Page_Addresses := @ & String'("/search" & Params.Uri_Format);
            end if;
         end loop;

         Insert(Translations, Assoc("page", Pages));
         Insert(Translations, Assoc("page_address", Page_Addresses));
      end if;

      declare
         Category_Names, Category_Values : Vector_Tag;
      begin
         Insert(Translations, Assoc("search_category", Category));
         for Category_Cursor in Torrent_Categories.Iterate loop
            Category_Values := @ & Key(Category_Cursor);
            Category_Names := @ & Element(Category_Cursor);
         end loop;
         Insert(Translations, Assoc("category_name", Category_Names));
         Insert(Translations, Assoc("category_value", Category_Values));
      end;

      return Response.Build(Mime.Text_Html,
                            String'(Templates_Parser.Parse("assets/search.html", Translations)));
   end;
end Search_Dispatch;
