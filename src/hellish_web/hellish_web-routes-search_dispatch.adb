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
   Snatched_By : Integer := (if Params.Exist("snatched_by") then Integer'Value(Params.Get("snatched_by")) else -1);

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
   if Snatched_By /= -1 then
      declare
         Snatched_User : Detached_User'Class := Database.Get_User(Snatched_By);
      begin
         if Detached_User(Snatched_User) /= No_Detached_User then
            Insert(Translations, Assoc("query_snatched", Snatched_User.Username));
         end if;
      end;      
   end if;

   declare
      Page_Size, Page_Offset : Natural;
      Page : Integer := Page_Parameters(Params, Page_Size, Page_Offset);

      Total_Count : Natural;
      Found_Torrents : Direct_Torrent_List := Database.Search_Torrents(Query, Uploader, Category, Snatched_By,
                                                                       Page_Offset, Page_Size, Total_Count);
   begin
      Torrent_Table_Translations(Found_Torrents, Translations);
      
      Page_Translations(Request, Total_Count, Translations);

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

      Userinfo_Translations(Database.Get_User(Username), Translations);

      return Response.Build(Mime.Text_Html,
                            String'(Templates_Parser.Parse("assets/search.html", Translations)));
   end;
end Search_Dispatch;
