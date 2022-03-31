separate (Hellish_Web.Routes)
function Post_Search_Dispatch(Handler : in Post_Search_Handler;
                              Request : in Status.Data) return Response.Data is
   Session_Id : Session.Id := Request_Session(Request);
   Username : String := Session.Get(Session_Id, "username");

   Params : Parameters.List := Status.Parameters(Request);
   Query : String := Params.Get("query");
   Uploader : Natural := (if Params.Exist("uploader") then Natural'Value(Params.Get("uploader")) else 0);
   Page : Natural := (if Params.Exist("page") then Integer'Value(Params.Get("page")) else 1);

   Translations : Translate_Set;
begin
   if not Database.User_Exists(Username) then
      -- Redirect to the login page
      return Response.Url(Location => "/login");
   end if;

   if Query /= "" then
      Insert(Translations, Assoc("query", Query));
   end if;

   declare
      Page_Size : constant Natural := 25;
      Page_Offset : constant Natural := (Page - 1) * Page_Size;
      Total_Count : Natural;
      Found_Posts : Post_List := Database.Search_Posts(Query, Page_Offset, Page_Size, Total_Count);
      -- Round up
      Page_Count : Natural := Natural(Float'Ceiling(Float(Total_Count) / Float(Page_Size)));

      Post_Titles, Post_Ids,
        Post_Authors, Post_Author_Ids,
        Post_Flags, Post_Replies : Vector_Tag;
      Pages, Page_Addresses : Vector_Tag;
   begin
      while Found_Posts.Has_Row loop
         Post_Ids := Post_Ids & Found_Posts.Element.Id;
         Post_Titles := Post_Titles & Found_Posts.Element.Title;
         Post_Authors := Post_Authors & Database.Get_User(Found_Posts.Element.By_User).Username;
         Post_Author_Ids := Post_Author_Ids & Integer'(Found_Posts.Element.By_User);

         Post_Flags := Post_Flags & (case Found_Posts.Element.Flag is
                                        when 1 => "News",
                                        when 2 => "Request / Offer",
                                        when others => "");

         declare
            Total_Replies : Integer;
            Searched_Replies : Post_List := Database.Post_Replies(Found_Posts.Element.Id, 0, 0, Total_Replies);
         begin
            Post_Replies := Post_Replies & Total_Replies;
         end;

         Found_Posts.Next;
      end loop;

      Insert(Translations, Assoc("post_id", Post_Ids));
      Insert(Translations, Assoc("post_title", Post_Titles));
      Insert(Translations, Assoc("post_author", Post_Authors));
      Insert(Translations, Assoc("post_author_id", Post_Author_Ids));
      Insert(Translations, Assoc("post_flag", Post_Flags));
      Insert(Translations, Assoc("post_replies", Post_Replies));

      if Page_Count > 1 then
         for P in 1..Page_Count loop
            if P <= 10 or P = Page_Count then
               if P = Page_Count and Page_Count > 11 then
                  -- Insert a ... before the last page
                  Pages := Pages & "...";
                  Page_Addresses := Page_Addresses & "";
               end if;

               Pages := Pages & P;

               Params.Update(To_Unbounded_String("page"),
                             To_Unbounded_String(Trim(P'Image, Ada.Strings.Left)),
                             Decode => False);
               Page_Addresses := Page_Addresses & String'("/post/search" & Params.Uri_Format);
            end if;
         end loop;

         Insert(Translations, Assoc("page", Pages));
         Insert(Translations, Assoc("page_address", Page_Addresses));
      end if;

      return Response.Build(Mime.Text_Html,
                            String'(Templates_Parser.Parse("assets/post_search.html", Translations)));
   end;
end Post_Search_Dispatch;
