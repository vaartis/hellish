separate (Hellish_Web.Routes)
package body Posts is
   Post_Id_Matcher : constant Pattern_Matcher := Compile("/post/(\d+)");
   function Dispatch
     (Handler : in Post_Handler;
      Request : in Status.Data) return Response.Data is

      Session_Id : Session.Id := Request_Session(Request);
      Username : String := Session.Get(Session_Id, "username");

      Page_Size : constant Natural := 25;
   begin
      if not Database.User_Exists(Username) then
         -- Redirect to the login page
         return Response.Url(Location => "/login");
      end if;

      declare
         Id : Natural := Natural'Value(Uri_Group_Match(Request, Post_Id_Matcher, 1));

         Parent_Post : Detached_Post'Class := No_Detached_Post;
         Post : Detached_Post'Class := Database.Get_Post(Id, Parent_Post);
         Author : Detached_User'Class := Database.Get_User(Post.By_User);

         Parent_Torrent : Detached_Torrent'Class := Database.Get_Torrent(Post.Parent_Torrent);

         Translations : Translate_Set;
      begin
         if Parent_Post /= Detached_Post'Class(No_Detached_Post) then
            declare
               Total_Searched : Integer;
               -- -1 means all
               Searched_Replies : Post_List := Database.Post_Replies(Parent_Post.Id, 0, -1, Total_Searched);
               Searched_N : Natural := 0;

               Found_Page : Natural := 1;
            begin
               while Searched_Replies.Has_Row loop
                  Searched_N := Searched_N + 1;

                  if Searched_Replies.Element.Id = Post.Id then
                     Found_Page := Natural(Float'Ceiling(Float(Searched_N) / Float(Page_Size)));
                  end if;

                  Searched_Replies.Next;
               end loop;

               return Response.Url("/post/"
                                     & Trim(Parent_Post.Id'Image, Ada.Strings.Left)
                                     & "?page=" & Trim(Found_Page'Image, Ada.Strings.Left)
                                     & "#child-" & Trim(Post.Id'Image, Ada.Strings.Left));
            end;
         elsif Parent_Torrent /= Detached_Torrent'Class(No_Detached_Torrent) then
            declare
               Total_Searched : Integer;
               -- -1 means all
               Searched_Replies : Post_List := Database.Torrent_Comments(Parent_Torrent.Id, 0, -1, Total_Searched);
               Searched_N : Natural := 0;

               Found_Page : Natural := 1;
            begin
               while Searched_Replies.Has_Row loop
                  Searched_N := Searched_N + 1;

                  if Searched_Replies.Element.Id = Post.Id then
                     Found_Page := Natural(Float'Ceiling(Float(Searched_N) / Float(Page_Size)));
                  end if;

                  Searched_Replies.Next;
               end loop;

               return Response.Url("/view/"
                                     & Trim(Parent_Torrent.Id'Image, Ada.Strings.Left)
                                     & "?page=" & Trim(Found_Page'Image, Ada.Strings.Left)
                                     & "#child-" & Trim(Post.Id'Image, Ada.Strings.Left));
            end;
         end if;
         declare
            Html_Title : String := Templates_Parser.Utils.Web_Escape(Post.Title);
            -- This both translates markdown to html and escapes whatever html the text might've had,
            -- so should be safe
            Post_Content : String := Markdown.To_Html(Post.Content, Default_Md_Flags);

            The_User : Detached_User'Class := Database.Get_User(Username);
         begin
            Insert(Translations, Assoc("id", Post.Id));
            Insert(Translations, Assoc("title", Html_Title));
            Insert(Translations, Assoc("content", Post_Content));
            Insert(Translations, Assoc("author", Author.Username));
            Insert(Translations, Assoc("is_author", Author.Id = The_User.Id or The_User.Role = 1));

            Replies_Translations(Post.Id, The_User, Translations, Database.Post_Replies'Access, Request);

            return Response.Build(Mime.Text_Html,
                                  String'(Templates_Parser.Parse("assets/post.html", Translations)));
         end;
      end;
   end Dispatch;

   overriding function Dispatch(Handler : in Post_Create_Handler;
                                Request : in Status.Data) return Response.Data is
      Session_Id : Session.Id := Request_Session(Request);
      Username : String := Session.Get(Session_Id, "username");

      Translations : Translate_Set;
      The_User : Detached_User'Class := No_Detached_user;

      Params : Parameters.List := Status.Parameters(Request);
      Update : Natural := (if Params.Exist("update") then Natural'Value(Params.Get("update")) else 0);
   begin
      if not Database.User_Exists(Username) then
         -- Redirect to the login page
         return Response.Url(Location => "/login");
      end if;

      The_User := Database.Get_User(Username);
      Insert(Translations, Assoc("admin", The_User.Role = 1));

      Insert(Translations, Assoc("reply", False));
      Insert(Translations, Assoc("content", ""));
      if Update /= 0 then
         declare
            Parent_Post : Detached_Post'Class := No_Detached_Post;
            The_Post : Detached_Post'Class := Database.Get_Post(Update, Parent_Post);
            Parent_Torrent : Detached_Torrent'Class := Database.Get_Torrent(The_Post.Parent_Torrent);
         begin
            if Parent_Post /= Detached_Post'Class(No_Detached_Post)
              or Parent_Torrent /= Detached_Torrent'Class(No_Detached_Torrent)  then
               Insert(Translations, Assoc("reply", True));
            end if;
            Insert(Translations, Assoc("update", Update));
            Insert(Translations, Assoc("title", The_Post.Title));
            Insert(Translations, Assoc("content", The_Post.Content));
            Insert(Translations, Assoc("flag", The_Post.Flag));
         end;
      end if;

      return Response.Build(Mime.Text_Html,
                               String'(Templates_Parser.Parse("assets/post_create.html", Translations)));
   end Dispatch;

   overriding function Dispatch(Handler : in Post_Search_Handler;
                                Request : in Status.Data) return Response.Data is
      Session_Id : Session.Id := Request_Session(Request);
      Username : String := Session.Get(Session_Id, "username");

      Params : Parameters.List := Status.Parameters(Request);
      Query : String := Params.Get("query");
      Author : Natural := (if Params.Exist("author") then Natural'Value(Params.Get("author")) else 0);
      Page : Natural := (if Params.Exist("page") then Integer'Value(Params.Get("page")) else 1);
      Flag : Integer := (if Params.Exist("flag") then Integer'Value(Params.Get("flag")) else -1);

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
         Found_Posts : Post_List := Database.Search_Posts(Query, Flag, Author,
                                                          Page_Offset, Page_Size, Total_Count);
         -- Round up
         Page_Count : Natural := Natural(Float'Ceiling(Float(Total_Count) / Float(Page_Size)));

         Post_Titles, Post_Ids, Post_Authors,
           The_Post_Flags, Post_Replies : Vector_Tag;
         Pages, Page_Addresses : Vector_Tag;
      begin
         while Found_Posts.Has_Row loop
            Post_Ids := @ & Found_Posts.Element.Id;
            Post_Titles := @ & Found_Posts.Element.Title;
            Post_Authors := @ & Database.Get_User(Found_Posts.Element.By_User).Username;

            The_Post_Flags := @ & (if Post_Flags.Contains(Found_Posts.Element.Flag)
                                   then Post_Flags(Found_Posts.Element.Flag)
                                   else "");

            declare
               Total_Replies : Integer;
               Searched_Replies : Post_List := Database.Post_Replies(Found_Posts.Element.Id, 0, 0, Total_Replies);
            begin
               Post_Replies := @ & Total_Replies;
            end;

            Found_Posts.Next;
         end loop;

         Insert(Translations, Assoc("post_id", Post_Ids));
         Insert(Translations, Assoc("post_title", Post_Titles));
         Insert(Translations, Assoc("post_author", Post_Authors));
         Insert(Translations, Assoc("post_flag", The_Post_Flags));
         Insert(Translations, Assoc("post_replies", Post_Replies));

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
                  Page_Addresses := @ & String'("/post/search" & Params.Uri_Format);
               end if;
            end loop;

            Insert(Translations, Assoc("page", Pages));
            Insert(Translations, Assoc("page_address", Page_Addresses));

         end if;

         declare
            Flag_Names, Flag_Values : Vector_Tag;
         begin
            Insert(Translations, Assoc("search_flag", Flag));
            for Flag_Cursor in Post_Flags.Iterate loop
               Flag_Values := @ & Key(Flag_Cursor);
               Flag_Names := @ & Element(Flag_Cursor);
            end loop;
            Insert(Translations, Assoc("flag_name", Flag_Names));
            Insert(Translations, Assoc("flag_value", Flag_Values));
         end;

         if Author /= 0 then
            declare
               Author_User : Detached_User'Class := Database.Get_User(Author);
            begin
               if Author_User /= Detached_User'Class(No_Detached_User) then
                  Insert(Translations, Assoc("query_author", Author_User.Username));
               end if;
            end;
   end if;

         return Response.Build(Mime.Text_Html,
                               String'(Templates_Parser.Parse("assets/post_search.html", Translations)));
      end;
   end Dispatch;

   -- API

   overriding function Dispatch(Handler : in Api_Post_Create_Handler;
                                Request : in Status.Data) return Response.Data is
      Session_Id : Session.Id := Request_Session(Request);
      Username : String := Session.Get(Session_Id, "username");
      The_User : Detached_User'Class := No_Detached_User;

      Params : constant Parameters.List := Status.Parameters(Request);
      Title : String := Params.Get("title");
      Content : String := Params.Get("content");
      Parent : Integer := (if Params.Exist("parent") then Natural'Value(Params.Get("parent")) else -1);
      Parent_Torrent : Integer := (if Params.Exist("parent_torrent")
                                   then Natural'Value(Params.Get("parent_torrent"))
                                   else -1);
      Flag : Integer := (if Params.Exist("flag") then Natural'Value(Params.Get("flag")) else 0);

      Update : Integer := (if Params.Exist("update") then Natural'Value(Params.Get("update")) else -1);
      Updated_Post : Detached_Post'Class := No_Detached_Post;

      Parent_Post : Detached_Post'Class := No_Detached_Post;
      Post : Detached_Post'Class := New_Post;
   begin
      if not Database.User_Exists(Username) then
         return Response.Acknowledge(Messages.S403, "Forbidden");
      end if;
      The_User := Database.Get_User(Username);

      if Update /= -1 then
         Updated_Post := Database.Get_Post(Update, Parent_Post);
         if Integer'(Updated_Post.By_User) /= The_User.Id and The_User.Role /= 1 then
            return Response.Acknowledge(Messages.S403, "Forbidden");
         end if;
         Post := Updated_Post;
      end if;

      Post.Set_By_User(The_User.Id);
      Post.Set_Content(Content);

      if Title /= "" then
         Post.Set_Title(Title);
      end if;
      if Parent /= -1 then
         Post.Set_Parent_Post(Parent);
      elsif Parent_Torrent /= -1
        and Database.Get_Torrent(Parent_Torrent) /= Detached_Torrent'Class(No_Detached_Torrent) then
         Post.Set_Parent_Torrent(Parent_Torrent);
      end if;
      if Parent = -1 and Parent_Torrent = -1 then
         if (Flag = 1 and The_User.Role = 1) or Flag = 2 then
            Post.Set_Flag(Flag);
         end if;
      end if;

      Database.Create_Post(Post);

      return Response.Url("/post/" & Trim(Post.Id'Image, Ada.Strings.Left));
   end Dispatch;
end Posts;
