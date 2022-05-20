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

            The_User : Detached_User'Class := Database.Get_User(Username);

            use Gnatcoll.Json;
            Post_Meta : Json_Value := Read(Post.Meta);
            Created_At : String := (if Post_Meta.Has_Field("created_at")
                                   then Post_Meta.Get("created_at")
                                   else "");
         begin
            Insert(Translations, Assoc("id", Post.Id));
            Insert(Translations, Assoc("title", Html_Title));
            Insert(Translations, Assoc("content", Process_Content(Post.Content)));
            Insert(Translations, Assoc("author", Author.Username));
            Insert(Translations, Assoc("is_author", Author.Id = The_User.Id or The_User.Role = 1));
            Insert(Translations, Assoc("is_subscribed", Post_Subscriptions.Subscribed(The_User, Detached_Post(Post))));

            Insert(Translations, Assoc("created_at", Created_At));

            Replies_Translations(Post.Id, The_User, Translations, Database.Post_Replies'Access, Request);

            Userinfo_Translations(The_User, Translations);

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

      Userinfo_Translations(The_User, Translations);

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
         Page_Size, Page_Offset : Natural;
         Page : Integer := Page_Parameters(Params, Page_Size, Page_Offset);

         Total_Count : Natural;
         Found_Posts : Post_List := Database.Search_Posts(Query, Flag, Author,
                                                          Page_Offset, Page_Size, Total_Count);

         Post_Titles, Post_Ids, Post_Authors,
           The_Post_Flags, Post_Replies : Vector_Tag;
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

         Page_Translations(Request, Total_Count, Translations);

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

         Userinfo_Translations(Database.Get_User(Username), Translations);

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

      Parent_Post : Detached_Post'Class := No_Detached_Post;
      Post : Detached_Post'Class := New_Post;

      The_Parent_Torrent : Detached_Torrent'Class := No_Detached_Torrent;
   begin
      if not Database.User_Exists(Username) then
         return Response.Acknowledge(Messages.S403, "Forbidden");
      end if;
      The_User := Database.Get_User(Username);

      declare
         use Gnatcoll.Json;
         use Ada.Calendar, Ada.Calendar.Formatting;

         Post_Meta : Json_Value;
      begin
         if Update /= -1 then
            Post := Database.Get_Post(Update, Parent_Post);
            if Integer'(Post.By_User) /= The_User.Id and The_User.Role /= 1 then
               return Response.Acknowledge(Messages.S403, "Forbidden");
            end if;

            Post_Meta := Read(Post.Meta);
         else
            Post_Meta := Read(Post.Meta);

            -- Set date to right now
            Post_Meta.Set_Field("created_at", Image(Clock));

            Post.Set_By_User(The_User.Id);
         end if;

         declare
            Previously_Mentioned : Json_Array := (if Has_Field(Post_Meta, "mentioned_users")
                                                  then Get(Post_Meta, "mentioned_users")
                                                  else Empty_Array);

            Mentioned_Match : Match_Array(0..0);
            Current : Natural := Content'First;
         begin
            loop
               Match(User_Mention_Matcher, Content, Mentioned_Match, Current);
               exit when Mentioned_Match(0) = No_Match;

               declare
                  Match_Name : String := Content(Mentioned_Match(0).First + 1..Mentioned_Match(0).Last);
                  Mentioned_User : Detached_User'Class := Database.Get_User(Match_Name);
               begin
                  -- Skip if the user does not exist
                  if Mentioned_User = Detached_User'Class(No_Detached_User) then goto Skip; end if;

                  for Previous of Previously_Mentioned loop
                     -- Skip if the user was already mentioned previously
                     if Equal_Case_Insensitive(Get(Previous), Match_Name) then goto Skip; end if;
                  end loop;
                  Append(Previously_Mentioned, Create(Match_Name));

                  declare
                     Author : Detached_User'Class := Database.Get_User(Post.By_User);
                  begin
                     Database.Notify_User(Mentioned_User, "You have been [mentioned](/post/" & Trim(Post.Id'Image, Ada.Strings.Left)
                                            & ") by [" & Author.Username & "](/profile/" & Author.Username & ")");
                  end;
                  <<Skip>>
               end;

               Current := Mentioned_Match(0).Last + 1;
            end loop;

            if not Is_Empty(Previously_Mentioned) then
               Post_Meta.Set_Field("mentioned_users", Previously_Mentioned);
            end if;
         end;

         Post.Set_Meta(Write(Post_Meta));
      end;

      Post.Set_Content(Content);

      if Title /= "" then
         Post.Set_Title(Title);
      end if;
      if Parent /= -1 then
         Parent_Post := Database.Get_Post(Parent);
         if Parent_Post /= Detached_Post'Class(No_Detached_Post) then
            Post.Set_Parent_Post(Parent);
         end if;
      elsif Parent_Torrent /= -1 then
         The_Parent_Torrent := Database.Get_Torrent(Parent_Torrent);
         if The_Parent_Torrent /= Detached_Torrent'Class(No_Detached_Torrent) then
            Post.Set_Parent_Torrent(Parent_Torrent);
         end if;
      end if;
      if Parent = -1 and Parent_Torrent = -1 then
         if (Flag = 1 and The_User.Role = 1) or Flag = 2 then
            Post.Set_Flag(Flag);
         end if;
      end if;

      Database.Create_Post(Post);

      if Update = -1 then
         if Parent /= -1 then
            if not Post_Subscriptions.Explicitly_Unsubscribed(The_User, Detached_Post(Parent_Post)) then
               Post_Subscriptions.Subscribe(The_User, Detached_Post(Parent_Post));
            end if;
         elsif Parent_Torrent /= -1 then
            if not Torrent_Subscriptions.Explicitly_Unsubscribed(The_User, Detached_Torrent(The_Parent_Torrent)) then
               Torrent_Subscriptions.Subscribe(The_User, Detached_Torrent(The_Parent_Torrent));
            end if;
         else
            Post_Subscriptions.Subscribe(The_User, Detached_Post(Post));
         end if;

         if Parent /= -1 then
            Post_Subscriptions.Notify(The_User,
                             Detached_Post(Parent_Post),
                             "There's [a new reply to the post """ & Parent_Post.Title & """](/post/" & Trim(Post.Id'Image, Ada.Strings.Left) & ")");
         elsif Parent_Torrent /= -1 then
            Torrent_Subscriptions.Notify(The_User,
                                Detached_Torrent(The_Parent_Torrent),
                                "There's [a new comment on the torrent """ & The_Parent_Torrent.Display_Name & """](/post/" &
                                  Trim(Post.Id'Image, Ada.Strings.Left) & ")");
         end if;
      end if;

      return Response.Url("/post/" & Trim(Post.Id'Image, Ada.Strings.Left));
   end Dispatch;
end Posts;
