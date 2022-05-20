separate (Hellish_Web.Routes)
package body Groups is
   overriding function Dispatch(Handler : in Group_Create_Handler;
                                Request : in Status.Data) return Response.Data is
      Session_Id : Session.Id := Request_Session(Request);
      Username : String := Session.Get(Session_Id, "username");

      Translations : Translate_Set;

      The_User : Detached_User'Class := No_Detached_User;

      Params : Parameters.List := Status.Parameters(Request);
      Update : Integer := (if Params.Exist("update")
                           then Integer'Value(Params.Get("update"))
                           else -1);
      Error : String := Params.Get("error");
   begin
      if not Database.User_Exists(Username) then
         -- Redirect to the login page
         return Response.Url(Location => "/login");
      end if;
      The_User := Database.Get_User(Username);

      if Update /= -1 then
         declare
            Updated_Group : Detached_Torrent_Group'Class := Database.Get_Group(Update);
         begin
            Insert(Translations, Assoc("update", Update));
            Insert(Translations, Assoc("update_name", Updated_Group.Name));
            Insert(Translations, Assoc("update_desc", Updated_Group.Description));
         end;
      else
         Insert(Translations, Assoc("update_desc", ""));
      end if;

      if Error /= "" then
         Insert(Translations, Assoc("error", Error));
      end if;

      Userinfo_Translations(The_User, Translations);
      return Response.Build(Mime.Text_Html,
                            String'(Templates_Parser.Parse("assets/group_create.html", Translations)));
   end Dispatch;

   Group_Matcher : constant Pattern_Matcher := Compile("/group/(\d+)");
   overriding function Dispatch(Handler : in Group_Handler;
                                Request : in Status.Data) return Response.Data is
      Session_Id : Session.Id := Request_Session(Request);
      Username : String := Session.Get(Session_Id, "username");

      Params : Parameters.List := Status.Parameters(Request);

      Translations : Translate_Set;

      The_User : Detached_User'Class := No_Detached_User;
   begin
      if not Database.User_Exists(Username) then
         -- Redirect to the login page
         return Response.Url(Location => "/login");
      end if;
      The_User := Database.Get_User(Username);

      declare
         Id : Natural := Natural'Value(Uri_Group_Match(Request, Group_Matcher, 1));
         The_Group : Detached_Torrent_Group'Class := Database.Get_Group(Id);
      begin
         Insert(Translations, Assoc("id", The_Group.Id));
         Insert(Translations, Assoc("name", Templates_Parser.Utils.Web_Escape(The_Group.Name)));
         Insert(Translations, Assoc("description", Process_Content(The_Group.Description)));

         declare
            Torrents_In_Group : Direct_Torrent_List := Database.Get_Group_Torrents(Id);
         begin
            Torrent_Table_Translations(Torrents_In_Group, Translations, The_User);
         end;
      end;

      Params.Update(To_Unbounded_String("passkey"), To_Unbounded_String(The_User.Passkey), Decode => False);
      Insert(Translations, Assoc("rss", Status.Uri(Request) & ".rss" & Params.Uri_format));

      Userinfo_Translations(The_User, Translations);
      return Response.Build(Mime.Text_Html,
                            String'(Templates_Parser.Parse("assets/group.html", Translations)));
   end Dispatch;

   Group_Rss_Matcher : constant Pattern_Matcher := Compile("/group/(\d+).rss");
   overriding function Dispatch(Handler : in Group_Rss_Handler;
                                Request : in Status.Data) return Response.Data is
      Params : Parameters.List := Status.Parameters(Request);

      Passkey : String := Params.Get("passkey");
   begin
      if Database.Get_User_By_Passkey(Passkey) = Detached_User'Class(No_Detached_User) then
         return Response.Acknowledge(Messages.S403, "Forbidden");
      end if;

      declare
         use
           Dom.Core,
           Dom.Core.Documents,
           Dom.Core.Elements,
           Dom.Core.Nodes,
           Dom.Core.Attrs;
         package Dc renames Dom.Core;

         Id : Natural := Natural'Value(Uri_Group_Match(Request, Group_Matcher, 1));
         The_Group : Detached_Torrent_Group'Class := Database.Get_Group(Id);

         Channel : Dc.Element;
         Doc : Document := Rss_Feed_Base(Title => "Hellish :: Torrents from the """ & The_Group.Name & """ group",
                                         Description => "RSS feed for the """ & The_Group.Name & """ group",
                                         Page_Link => Status.Url(Request),
                                         Channel => Channel,
                                         Correlating_Page => "/group/" & Trim(The_Group.Id'Image, Ada.Strings.Left));

         Doc_Stream : aliased Aws.Utils.Streams.Strings;

         Torrents_In_Group : Direct_Torrent_List := Database.Get_Group_Torrents(Id);
      begin
         Rss_Feed_Torrent_List (Channel => Channel, Found_Torrents => Torrents_In_Group, Passkey => Passkey);

         Write(Doc_Stream'Access, Doc);
         return Response.Build(Mime.Application_Xml, Doc_Stream.Value);
      end;
   end Dispatch;


   overriding function Dispatch(Handler : in Group_Search_Handler;
                                Request : in Status.Data) return Response.Data is
      Session_Id : Session.Id := Request_Session(Request);
      Username : String := Session.Get(Session_Id, "username");

      Params : Parameters.List := Status.Parameters(Request);
      Query : String := Params.Get("query");

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
         use Gnatcoll.Sql.Exec;

         Page_Size, Page_Offset : Natural;
         Page : Integer := Page_Parameters(Params, Page_Size, Page_Offset);

         Total_Count : Natural;
         Found_Groups : Torrent_Group_List := Database.Search_Torrent_Groups(Query, Page_Offset, Page_Size, Total_Count);

         Group_Ids, Group_Names, Group_Creators, Group_N_Torrents : Vector_Tag;
      begin
         Page_Translations(Request, Total_Count, Translations);

         while Found_Groups.Has_Row loop
            Group_Ids := @ & Found_Groups.Element.Id;
            Group_Names := @ & Found_Groups.Element.Name;
            Group_Creators := @ & Database.Get_User(Found_Groups.Element.Creator).Username;
            Group_N_Torrents := @ & Rows_Count(Direct_Cursor(Database.Get_Group_Torrents(Found_Groups.Element.Id)));

            Found_Groups.Next;
         end loop;
         Insert(Translations, Assoc("group_id", Group_Ids));
         Insert(Translations, Assoc("group_name", Group_Names));
         Insert(Translations, Assoc("group_creator", Group_Creators));
         Insert(Translations, Assoc("group_n_torrents", Group_N_Torrents));

         Userinfo_Translations(Database.Get_User(Username), Translations);

         return Response.Build(Mime.Text_Html,
                               String'(Templates_Parser.Parse("assets/group_search.html", Translations)));
      end;
   end Dispatch;

   -- API

   overriding function Dispatch(Handler : in Api_Group_Create_Handler;
                                Request : in Status.Data) return Response.Data is
      Session_Id : Session.Id := Request_Session(Request);
      Username : String := Session.Get(Session_Id, "username");

      Params : Parameters.List := Status.Parameters(Request);
      Update : Integer := (if Params.Exist("update")
                           then Integer'Value(Params.Get("update"))
                           else -1);
      Name : String := Trim(Params.Get("name"), Ada.Strings.Both);
      Description : String := Params.Get("description");

      The_User : Detached_User'Class := No_Detached_User;
      The_Group : Detached_Torrent_Group'Class := No_Detached_Torrent_Group;

      Maybe_Existing_Name : Detached_Torrent_Group'Class := Database.Get_Group(Name);
   begin
      if not Database.User_Exists(Username) then
         return Response.Acknowledge(Messages.S403, "Forbidden");
      end if;
      The_User := Database.Get_User(Username);


      if (Update = -1 and Maybe_Existing_Name /= Detached_Torrent_Group'Class(No_Detached_Torrent_Group))
        or else (Update /= -1 and then Maybe_Existing_Name /= Detached_Torrent_Group'Class(No_Detached_Torrent_Group)
                   and then Maybe_Existing_Name.Id /= Update) then
         return Referer_With_Error(Request, "A group with this name already exists");
      elsif Name = "" then
         return Referer_With_Error(Request, "A group can't have an empty name");
      end if;

      if Update /= -1 then
         The_Group := Database.Get_Group(Update);

         if Detached_Torrent_Group(The_Group) = No_Detached_Torrent_Group then
            return Response.Acknowledge(Messages.S403, "No such group");
         end if;
      else
         The_Group := New_Torrent_Group;

         The_Group.Set_Creator(The_User.Id);
      end if;

      Put_Line(Description);

      The_Group.Set_Name(Name);
      The_Group.Set_Description(Description);
      Database.Create_Group(The_Group);

      return Response.Url(Location => "/group/" & Trim(The_Group.Id'Image, Ada.Strings.Left));
   end Dispatch;
end Groups;
