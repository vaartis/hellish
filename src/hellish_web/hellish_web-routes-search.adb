with Ada.Calendar.Time_Zones;

with Gnat.Calendar.Time_Io;

with Aws.Utils.Streams;

with 
  Dom.Core,
  Dom.Core.Documents,
  Dom.Core.Elements,
  Dom.Core.Nodes,
  Dom.Core.Attrs;

separate (Hellish_Web.Routes) 
package body Search is
   function Dispatch(Handler : in Search_Handler;
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
         
         declare
            The_User : Detached_User'Class := Database.Get_User(Username);
         begin
            Params.Update(To_Unbounded_String("passkey"), To_Unbounded_String(The_User.Passkey), Decode => False);
            Insert(Translations, Assoc("rss", Status.Uri(Request) & ".rss" & Params.Uri_format));
            
            Userinfo_Translations(The_User, Translations);
         end;         

         return Response.Build(Mime.Text_Html,
                               String'(Templates_Parser.Parse("assets/search.html", Translations)));
      end;
   end Dispatch;
   
   function Dispatch(Handler : in Search_Rss_Handler;
                     Request : in Status.Data) return Response.Data is
      use
        Dom.Core,
        Dom.Core.Documents,
        Dom.Core.Elements,
        Dom.Core.Nodes,
        Dom.Core.Attrs;
      
      Params : Parameters.List := Status.Parameters(Request);
      
      Passkey : String := Params.Get("passkey");
      Query : String := Params.Get("query");
      Uploader : Natural := (if Params.Exist("uploader") then Natural'Value(Params.Get("uploader")) else 0);
      Page : Natural := (if Params.Exist("page") then Integer'Value(Params.Get("page")) else 1);
      Category : Integer := (if Params.Exist("category") then Integer'Value(Params.Get("category")) else -1);
      Snatched_By : Integer := (if Params.Exist("snatched_by") then Integer'Value(Params.Get("snatched_by")) else -1);
      
      Impl : Dom_Implementation;
      Doc : Document := Create_Document(Impl);
   begin      
      if Database.Get_User_By_Passkey(Passkey) = Detached_User'Class(No_Detached_User) then
         return Response.Acknowledge(Messages.S403, "Forbidden");
      end if;
      
      declare 
         package Dc renames Dom.Core;
         
         Full_Host : String := (if Https then "https://" else "http://") & Host_Name;
         
         Rss : Dc.Element := Append_Child(Doc, Create_Element(Doc, "rss"));
         
         Channel : Dc.Element := Append_Child(Rss, Create_Element(Doc, "channel"));
         
         Title : Dc.Element := Append_Child(Channel, Create_Element(Doc, "title"));
         Title_Text : Text := Append_Child(Title, Create_Text_Node(Doc, "Hellish :: Torrents for """ & Query & """"));
         
         Desc : Dc.Element := Append_Child(Channel, Create_Element(Doc, "description"));
         Desc_Text : Text := Append_Child(Desc, Create_Text_Node(Doc, "RSS feed for """ & Query & """"));
         
         Link : Dc.Element := Append_Child(Channel, Create_Element(Doc, "link"));
         Link_Text : Text := Append_Child(Link, Create_Text_Node(Doc, Full_Host));
         
         W3_Atom : String := "http://www.w3.org/2005/Atom";
         Atom_Link : Dc.Element :=
           Append_Child(Channel, Create_Element_Ns(Doc, Namespace_URI => W3_Atom, Qualified_Name => "atom:link"));
      begin
         Set_Attribute(Rss, "version", "2.0");
         Set_Attribute_Ns(Rss, W3_Atom, "xmlns:atom", W3_Atom);
         
         Set_Attribute(Atom_Link, "href", status.Url(Request));
         Set_Attribute(Atom_Link, "rel", "self");
         Set_Attribute(Atom_Link, "type", "application/atom+xml");
         
         declare
            Page_Size, Page_Offset : Natural;
            Page : Integer := Page_Parameters(Params, Page_Size, Page_Offset);
   
            Total_Count : Natural;
            Found_Torrents : Direct_Torrent_List := Database.Search_Torrents(Query, Uploader, Category, Snatched_By,
                                                                             Page_Offset, Page_Size, Total_Count);
            Doc_Stream : aliased Aws.Utils.Streams.Strings;
         begin
            while Found_Torrents.Has_Row loop
               declare
                  use Gnatcoll.Json;
                  
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
                                 Create_Text_Node(Doc, Found_Torrents.Element.Description));
                  
                  Torrent_Meta : Json_Value := Read(Found_Torrents.Element.Meta);
                  Created_At : String := (if Torrent_Meta.Has_Field("created_at")
                                          then Torrent_Meta.Get("created_at")
                                          else "");
               begin
                  Set_Attribute(Guid, "isPermaLink", "true");
                  Set_Attribute(Desc, "type", "text/markdown");
                  
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
            
            Write(Doc_Stream'Access, Doc);
            return Response.Build(Mime.Application_Xml, Doc_Stream.Value);
         end;         
      end;
   end Dispatch;   
end Search;
