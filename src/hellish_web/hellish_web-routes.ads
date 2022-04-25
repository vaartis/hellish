with AWS.Dispatchers;
with AWS.Response;
with AWS.Status;
with Aws.Config;
with Aws.Server;
with Aws.Services.Dispatchers.URI;

with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Hellish_Web.Routes is
   use Aws;

   procedure Run_Server;
   procedure Shutdown_Server;

   function Host_Name return String;
private
   package Posts is
      type Post_Handler is new Dispatchers.Handler with null record;
      type Post_Create_Handler is new Dispatchers.Handler with null record;
      type Post_Search_Handler is new Dispatchers.Handler with null record;

      overriding function Dispatch(Handler : in Post_Handler; Request : in Status.Data) return Response.Data;
      overriding function Dispatch(Handler : in Post_Create_Handler; Request : in Status.Data) return Response.Data;
      overriding function Dispatch(Handler : in Post_Search_Handler; Request : in Status.Data) return Response.Data;

      overriding function Clone(Element : in Post_Handler) return Post_Handler is (Element);
      overriding function Clone(Element : in Post_Create_Handler) return Post_Create_Handler is (Element);
      overriding function Clone(Element : in Post_Search_Handler) return Post_Search_Handler is (Element);

      Post : Post_Handler;
      Post_Create : Post_Create_Handler;
      Post_Search : Post_Search_Handler;

      -- API

      type Api_Post_Create_Handler is new Dispatchers.Handler with null record;

      overriding function Dispatch(Handler : in Api_Post_Create_Handler; Request : in Status.Data) return Response.Data;

      overriding function Clone(Element : in Api_Post_Create_Handler) return Api_Post_Create_Handler is (Element);

      Api_Post_Create : Api_Post_Create_Handler;
   end Posts;

   package Images is
      type Images_Handler is new Dispatchers.Handler with null record;

      overriding function Dispatch(Handler : in Images_Handler; Request : in Status.Data) return Response.Data;

      overriding function Clone(Element : in Images_Handler) return Images_Handler is (Element);

      Images : Images_Handler;

      -- API

      type Api_Image_Upload_Handler is new Dispatchers.Handler with null record;
      type Api_Image_Delete_Handler is new Dispatchers.Handler with null record;

      overriding function Dispatch(Handler : in Api_Image_Upload_Handler; Request : in Status.Data) return Response.Data;
      overriding function Dispatch(Handler : in Api_Image_Delete_Handler; Request : in Status.Data) return Response.Data;

      overriding function Clone(Element : in Api_Image_Upload_Handler) return Api_Image_Upload_Handler is (Element);
      overriding function Clone(Element : in Api_Image_Delete_Handler) return Api_Image_Delete_Handler is (Element);

      Api_Image_Upload : Api_Image_Upload_Handler;
      Api_Image_Delete : Api_Image_Delete_Handler;
   end Images;

   package Search is
      type Search_Handler is new Dispatchers.Handler with null record;
      type Search_Rss_Handler is new Dispatchers.Handler with null record;

      overriding function Dispatch(Handler : in Search_Handler; Request : in Status.Data) return Response.Data;
      overriding function Dispatch(Handler : in Search_Rss_Handler; Request : in Status.Data) return Response.Data;

      overriding function Clone(Element : in Search_Handler) return Search_Handler is (Element);
      overriding function Clone(Element : in Search_Rss_Handler) return Search_Rss_Handler is (Element);

      Search : Search_Handler;
      Search_Rss : Search_Rss_Handler;
   end Search;

   type Index_Handler is new Dispatchers.Handler with null record;
   type Announce_Handler is new Dispatchers.Handler with null record;
   type Scrape_Handler is new Dispatchers.Handler with null record;
   type Login_Handler is new Dispatchers.Handler with null record;
   type Register_Handler is new Dispatchers.Handler with null record;
   type Download_Handler is new Dispatchers.Handler with null record;
   type Upload_Handler is new Dispatchers.Handler with null record;
   type View_Handler is new Dispatchers.Handler with null record;
   type Invite_Handler is new Dispatchers.Handler with null record;
   type Confirm_Handler is new Dispatchers.Handler with null record;
   type Profile_Handler is new Dispatchers.Handler with null record;
   type Admin_Handler is new Dispatchers.Handler with null record;
   type Group_Create_Handler is new Dispatchers.Handler with null record;
   type Group_Handler is new Dispatchers.Handler with null record;
   type Group_Search_Handler is new Dispatchers.Handler with null record;
   type Group_Rss_Handler is new Dispatchers.Handler with null record;

   overriding function Dispatch(Handler : in Index_Handler; Request : in Status.Data) return Response.Data;
   overriding function Dispatch(Handler : in Announce_Handler; Request : in Status.Data) return Response.Data;
   overriding function Dispatch(Handler : in Scrape_Handler; Request : in Status.Data) return Response.Data;
   overriding function Dispatch(Handler : in Login_Handler; Request : in Status.Data) return Response.Data;
   overriding function Dispatch(Handler : in Register_Handler; Request : in Status.Data) return Response.Data;
   overriding function Dispatch(Handler : in Download_Handler; Request : in Status.Data) return Response.Data;
   overriding function Dispatch(Handler : in Upload_Handler; Request : in Status.Data) return Response.Data;
   overriding function Dispatch(Handler : in View_Handler; Request : in Status.Data) return Response.Data;
   overriding function Dispatch(Handler : in Invite_Handler; Request : in Status.Data) return Response.Data;
   overriding function Dispatch(Handler : in Confirm_Handler; Request : in Status.Data) return Response.Data;
   overriding function Dispatch(Handler : in Profile_Handler; Request : in Status.Data) return Response.Data;
   overriding function Dispatch(Handler : in Admin_Handler; Request : in Status.Data) return Response.Data;
   overriding function Dispatch(Handler : in Group_Create_Handler; Request : in Status.Data) return Response.Data;
   overriding function Dispatch(Handler : in Group_Handler; Request : in Status.Data) return Response.Data;
   overriding function Dispatch(Handler : in Group_Search_Handler; Request : in Status.Data) return Response.Data;
   overriding function Dispatch(Handler : in Group_Rss_Handler; Request : in Status.Data) return Response.Data;

   overriding function Clone(Element : in Index_Handler) return Index_Handler is (Element);
   overriding function Clone(Element : in Announce_Handler) return Announce_Handler is (Element);
   overriding function Clone(Element : in Scrape_Handler) return Scrape_Handler is (Element);
   overriding function Clone(Element : in Login_Handler) return Login_Handler is (Element);
   overriding function Clone(Element : in Register_Handler) return Register_Handler is (Element);
   overriding function Clone(Element : in Download_Handler) return Download_Handler is (Element);
   overriding function Clone(Element : in Upload_Handler) return Upload_Handler is (Element);
   overriding function Clone(Element : in View_Handler) return View_Handler is (Element);
   overriding function Clone(Element : in Invite_Handler) return Invite_Handler is (Element);
   overriding function Clone(Element : in Confirm_Handler) return Confirm_Handler is (Element);
   overriding function Clone(Element : in Profile_Handler) return Profile_Handler is (Element);
   overriding function Clone(Element : in Admin_Handler) return Admin_Handler is (Element);
   overriding function Clone(Element : in Group_Create_Handler) return Group_Create_Handler is (Element);
   overriding function Clone(Element : in Group_Handler) return Group_Handler is (Element);
   overriding function Clone(Element : in Group_Search_Handler) return Group_Search_Handler is (Element);
   overriding function Clone(Element : in Group_Rss_Handler) return Group_Rss_Handler is (Element);


   Index : Index_Handler;
   Announce : Announce_Handler;
   Scrape : Scrape_Handler;
   Login : Login_Handler;
   Register : Register_Handler;
   Download : Download_Handler;
   Upload : Upload_Handler;
   View : View_Handler;
   Invite : Invite_Handler;
   Confirm : Confirm_Handler;
   Profile : Profile_Handler;
   Admin : Admin_Handler;
   Group_Create : Group_Create_Handler;
   Group : Group_Handler;
   Group_Search : Group_Search_Handler;
   Group_Rss : Group_Rss_Handler;

   -- API

   type Api_Upload_Handler is new Dispatchers.Handler with null record;
   type Api_User_Register_Handler is new Dispatchers.Handler with null record;
   type Api_User_Login_Handler is new Dispatchers.Handler with null record;
   type Api_User_Logout_Handler is new Dispatchers.Handler with null record;
   type Api_Delete_Handler is new Dispatchers.Handler with null record;
   type Api_Subscribe_Handler is new Dispatchers.Handler with null record;
   type Api_Notifications_Clear_Handler is new Dispatchers.Handler with null record;
   type Api_Group_Create_Handler is new Dispatchers.Handler with null record;

   overriding function Dispatch(Handler : in Api_Upload_Handler; Request : in Status.Data) return Response.Data;
   overriding function Dispatch(Handler : in Api_User_Register_Handler; Request : in Status.Data) return Response.Data;
   overriding function Dispatch(Handler : in Api_User_Login_Handler; Request : in Status.Data) return Response.Data;
   overriding function Dispatch(Handler : in Api_User_Logout_Handler; Request : in Status.Data) return Response.Data;
   overriding function Dispatch(Handler : in Api_Delete_Handler; Request : in Status.Data) return Response.Data;
   overriding function Dispatch(Handler : in Api_Subscribe_Handler; Request : in Status.Data) return Response.Data;
   overriding function Dispatch(Handler : in Api_Notifications_Clear_Handler; Request : in Status.Data) return Response.Data;
   overriding function Dispatch(Handler : in Api_Group_Create_Handler; Request : in Status.Data) return Response.Data;

   overriding function Clone(Element : in Api_Upload_Handler) return Api_Upload_Handler is (Element);
   overriding function Clone(Element : in Api_User_Register_Handler) return Api_User_Register_Handler is (Element);
   overriding function Clone(Element : in Api_User_Login_Handler) return Api_User_Login_Handler is (Element);
   overriding function Clone(Element : in Api_User_Logout_Handler) return Api_User_Logout_Handler is (Element);
   overriding function Clone(Element : in Api_Delete_Handler) return Api_Delete_Handler is (Element);
   overriding function Clone(Element : in Api_Subscribe_Handler) return Api_Subscribe_Handler is (Element);
   overriding function Clone(Element : in Api_Notifications_Clear_Handler) return Api_Notifications_Clear_Handler is (Element);
   overriding function Clone(Element : in Api_Group_Create_Handler) return Api_Group_Create_Handler is (Element);

   Api_Upload : Api_Upload_Handler;
   Api_User_Register : Api_User_Register_Handler;
   Api_User_Login : Api_User_Login_Handler;
   Api_User_Logout : Api_User_Logout_Handler;
   Api_Delete : Api_Delete_Handler;
   Api_Subscribe : Api_Subscribe_Handler;
   Api_Notifications_Clear : Api_Notifications_Clear_Handler;
   Api_Group_Create : Api_Group_Create_Handler;

   -- Uploads

   type Uploads_Images_Handler is new Dispatchers.Handler with null record;

   overriding function Dispatch(Handler : in Uploads_Images_Handler; Request : in Status.Data) return Response.Data;

   overriding function Clone(Element : in Uploads_Images_Handler) return Uploads_Images_Handler is (Element);

   Uploads_Images : Uploads_Images_Handler;

   Http : Server.Http;
   Root : Services.Dispatchers.Uri.Handler;
   Conf : constant Config.Object := AWS.Config.Get_Current;
   Host : constant String := Aws.Config.Server_Host(Conf) & ":" & Trim(Aws.Config.Server_Port(Conf)'Image, Ada.Strings.Left);

   Session_File_Name : constant String := "session.data";

   function To_Hex_string(Input : String) return String;

   -- Additional options
   Invite_Required : Boolean := True;
   Https : Boolean := False;
   Server_Host : Unbounded_String;

   Uploads_Path : constant String := "uploads/torrents/";
   Image_Uploads_Path : constant String := "uploads/images/";

   function Host_Name return String is (if Server_Host /= "" then To_String(Server_Host) else Aws.Config.Server_Host(Conf));
end Hellish_Web.Routes;
