with AWS.Dispatchers;
with AWS.Response;
with AWS.Status;
with Aws.Config;
with Aws.Server;
with Aws.Services.Dispatchers.URI;

package Hellish_Web.Routes is
   use Aws;

   procedure Run_Server;
private
   type Index_Handler is new Dispatchers.Handler with null record;
   type Announce_Handler is new Dispatchers.Handler with null record;
   type Scrape_Handler is new Dispatchers.Handler with null record;

   type Api_Upload_Handler is new Dispatchers.Handler with null record;
   type Api_User_Register_Handler is new Dispatchers.Handler with null record;
   type Api_User_Login_Handler is new Dispatchers.Handler with null record;

   overriding function Dispatch(Handler : in Index_Handler; Request : in Status.Data) return Response.Data;
   overriding function Dispatch(Handler : in Announce_Handler; Request : in Status.Data) return Response.Data;
   overriding function Dispatch(Handler : in Scrape_Handler; Request : in Status.Data) return Response.Data;

   overriding function Dispatch(Handler : in Api_Upload_Handler; Request : in Status.Data) return Response.Data;
   overriding function Dispatch(Handler : in Api_User_Register_Handler; Request : in Status.Data) return Response.Data;
   overriding function Dispatch(Handler : in Api_User_Login_Handler; Request : in Status.Data) return Response.Data;

   overriding function Clone(Element : in Index_Handler) return Index_Handler is (Element);
   overriding function Clone(Element : in Announce_Handler) return Announce_Handler is (Element);
   overriding function Clone(Element : in Scrape_Handler) return Scrape_Handler is (Element);

   overriding function Clone(Element : in Api_Upload_Handler) return Api_Upload_Handler is (Element);
   overriding function Clone(Element : in Api_User_Register_Handler) return Api_User_Register_Handler is (Element);
   overriding function Clone(Element : in Api_User_Login_Handler) return Api_User_Login_Handler is (Element);

   Http : Server.Http;
   Root : Services.Dispatchers.Uri.Handler;
   Conf : constant Config.Object := AWS.Config.Get_Current;

   Index : Index_Handler;
   Announce : Announce_Handler;
   Scrape : Scrape_Handler;

   Api_Upload : Api_Upload_Handler;
   Api_User_Register : Api_User_Register_Handler;
   Api_User_Login : Api_User_Login_Handler;

   Session_File_Name : constant String := "session.data";

   function To_Hex_string(Input : String) return String;
end Hellish_Web.Routes;
