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
   type Announce_Handler is new Dispatchers.Handler with null record;
   type Scrape_Handler is new Dispatchers.Handler with null record;

   overriding function Dispatch
     (Handler : in Announce_Handler;
      Request : in Status.Data) return Response.Data;
   overriding function Dispatch
     (Handler : in Scrape_Handler;
      Request : in Status.Data) return Response.Data;

   overriding function Clone(Element : in Announce_Handler) return Announce_Handler is (Element);
   overriding function Clone(Element : in Scrape_Handler) return Scrape_Handler is (Element);

   Http : Server.Http;
   Root : Services.Dispatchers.Uri.Handler;
   Conf : constant Config.Object := AWS.Config.Get_Current;

   Announce : Announce_Handler;
   Scrape : Scrape_Handler;

   function To_Hex_string(Input : String) return String;
end Hellish_Web.Routes;
