with AWS.Dispatchers;
with AWS.Response;
with AWS.Status;
with Aws.Config;
with Aws.Server;
with Aws.Services.Dispatchers.URI;

package Hellish_Web.Routes is
   use Aws;  
   
   type Announce_Handler is new Dispatchers.Handler with null record;
   
   overriding function Dispatch
     (Handler : in Announce_Handler;
      Request : in Status.Data) return Response.Data;
   
   procedure Run_Server;
private
   overriding function Clone(Element : in Announce_Handler) return Announce_Handler;
   
   Http : Server.Http;  
   Root : Services.Dispatchers.Uri.Handler;
   Conf : constant Config.Object := AWS.Config.Get_Current;

   Announce : Announce_Handler;     
end Hellish_Web.Routes;
