with Ada.Text_Io; use Ada.Text_Io;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

with Aws.Server.Log;
with Aws.Config;
with Aws.Services.Dispatchers.URI;
with AWS.Mime;

with Json.Parsers;

with Hellish_Web.Bencoder;

package body Hellish_Web.Routes is
   function Clone(Element : in Announce_Handler) return Announce_Handler is
   begin
      return Element;
   end Clone;

   function Dispatch
     (Handler : in Announce_Handler;
      Request : in Status.Data) return Response.Data is
      Parser : Bencoder.Json_Parsers.Parser := Bencoder.Json_Parsers.Create("{""failure reason"":""testing""}");
      Value : constant Bencoder.Json_Types.Json_Value := Parser.Parse;
   begin
      Put_Line(Status.Parameters(Request).URI_Format);

      return Response.Build(Mime.Text_Html, Bencoder.Encode(Value).Image);
   end Dispatch;

   procedure Run_Server is
   begin
      Services.Dispatchers.Uri.Register(Root, "/announce", Announce);
      Server.Start(Hellish_Web.Routes.Http, Root, Conf);
      Server.Log.Start(Http, Put_Line'Access, "hellish");

      Put_Line("Started on http://" & Config.Server_Host(Conf)
                 -- Trim the number string on the left because it has a space for some reason
                 & ":" & Trim(Config.Server_Port(Conf)'Image, Ada.Strings.Left));
      Server.Wait(Server.Q_Key_Pressed);

      Server.Shutdown(Http);
   end Run_Server;
end Hellish_Web.Routes;
