with Ada.Containers.Indefinite_Vectors;
with Ada.Containers.Indefinite_Holders;

with Aws.Smtp, Aws.Smtp.Client;
use Aws.Smtp;

with Markdown; use Markdown;

package Hellish_Mail is
   Default_Md_Flags : Markdown.Parser_Flag := Md_Flag_No_Html_Blocks or Md_Flag_No_Html_Spans;

   procedure Start;
   procedure Send(To : E_Mail_Data; Subject, Message : String);

   package String_Holders is new Ada.Containers.Indefinite_Holders(String);
   use String_Holders;

   Smtp_Server : String_Holders.Holder;
   Smtp_Port : Integer := 465;
   From_Address : String_Holders.Holder;
private
   type Email_Entry is record
      To: E_Mail_Data;
      Subject, Message : String_Holders.Holder;
   end record;
   package Email_Entry_Vectors is new Ada.Containers.Indefinite_Vectors
     (Index_Type => Natural, Element_Type => Email_entry);
   use Email_Entry_Vectors;
   Email_Queue : Email_Entry_Vectors.Vector;

   task Process_Emails;
end Hellish_Mail;
