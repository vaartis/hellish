with Ada.Text_Io; use Ada.Text_Io;
with Ada.Task_Termination; use Ada.Task_Termination;
with Ada.Task_Identification; use Ada.Task_Identification;
with Ada.Exceptions; use Ada.Exceptions;

with
  Aws.Attachments,
  Aws.Mime,
  Aws.Smtp.Client;
use Aws.Attachments;

package body Hellish_Mail is
   procedure Send(To : E_Mail_Data; Subject, Message : String) is
   begin
      Append(Email_Queue, Email_Entry'(To => To,
                                       Subject => To_Holder(Subject), Message => To_Holder(Message)));
   end Send;

   task body Process_Emails is
   begin
      loop
         if Email_Queue.Is_Empty then
            -- Sleep for a bit
            delay 5.0;
            goto Next;
         end if;

         declare
            use Aws;

            Smtp_Srv : Receiver := Smtp.Client.Initialize(Smtp_Server.Element, Port => Smtp_Port);
            Message_Status : Status;

            Curr_Entry : Email_Entry := Email_Queue(Email_Queue.First);
            To : E_Mail_Data := Curr_Entry.To;
            Subject : String := Curr_Entry.Subject.Element;
            Message : String := Curr_Entry.Message.Element;

            Attachment_List : Attachments.List;
            Message_Attachment : Attachments.Content := Value(Data => Markdown.To_Html(Message, Default_Md_Flags),
                                                              Name => "message.html",
                                                              Content_Type => Aws.Mime.Text_Html);

            Alternatives : Attachments.Alternatives;
         begin
            Delete_First(Email_Queue);

            Attachments.Add(Alternatives, Message_Attachment);
            Attachments.Add(Alternatives, Value(Message, Content_Type => MIME.Text_Plain));
            Attachments.Add(Attachment_List, Alternatives);

            Smtp.Client.Send(Smtp_Srv,
                             From => E_Mail("Hellish", From_Address.Element),
                             To => [1 => To],
                             Subject => Subject,
                             Attachments => Attachment_List,
                             Status => Message_Status);
            if Is_Ok(Message_Status) then
               Put_Line("Sent e-mail to " & Image(To, Name));
            else
               Put_Line("Could not send e-mail to " & Image(To, Name));
               Put_Line(Status_Message(Message_Status));
            end if;
         end;

         <<Next>>
      end loop;
   end Process_Emails;

   protected Handlers is
      procedure Termination_Handler(Unused_Cause : Cause_Of_Termination;
                                    Id : Task_Id;
                                    E : Exception_Occurrence);
   end Handlers;
   protected body Handlers is
      procedure Termination_Handler(Unused_Cause : Cause_Of_Termination;
                                    Id : Task_Id;
                                    E : Exception_Occurrence) is
      begin
         Put_Line("!! TASK CRASHED " & Image(Id));
         Put_Line(Exception_Information(E));
      end Termination_Handler;
   end Handlers;

   procedure Start is
   begin
      Set_Specific_Handler(Process_Emails'Identity, Handlers.Termination_Handler'Access);
   end;
end Hellish_Mail;
