with Ada.Task_Termination; use Ada.Task_Termination;
with Ada.Task_Identification; use Ada.Task_Identification;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_Io; use Ada.Text_Io;
with Ada.Streams; use Ada.Streams;
with Ada.Strings;

with Gnat.String_Split; use Gnat.String_Split;
with Gnat.Traceback.Symbolic;

with Hellish_Web.Routes;

package body Hellish_Irc is
   protected body Protected_Clients is
      procedure Append(The_Client : in out Client) is
      begin
         The_Client.Id := Next_Id;
         Clients.Include(Next_Id, The_Client);
         Socket_To_Client.Include(The_Client.Socket, The_Client.Id);
         Next_Id := @ + 1;
      end;

      procedure Process_Select_Connections is
         Selector : Selector_Type;

         R_Selector_Set : Socket_Set_Type;
         W_Selector_Set : Socket_Set_Type;
         Status : Selector_Status;

         Curr_Sock : Socket_Type;

         To_Remove : User_Hashed_Sets.Set;
      begin
         for Client of Clients loop
            Gnat.Sockets.Set(R_Selector_Set, Client.Socket);
            Gnat.Sockets.Set(W_Selector_Set, Client.Socket);
         end loop;

         if Is_Empty(R_Selector_Set) then return; end if;

         Create_Selector(Selector);
         Check_Selector(Selector, R_Selector_Set, W_Selector_Set, Status, Timeout => 5.0);
         Close_Selector(Selector);

         while not Is_Empty(R_Selector_Set) loop
            Get(R_Selector_Set, Curr_Sock);

            declare
               Client : Client_Maps.Reference_Type := Clients.Reference(Socket_To_Client(Curr_Sock));

               Element_Array : Stream_Element_Array(0..1024);
               Element_Last : Stream_Element_Offset;
            begin
               -- Peek to check if socket is closed
               Receive_Socket(Client.Socket, Element_Array, Element_Last, Peek_At_Incoming_Data);

               if Element_Last = Element_Array'First - 1 then
                  -- Socket closed
                  To_Remove.Include(Client.Id);

                  goto After;
               end if;

               -- Actually read the data now
               Receive_Socket(Client.Socket, Element_Array, Element_Last);

               -- Read all the content that arrived
               for I in 0..Element_Last loop
                  Append(Client.Unfinished_Messages, Character'Val(Element_Array(I)));
               end loop;

               -- Don't process messages unless the last message is fully received
               -- TODO: maximum size for this?
               if Element(Client.Unfinished_Messages, Length(Client.Unfinished_Messages) - 1) /= Latin_1.Cr
                 or Element(Client.Unfinished_Messages, Length(Client.Unfinished_Messages)) /= Latin_1.Lf then
                  return;
               end if;

               declare
                  use Message_Vectors;

                  Message_Slices : Slice_Set;

                  Message_Queue : Message_Vectors.Vector renames Client.Message_Queue;
                  Messages : Unbounded_String renames Client.Unfinished_Messages;
               begin
                  Create(Message_Slices, To_String(Messages), Cr_Lf, Multiple);
                  for Message of Message_Slices loop
                     if Message'Length > 0 then
                        Put_Line(Message);

                        declare
                           use String_Vectors;

                           Part_Slices : Slice_Set;
                           Message_Parts : String_Vectors.Vector;

                           Unbounded_Message : Unbounded_String;
                           Maybe_Trailing : Unbounded_String;
                           Col_Index : Natural := Index(Message, ":");
                        begin
                           if Col_Index /= 0 and Col_Index /= Message'First then
                              Maybe_Trailing := To_Unbounded_String(Message(Col_Index..Message'Length));
                              Unbounded_Message := To_Unbounded_String(Message(Message'First..Col_Index - 2));
                           else
                              Unbounded_Message := To_Unbounded_String(Message);
                           end if;

                           Create(Part_Slices, To_String(Unbounded_Message), " ", Multiple);
                           for Part of Part_Slices loop
                              Append(Message_Parts, Part);
                           end loop;
                           if Maybe_Trailing /= "" then
                              Append(Message_Parts, To_String(Maybe_Trailing));
                           end if;

                           Append(Message_Queue, Message_Parts);
                        end;
                     end if;

                     -- Clear unfinished messages
                     Messages := To_Unbounded_String("");
                  end loop;
               end;
            end;

         <<After>>
         end loop;

         Remove(To_Remove);
      end Process_Select_Connections;

      procedure Process_Message_Queues is
         To_Remove : User_Hashed_Sets.Set;
      begin
         for Client of Clients loop
            declare
               Queue : Message_Vectors.Vector renames Client.Message_Queue;
            begin
               for Message_Parts of Queue loop
                  if Message_Parts(0) = "NICK" then
                     if Users.Contains(Message_Parts(1)) then
                        -- Nick already known
                        Send(Client, Err_Nickname_In_Use & " " & Message_Parts(1) & " :" & Message_Parts(1) & " already taken");
                     else
                        if not Client.Nick.Is_Empty then
                           -- Remove old nickname binding
                           Users.Delete(Client.Nick.Element);
                        end if;

                        -- Save the nickname
                        Users.Include(Key => Message_Parts(1), New_Item => Client.Id);

                        Client.Nick := To_Holder(Message_Parts(1));
                        Put_Line("Nickname set to " & Client.Nick.Element);
                     end if;
                  elsif Message_Parts(0) = "USER" then
                     Client.Username := To_Holder(Message_Parts(1));
                     Put_Line("Username set to " & Client.Username.Element);
                  elsif Message_Parts(0) = "CAP" then
                     if Message_Parts(1) = "LS" then
                        Client.Caps_Negotiated := False;
                        Send(Client, "CAP * LS :");
                     elsif Message_Parts(1) = "END" then
                        Client.Caps_Negotiated := True;
                     end if;
                  elsif Message_Parts(0) = "PING" then
                     if Message_Parts(1) = Irc_Host.Element then
                        Send(Client, "PONG " & Irc_Host.Element & " :" & Irc_Host.Element);
                     end if;
                  elsif Message_Parts(0) = "QUIT" then
                     To_Remove.Include(Client.Id);

                     for Channel of Channels loop
                        if Channel.Users.Contains(Client.Id) then
                           declare
                              To_Send : Unbounded_String;
                           begin
                              To_Send := @ & "PART " & Channel.Name.Element;
                              if Message_Parts.Length > 1 then
                                 To_Send := @ & " " & Message_Parts(1);
                              end if;

                              for Channel_User of Channel.Users loop
                                 Send(Clients(Channel_User), To_String(To_Send), From => Client.Nick.Element);
                              end loop;
                           end;
                        end if;
                     end loop;

                     goto After;
                  end if;

                  if Client.Username.Is_Empty or Client.Username.Is_Empty or not Client.Caps_Negotiated then
                     goto After;
                  end if;

                  if Message_Parts(0) = "JOIN" then
                     declare
                        Channel_Slices : Slice_Set;
                     begin
                        Create(Channel_Slices, Message_Parts(1), ",");
                        for The_Channel of Channel_Slices loop
                           -- Starts from not-zero/one for some reason, so have to use 'First
                           if The_Channel(The_Channel'First) /= '#' then
                              Send(Client, Err_No_Such_Channel & " " & The_Channel(1) & " :Channel name must start with #");
                           else
                              Join_Channel(Client, The_Channel);
                           end if;
                        end loop;
                     end;
                  elsif Message_Parts(0) = "PART" then
                     if Channels.Contains(Message_Parts(1)) then
                        declare
                           To_Send : String := Join_Parts(Message_Parts);
                        begin
                           for Channel_User of Channels(Message_Parts(1)).Users loop
                              if Channel_User /= Client.Id then
                                 Send(Clients(Channel_User), To_Send, From => Client.Nick.Element);
                              end if;
                           end loop;
                        end;
                     end if;
                  elsif Message_Parts(0) = "WHOIS" then
                     declare
                        User_Slices : Slice_Set;
                     begin
                        Create(User_Slices, Message_Parts(1), ",");
                        for Whois_Username of User_Slices loop
                           if Users.Contains(Whois_Username) then
                              declare
                                 The_User : Client_Cref := Clients(Users(Whois_Username));
                              begin
                                 Send(Client, Rpl_Whois_User
                                        & " " & The_User.Nick.Element
                                        & " " & The_User.Username.Element
                                        & " " & Irc_Host.Element & " * :unknown");
                                 Send(Client, Rpl_End_Of_Whois & " :End of WHOIS");
                              end;
                           else
                              Send(Client, Err_No_Such_Nick & " " & Whois_Username & " :No such nickname");
                           end if;
                        end loop;
                     end;
                  elsif Message_Parts(0) = "PRIVMSG" then
                     if Channels.Contains(Message_Parts(1)) then
                        declare
                           To_Send : String := Join_Parts(Message_Parts);
                        begin
                           for Channel_User of Channels(Message_Parts(1)).Users loop
                              if Channel_User /= Client.Id then
                                 Send(Clients(Channel_User), To_Send, From => Client.Nick.Element);
                              end if;
                           end loop;
                        end;
                     elsif Users.Contains(Message_Parts(1)) then
                        Send(Clients(Users(Message_Parts(1))), Join_Parts(Message_Parts), From => Client.Nick.Element);
                     end if;
                  end if;

               <<After>>
               end loop;

               if Client.Caps_Negotiated and not Client.Nick.Is_Empty and not Client.Motd_Sent then
                  Send(Client, "001 " & Client.Nick.Element & " :Welcome to Hellish IRC!");
                  Send(Client, Rpl_Motd_Start & " : ** Message of the day:");
                  Send(Client, Rpl_Motd & " : **   Nothing in particular..");
                  Send(Client, Rpl_End_Of_Motd & " : ** End of MOTD");
                  Client.Motd_Sent := True;
               end if;

               Queue.Clear;
            end;
         end loop;

         Remove(To_Remove);
      end Process_Message_Queues;

      procedure Remove(To_Remove : User_Hashed_Sets.Set) is
      begin
         for Removed of To_Remove loop
            if not Clients(Removed).Nick.Is_Empty then
               Put_Line("!! User " & Clients(Removed).Nick.Element & " quit");
               Users.Delete(Clients(Removed).Nick.Element);
            end if;

            Socket_To_Client.Delete(Clients(Removed).Socket);

            for Channel of Channels loop
               Channel.Users.Exclude(Removed);
            end loop;

            Clients.Delete(Removed);
         end loop;
      end Remove;

      procedure Process_Clients is
      begin
         Process_Select_Connections;
         Process_Message_Queues;
      end;

      procedure Send(The_Client : Client; Message : String; From : String := Irc_Host.Element) is
         Message_Full : String := ":" & From & " " & Message;
         Message_Crlf : String := Message_Full & Cr_Lf;
         Element_Array : Stream_Element_Array(1..Message_Crlf'Length);
         Element_Last : Stream_Element_Offset;
      begin
         Put_Line("SENT: " & Message_Full);

         for Char_I in 1..Message_Crlf'Length loop
            Element_Array(Stream_Element_Offset(Char_I)) := Character'Pos(Message_Crlf(Char_I));
         end loop;

         Send_Socket(The_Client.Socket, Element_Array, Element_Last);
         if Element_Last /= Element_Array'Length then
            Put_Line("Wanted to send" & Element_Array'Length'Image & " but sent" & Element_Last'Image);
         end if;
      end Send;

      procedure Join_Channel(The_Client : Client; Channel_Name : String) is
      begin
         if Channels.Contains(Channel_Name) then
            for User of Channels(Channel_Name).Users loop
               Send(Clients(User), "JOIN " & Channel_Name, From => The_Client.Nick.Element);
            end loop;

            Channels(Channel_Name).Users.Include(The_Client.Id);
         else
            declare
               New_Channel : Channel;
            begin
               New_Channel.Name := To_Holder(Channel_Name);
               New_Channel.Users.Include(The_Client.Id);

               Channels.Include(Channel_Name, New_Channel);
            end;
         end if;

         Send_Topic(The_Client, Channel_Name);
         Send_Names(The_Client, Channel_Name);
      end Join_Channel;

      procedure Send_Topic(The_Client : Client; Channel_Name : String) is
         Channel : Channel_Maps.Constant_Reference_Type := Channels(Channel_Name);
      begin
         if Channel.Topic.Is_Empty then
            Send(The_Client, Rpl_Notopic & " " & Channel.Name.Element & " :No topic");
         else
            Send(The_Client, Rpl_Topic & " " & Channel.Name.Element & " :" & Channel.Topic.Element);
         end if;
      end Send_Topic;

      procedure Send_Names(The_Client : Client; Channel_Name : String) is
         Channel : Channel_Maps.Constant_Reference_Type := Channels(Channel_Name);
         User_Str : Unbounded_String;
         User_Str_Prefix : constant Unbounded_String :=
           To_Unbounded_String(Rpl_Name_Reply & " " & The_Client.Nick.Element & " = " & Channel.Name.Element & " :");
      begin
         User_Str := User_Str_Prefix;
         for User of Channel.Users loop
            declare
               Client : Client_Cref := Clients(User);
            begin
               if Length(User_Str) + Client.Nick.Element'Length < 400 then
                  User_Str := @ & Clients(User).Nick.Element & " ";
               else
                  Send(The_Client, To_String(User_Str));
                  User_Str := User_Str_Prefix;
               end if;
            end;
         end loop;
         if User_Str /= User_Str_Prefix then
            Send(The_Client, To_String(User_Str));
         end if;

         Send(The_Client, Rpl_End_Of_Names & " " & The_Client.Nick.Element & " " & Channel.Name.Element & " :End of NAMES list");
      end Send_Names;
   end Protected_Clients;

   function Join_Parts(Parts : String_Vectors.Vector) return String is
      To_Send : Unbounded_String;
   begin
      for Part of Parts loop
         To_Send := @ & Part & " ";
      end loop;
      To_Send := Trim(To_Send, Ada.Strings.Both);

      return To_String(To_Send);
   end Join_Parts;

   task body Accept_Connections is
   begin
      -- Wait for socket open
      accept Start;

      loop
         declare
            The_Client : Client;
            Status : Selector_Status;
         begin
            Accept_Socket(Socket, The_Client.Socket, The_Client.Address,
                          Timeout => Forever, Status => Status);
            if Status = Completed then
               Protected_Clients.Append(The_Client);
            end if;
         end;
      end loop;
   end Accept_Connections;

   task body Process_Connections is
   begin
      accept Start;

      loop
         Protected_Clients.Process_Clients;
         delay 0.5;
      end loop;
   end Process_Connections;

   protected Termination_Handler is
      procedure Handler(Cause : Cause_Of_Termination;
                        Id : Task_Id;
                        E : Exception_Occurrence);
   end Termination_Handler;
   protected body Termination_Handler is
      procedure Handler(Cause : Cause_Of_Termination;
                        Id : Task_Id;
                        E : Exception_Occurrence) is
      begin
         Put_Line(Exception_Name(E));
         Put_Line(Exception_Information(E));
         Put_Line(GNAT.Traceback.Symbolic.Symbolic_Traceback(E));
      end Handler;
   end Termination_Handler;

   procedure Start is
      Addr : Sock_Addr_Type;
   begin
      Irc_Host := To_Holder(Hellish_Web.Routes.Host_Name);

      Addr.Addr := (Family => Family_Inet, Sin_V4 => (others => 0));
      Addr.Port := Port_Type(Port);

      Create_Socket(Socket);
      Set_Socket_Option(Socket, Socket_Level, (Reuse_Address, True));
      Bind_Socket(Socket, Addr);
      Listen_Socket(Socket, Queue_Size);

      Set_Specific_Handler(Accept_Connections'Identity, Termination_Handler.Handler'Access);
      Set_Specific_Handler(Process_Connections'Identity, Termination_Handler.Handler'Access);

      Accept_Connections.Start;
      Process_Connections.Start;
   end Start;
end Hellish_Irc;
