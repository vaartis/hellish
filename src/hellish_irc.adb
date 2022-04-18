with Ada.Task_Termination; use Ada.Task_Termination;
with Ada.Task_Identification; use Ada.Task_Identification;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_Io; use Ada.Text_Io;
with Ada.Text_Io.Unbounded_Io; use Ada.Text_Io.Unbounded_Io;
with Ada.Streams; use Ada.Streams;
with Ada.Calendar; use Ada.Calendar;
with Ada.Calendar.Time_Zones; use Ada.Calendar.Time_Zones;
with Ada.Directories;
with Ada.Strings;

with Gnat.Regpat; use Gnat.Regpat;
with Gnat.String_Split; use Gnat.String_Split;

with Gnatcoll.Json;

with Hellish_Web.Routes;
with Hellish_Web.Database;
use Hellish_Web;

package body Hellish_Irc is
   package body Ssl is separate;

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

               Max_Read : constant := 1024;
               Element_Array : Stream_Element_Array(0..Max_Read);
               Element_Last : Stream_Element_Offset;
            begin
               -- Peek to check if socket is closed
               Receive_Socket(Client.Socket, Element_Array, Element_Last, Peek_At_Incoming_Data);

               if Element_Last = Element_Array'First - 1 then
                  -- Socket closed
                  To_Remove.Include(Client.Id);

                  goto After;
               end if;

               if Client.Is_Ssl then
                  declare
                     Unused_Amount : Integer;
                     Read_String : String := Ssl.Receive_Ssl(Client.Socket_Ssl, Max_Read, Unused_Amount);
                  begin
                     Append(Client.Unfinished_Messages, Read_String);
                  end;
               else
                  -- Actually read the data now
                  Receive_Socket(Client.Socket, Element_Array, Element_Last);

                  -- Read all the content that arrived
                  for I in 0..Element_Last loop
                     Append(Client.Unfinished_Messages, Character'Val(Element_Array(I)));
                  end loop;
               end if;

               -- Don't process messages unless the last message is fully received
               if Element(Client.Unfinished_Messages, Length(Client.Unfinished_Messages) - 1) /= Latin_1.Cr
                 or Element(Client.Unfinished_Messages, Length(Client.Unfinished_Messages)) /= Latin_1.Lf then
                  if Length(Client.Unfinished_Messages) > 10_000 or Length(Client.Message_Queue) > 100 then
                     -- That's way too much
                     To_Remove.Include(Client.Id);
                     goto After;
                  end if;
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
                              Maybe_Trailing := To_Unbounded_String(Message(Col_Index..Message'Last));
                              Unbounded_Message := To_Unbounded_String(Message(Message'First..Col_Index - 2));
                           else
                              Unbounded_Message := To_Unbounded_String(Message);
                           end if;

                           Create(Part_Slices, To_String(Unbounded_Message), " ", Multiple);
                           for Part of Part_Slices loop
                              if Part /= "" then
                                 Append(Message_Parts, Part);
                              end if;
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
            exception
               when E : others =>
                  declare
                     E_Info : String := Exception_Information(E);
                     Error_Slices : Slice_Set;
                  begin
                     Put_Line(E_Info);

                     Create(Error_Slices, E_Info, Cr_Lf, Multiple);

                     for Slice of Error_Slices loop
                        if Slice /= "" then
                           Send(Client, Err_Unknown_Error & " ERROR :!! " & Slice);
                        end if;
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
                        Send(Client, Err_Nickname_In_Use & " " & Message_Parts(1) & " " & Message_Parts(1) & " :" & Message_Parts(1) & " already taken");
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
                     declare
                        Real_Name : String := Message_Parts(4);
                     begin
                        Client.Real_Name := To_Holder(Real_Name(Real_Name'First + 1..Real_Name'Last));
                     end;
                     Put_Line("Username set to " & Client.Username.Element);
                  elsif Message_Parts(0) = "CAP" then
                     if Message_Parts(1) = "LS" then
                        Client.Caps_Negotiated := False;
                        Send(Client, "CAP * LS :");
                     elsif Message_Parts(1) = "END" then
                        Client.Caps_Negotiated := True;
                     end if;
                  elsif Message_Parts(0) = "PING" then
                     Send(Client, "PONG " & Message_Parts(1));
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
                              Send(Clients(Channel_User), To_Send, From => Client.Nick.Element);
                           end loop;

                           Channels(Message_Parts(1)).Users.Exclude(Client.Id);
                        end;
                     end if;
                  elsif Message_Parts(0) = "WHOIS" then
                     Send_Whois(Client, Message_Parts);
                  elsif Message_Parts(0) = "MODE" then
                     if Channels.Contains(Message_Parts(1)) then
                        if Length(Message_Parts) >= 3 then
                           declare
                              Req_Str : String := Message_Parts(2);
                           begin
                              if Req_Str(Req_Str'First) /= '+' and Req_Str(Req_Str'First) /= '-' then
                                 -- Request for user modes or something? Don't respond with anything (for now)
                                 goto After;
                              end if;
                           end;

                           if Client.Tracker_User = No_Detached_User or else Client.Tracker_User.Role /= 1 then
                              Send(Client, Err_Chan_Op_Privs_Needed
                                     & " " & Client.Nick.Element & " " & Message_Parts(1) & " :Only admins can channel set modes");
                              goto After;
                           end if;

                           declare
                              Mode_String : String := Message_Parts(2);
                              I : Natural := Mode_String'First;

                              Mode_Action : Character := ' ';
                              Mode_Key : Character := ' ';
                           begin
                              while I <= Mode_String'Last loop
                                 if Mode_String(I) = '+' or Mode_String(I) = '-' then
                                    Mode_Action := Mode_String(I);
                                    I := @ + 1;
                                 end if;

                                 if I <= Mode_String'Last then
                                    Mode_Key := Mode_String(I);
                                    I := @ + 1;

                                    if Mode_Action = '+' then
                                       Channels(Message_Parts(1)).Modes.Include((1 => Mode_Key));
                                    elsif Mode_Action = '-' then
                                       Channels(Message_Parts(1)).Modes.Exclude((1 => Mode_Key));
                                    end if;
                                 end if;
                              end loop;
                           end;
                           Persist_Channel(Channels(Message_Parts(1)));
                        end if;

                        declare
                           Mode_Str : Unbounded_String;
                        begin
                           for Mode of Channels(Message_Parts(1)).Modes loop
                              Mode_Str := @ & "+" & Mode;
                           end loop;

                           for User_Id of Channels(Message_Parts(1)).Users loop
                              Send(Client, Rpl_Channel_Mode_Is & " " & Clients(User_Id).Nick.Element & " "
                                     & Channels(Message_Parts(1)).Name.Element & " " & To_String(Mode_Str));
                           end loop;
                        end;
                     end if;
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
                     elsif Message_Parts(1) = "hellish" then
                        Special_Message(Client, Message_Parts(2));
                     elsif Users.Contains(Message_Parts(1)) then
                        declare
                           Sent_To : Hellish_Irc.Client := Clients(Users(Message_Parts(1)));
                        begin
                           if not Sent_To.Away_Message.Is_Empty then
                              Send(Client, Rpl_Away & " " & Client.Nick.Element & " " & Sent_To.Nick.Element
                                  & " " & Sent_To.Away_Message.Element);
                           end if;

                           Send(Sent_To, Join_Parts(Message_Parts), From => Client.Nick.Element);
                        end;
                     end if;
                  elsif Message_Parts(0) = "TOPIC" then
                     if Length(Message_Parts) = 2 then
                        if Channels.Contains(Message_Parts(1)) then
                           if Channels(Message_Parts(1)).Users.Contains(Client.Id) then
                              Send_Topic(Client, Message_Parts(1));
                           else
                              Send(Client, Err_Not_On_Channel & " " & Client.Nick.Element & " " & Message_Parts(1) &
                                     " :You're not on the channel");
                              goto After;
                           end if;
                        end if;
                     elsif Length(Message_Parts) = 3 then
                        if Channels.Contains(Message_Parts(1)) then
                           if Channels(Message_Parts(1)).Users.Contains(Client.Id) then
                              -- Protected topic
                              if Channels(Message_Parts(1)).Modes.Contains("t")
                                and then (Client.Tracker_User = No_Detached_User or else Client.Tracker_User.Role /= 1) then
                                 Send(Client, Err_Chan_Op_Privs_Needed
                                        & " " & Client.Nick.Element & " " & Message_Parts(1) & " :Only admins can change protected topic");
                              else
                                 if Trim(Message_Parts(2), Ada.Strings.Both) = ":" then
                                    Channels(Message_Parts(1)).Topic.Clear;
                                 else
                                    declare
                                       Topic_Str : String := Message_Parts(2);

                                       Epoch : constant Time := Time_Of(1970, 1, 1, 0.0);
                                    begin
                                       Channels(Message_Parts(1)).Topic := To_Holder(Topic_Str(Topic_Str'First + 1..Topic_Str'Last));
                                       Channels(Message_Parts(1)).Topic_Set_By := Client.Nick;
                                       Channels(Message_Parts(1)).Topic_Set_At := Positive(Clock - Epoch - Duration(60 * UTC_Time_Offset));
                                    end;
                                 end if;
                                 Persist_Channel(Channels(Message_Parts(1)));

                                 -- Send new topic
                                 for User of Channels(Message_Parts(1)).Users loop
                                    Send_Topic(Clients(User), Message_Parts(1), From => Client.Nick.Element);
                                 end loop;
                              end if;
                           else
                              Send(Client, Err_Not_On_Channel & " " & Client.Nick.Element & " " & Message_Parts(1) &
                                     " :You're not on that channel");
                              goto After;
                           end if;
                        end if;
                     end if;
                  elsif Message_Parts(0) = "NAMES" then
                     declare
                        Channel_Slices : Slice_Set;
                     begin
                        Create(Channel_Slices, Message_Parts(1), ",");
                        for The_Channel of Channel_Slices loop
                           if Channels.Contains(The_Channel) then
                              if Channels(The_Channel).Users.Contains(Client.Id) then
                                 Send_Names(Client, The_Channel);
                              else
                                 Send(Client, Err_Not_On_Channel & " " & Client.Nick.Element & " " & The_Channel &
                                        " :You're not on channel");
                              end if;
                           end if;
                        end loop;
                     end;
                  elsif Message_Parts(0) = "WHO" then
                     if Channels.Contains(Message_Parts(1)) then
                        for User of Channels(Message_Parts(1)).Users loop
                           declare
                              The_User : Client_Cref := Clients(User);
                           begin
                              Send(Client, Rpl_Who_Reply & " " & Client.Nick.Element & " " & Channels(Message_Parts(1)).Name.Element
                                     & " " & The_User.Username.Element & " unknown " & Irc_Host.Element & " " & The_User.Nick.Element & " "
                                     & (if The_User.Away_Message.Is_Empty then "H" else "G")
                                     & (if The_User.Tracker_User /= No_Detached_User and then The_User.Tracker_User.Role = 1
                                        then "*" else "")
                                     & " :0 " & The_User.Real_Name.Element);
                           end;
                        end loop;
                        Send(Client, Rpl_End_Of_Who & " " & Client.Nick.Element & " " & Message_Parts(1) & " :End of WHO list");
                     end if;
                  elsif Message_Parts(0) = "LIST" then
                     if Length(Message_Parts) < 2 then
                        -- All
                        for Channel of Channels loop
                           Send_List(Client, Channel.Name.Element);
                        end loop;
                     else
                        declare
                           Channel_Slices : Slice_Set;
                        begin
                           Create(Channel_Slices, Message_Parts(1), ",");
                           for The_Channel of Channel_Slices loop
                              if Channels.Contains(The_Channel) then
                                 Send_List(Client, The_Channel);
                              end if;
                           end loop;
                        end;
                     end if;

                     Send(Client, Rpl_List_End & " " & Client.Nick.Element & " :End of LIST");
                  elsif Message_Parts(0) = "AWAY" then
                     if Length(Message_Parts) = 2 then
                        Client.Away_Message := To_Holder(Message_Parts(1));
                        Send(Client, Rpl_Now_Away & " " & Client.Nick.Element & " :You have been marked as being away");
                     elsif Length(Message_Parts) = 1 then
                        Client.Away_Message.Clear;
                        Send(Client, Rpl_Unaway & " " & Client.Nick.Element & " :You are no longer marked as being away");
                     end if;
                  end if;

               <<After>>
               end loop;

               if Client.Caps_Negotiated and not Client.Nick.Is_Empty and not Client.Motd_Sent then
                  Send(Client, "001 " & Client.Nick.Element & " :Welcome to Hellish IRC!");
                  Send(Client, Rpl_Motd_Start & " : ** Message of the day:");

                  declare
                     use Ada.Directories;

                     Motd_File : File_Type;
                     Motd_Line : Unbounded_String;
                  begin
                     if Exists("motd.txt") then
                        Open(Motd_File, Mode => In_File, Name => "motd.txt");
                        while not End_Of_File(Motd_File) loop
                           Get_Line(Motd_File, Motd_Line);
                           Send(Client, Rpl_Motd & " : **   " & To_String(Motd_Line));
                        end loop;
                        Close(Motd_File);
                     else
                        Send(Client, Rpl_Motd & " : **   Nothing in particular..");
                     end if;
                  end;

                  Send(Client, Rpl_End_Of_Motd & " : ** End of MOTD");
                  Client.Motd_Sent := True;
               end if;

               Queue.Clear;
            exception
               when E : others =>
                  Queue.Clear;

                  declare
                     E_Info : String := Exception_Information(E);
                     Error_Slices : Slice_Set;
                  begin
                     Put_Line(E_Info);

                     Create(Error_Slices, E_Info, Cr_Lf, Multiple);

                     for Slice of Error_Slices loop
                        if Slice /= "" then
                           Send(Client, Err_Unknown_Error & " ERROR :!! " & Slice);
                        end if;
                     end loop;
                  end;
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

            if Clients(Removed).Is_Ssl then
               Ssl.Free_Ssl(Clients(Removed).Socket_Ssl);
            end if;
            Close_Socket(Clients(Removed).Socket);

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

         if The_Client.Is_Ssl then
            declare
               Sent_Amount : Integer := Ssl.Send_Ssl(The_Client.Socket_Ssl, Message_Crlf);
            begin
               if Sent_Amount /= Message_Crlf'Length then
                  Put_Line("Wanted to send" & Message_Crlf'Length'Image & " but sent" & Sent_Amount'Image);
               end if;
            end;
         else
            for Char_I in 1..Message_Crlf'Length loop
               Element_Array(Stream_Element_Offset(Char_I)) := Character'Pos(Message_Crlf(Char_I));
            end loop;

            Send_Socket(The_Client.Socket, Element_Array, Element_Last);
            if Element_Last /= Element_Array'Length then
               Put_Line("Wanted to send" & Element_Array'Length'Image & " but sent" & Element_Last'Image);
            end if;
         end if;
      end Send;

      procedure Join_Channel(The_Client : Client; Channel_Name : String) is
      begin
         if Channels.Contains(Channel_Name) then
            -- Don't try to add the user again if they've already joined
            if Channels(Channel_Name).Users.Contains(The_Client.Id) then
               return;
            end if;

            if Channels(Channel_Name).Modes.Contains("i") then
               if The_Client.Tracker_User = No_Detached_User then
                  Send(The_Client, Err_Invite_Only_Chan & " " & The_Client.Nick.Element & " " &
                         Channel_Name & " :You need to be logged in to join this channel");
                  return;
               end if;
            end if;

            Channels(Channel_Name).Users.Include(The_Client.Id);

            for User of Channels(Channel_Name).Users loop
               Send(Clients(User), "JOIN " & Channel_Name, From => The_Client.Nick.Element);
            end loop;
         else
            if The_Client.Tracker_User = No_Detached_User or else The_Client.Tracker_User.Role /= 1 then
               Send(The_Client, Err_No_Such_Channel & " " & The_Client.Nick.Element & " " &
                      Channel_Name & " :The channel does not exist, only admins can create new channels");
               return;
            end if;

            declare
               New_Channel : Channel;
            begin
               New_Channel.Name := To_Holder(Channel_Name);
               New_Channel.Users.Include(The_Client.Id);

               Channels.Include(Channel_Name, New_Channel);

               Persist_Channel(New_Channel);
            end;
         end if;

         Send_Topic(The_Client, Channel_Name);
         Send_Names(The_Client, Channel_Name);
      end Join_Channel;

      procedure Send_Whois(The_Client : Client; Message_Parts : String_Vectors.Vector) is
         User_Slices : Slice_Set;
      begin
         Create(User_Slices, Message_Parts(1), ",");
         for Whois_Username of User_Slices loop
            if Users.Contains(Whois_Username) then
               declare
                  The_User : Client_Cref := Clients(Users(Whois_Username));
               begin
                  Send(The_Client, Rpl_Whois_User
                         & " " & The_Client.Nick.Element
                         & " " & The_User.Nick.Element
                         & " " & The_User.Username.Element
                         & " " & Irc_Host.Element & " * :" & The_User.Real_Name.Element);

                  if not The_User.Away_Message.Is_Empty then
                     Send(The_Client, Rpl_Away & " " & The_Client.Nick.Element & " " & The_User.Nick.Element
                            & " " & The_User.Away_Message.Element);
                  end if;
                  if The_User.Is_Ssl then
                     Send(The_Client, Rpl_Whois_Secure & " " & The_Client.Nick.Element & " " &
                            The_User.Nick.Element & " :is using a secure connection");
                  end if;
                  if The_User.Tracker_User /= No_Detached_User then
                     Send(The_Client, Rpl_Whois_Account & " " & The_Client.Nick.Element & " " &
                            The_User.Nick.Element & " " & The_User.Tracker_User.Username & " :is logged in as");

                     if The_User.Tracker_User.Role = 1 then
                        Send(The_Client, Rpl_Whois_Operator & " " & The_Client.Nick.Element & " " &
                               The_User.Nick.Element & " :is an IRC operator");
                     end if;
                  end if;
                  if The_Client.Id = The_User.Id or (The_Client.Tracker_User /= No_Detached_User and then The_Client.Tracker_User.Role = 1) then
                     Send(The_Client, Rpl_Whois_Actually & " " & The_Client.Nick.Element & " " &
                            The_User.Nick.Element & " " & Image(The_User.Address) & " :is actually using host");
                  end if;

                  Send(The_Client, Rpl_End_Of_Whois & " :End of WHOIS");
               end;
            else
               Send(The_Client, Err_No_Such_Nick & " " & The_Client.Nick.Element & " " & Whois_Username & " :No such nickname");
            end if;
         end loop;
      end;

      procedure Send_Topic(The_Client : Client; Channel_Name : String; From : String := Irc_Host.Element) is
         Channel : Channel_Maps.Constant_Reference_Type := Channels(Channel_Name);
      begin
         if Channel.Topic.Is_Empty then
            Send(The_Client, Rpl_Notopic & " " & The_Client.Nick.Element & " " & Channel.Name.Element & " :No topic",
                 From => From);
         else
            Send(The_Client, Rpl_Topic & " " & The_Client.Nick.Element & " " & Channel.Name.Element & " :" & Channel.Topic.Element,
                 From => From);
            Send(The_Client, Rpl_Topic_Who_Time & " " & The_Client.Nick.Element & " " & Channel.Name.Element & " " & Channel.Topic_Set_By.Element
                   & Channel.Topic_Set_At'Image, From => From);
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
                  if Client.Tracker_User /= No_Detached_User and then Client.Tracker_User.Role = 1 then
                     User_Str := @ & "@";
                  end if;
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

      procedure Send_List(The_Client : Client; Channel_Name : String) is
         The_Channel : Channel := Channels(Channel_Name);
      begin
         Send(The_Client, Rpl_List & " " & The_Client.Nick.Element & " " & Channel_Name
                & Length(The_Channel.Users)'Image & (if The_Channel.Topic.Is_Empty
                                                     then " :"
                                                     else " :" & The_Channel.Topic.Element));
      end;

      procedure Special_Message(The_Client : in out Client; Message : String) is
         Login_Matcher : constant Pattern_Matcher := Compile(":login (\S+) (.+)", Case_Insensitive);
         Memo_Matcher : constant Pattern_Matcher := Compile(":memo (\S+) (.+)", Case_Insensitive);
         Help_Matcher : constant Pattern_Matcher := Compile(":help", Case_Insensitive);

         Matches : Match_Array (0..2);
      begin
         Match(Login_Matcher, Message, Matches);
         if Matches(1) /= No_Match and Matches(2) /= No_Match then
            declare
               Name : String := Message(Matches(1).First..Matches(1).Last);
               Key : String := Message(Matches(2).First..Matches(2).Last);
               The_User : Detached_User'Class := Database.Get_User(Name);
            begin
               if The_User = Detached_User'Class(No_Detached_User) then
                  Send(The_Client, "NOTICE " & The_Client.Nick.Element &
                         " :Nickname " & Name & " not registered on the tracker", From => "hellish");
                  return;
               end if;

               declare
                  use Gnatcoll.Json;

                  Profile_Json : Json_Value := Read(The_User.Profile);
                  Irc_Key : String := (if Has_Field(Profile_Json, "irc_key")
                                       then Get(Profile_Json, "irc_key")
                                       else "");
               begin
                  if Irc_Key = "" then
                     Send(The_Client, "NOTICE " & The_Client.Nick.Element &
                            " :IRC key for the user " & Name & " is unset", From => "hellish");
                     return;
                  end if;

                  if Irc_Key = Key then
                     Send(The_Client, "NOTICE " & The_Client.Nick.Element & " :Logged in as " & Name & "!", From => "hellish");
                     The_Client.Tracker_User := Detached_User(The_User);
                  else
                     Send(The_Client, "NOTICE " & The_Client.Nick.Element & " :Invalid login key for " & Name, From => "hellish");
                  end if;
               end;
            end;

            return;
         end if;

         Match(Memo_Matcher, Message, Matches);
         if Matches(1) /= No_Match and Matches(2) /= No_Match then
            if The_Client.Tracker_User = No_Detached_User then
               Send(The_Client, "NOTICE " & The_Client.Nick.Element & " :Only logged in users can send memos", From => "hellish");
               return;
            end if;
            declare
               Name : String := Message(Matches(1).First..Matches(1).Last);
               Memo : String := Message(Matches(2).First..Matches(2).Last);

               The_User : Detached_User := Detached_User(Database.Get_User(Name));
            begin
               if The_User = No_Detached_User then
                  Send(The_Client, "NOTICE " & The_Client.Nick.Element & " :There is no user named " & Name, From => "hellish");
                  return;
               end if;

               Database.Notify_User(The_User, "You have a new IRC memo from [" & The_Client.Tracker_User.Username & "](/profile/" &
                                      The_Client.Tracker_User.Username & "): " & Memo);
               Send(The_Client, "NOTICE " & The_Client.Nick.Element & " :Memo sent!", From => "hellish");
            end;

            return;
         end if;

         Match(Help_Matcher, Message, Matches);
         if Matches(0) /= No_Match then
            Send(The_Client, "NOTICE " & The_Client.Nick.Element & Text_Bold & " :Help for special commands" & Text_Bold,
                 From => "hellish");
            Send(The_Client, "NOTICE " & The_Client.Nick.Element & " :"
                   & Text_Bold & "login " & Text_Italic & "<username> <password>" & Text_Italic & Text_Bold
                   & " Login with your tracker username and irc key",
                 From => "hellish");
            Send(The_Client, "NOTICE " & The_Client.Nick.Element & " :"
                   & Text_Bold & "memo " & Text_Italic & "<username> <memo>" & Text_Italic & Text_Bold
                   & " Leave a memo for a tracker user, they will see it as a notification",
                 From => "hellish");
            return;
         end if;

         Send(The_Client, "NOTICE " & The_Client.Nick.Element & " :Unknown command or wrong arguments: "
                & Text_Bold & Message(Message'First + 1..Message'Last) & Text_Bold
                & ", use " & Text_Bold & "help" & Text_Bold & " to see available commands",
              From => "hellish");
      end;

      procedure Persist_Channel(The_Channel : Channel) is
         use Gnatcoll.Json;

         Channel_Map : Json_Value := Create_Object;
         Channel_Modes : Json_Array := Empty_Array;

         Topic_Map : Json_Value := Create_Object;
      begin
         for Mode of The_Channel.Modes loop
            Append(Channel_Modes, Create(Mode));
         end loop;
         Channel_Map.Set_Field("modes", Create(Channel_Modes));
         if not The_Channel.Topic.Is_Empty then
            Topic_Map.Set_Field("topic", Create(The_Channel.Topic.Element));
            Topic_Map.Set_Field("set_by", Create(The_Channel.Topic_Set_By.Element));
            Topic_Map.Set_Field("set_at", Create(The_Channel.Topic_Set_At));
            Channel_Map.Set_Field("topic", Topic_Map);
         end if;

         Database.Persist_Channel(The_Channel.Name.Element, Channel_Map.Write);
      end Persist_Channel;

      procedure Load_Persisted_Channels is
         Loaded_Channel : Irc_Channel;
         Loaded_Channels : Irc_Channel_List := Database.Persisted_Channels;
      begin
         while Loaded_Channels.Has_Row loop
            Loaded_Channel := Loaded_Channels.Element;

            declare
               use Gnatcoll.Json;
               Channel_Data : Json_Value := Read(Orm.Data(Loaded_Channel));
               Channel_Modes : Json_Array := Get(Channel_Data, "modes");
               Topic_Data : Json_Value := Get(Channel_Data, "topic");

               New_Channel : Channel;
            begin
               New_Channel.Name := To_Holder(Loaded_Channel.Name);
               for Mode of Channel_Modes loop
                  New_Channel.Modes.Include(Get(Mode));
               end loop;
               if Channel_Data.Has_Field("topic") then
                  New_Channel.Topic := To_Holder(Get(Topic_Data, "topic"));
                  New_Channel.Topic_Set_By := To_Holder(Get(Topic_Data, "set_by"));
                  New_Channel.Topic_Set_At := Get(Topic_Data, "set_at");
               end if;

               Channels.Include(Loaded_Channel.Name, New_Channel);
            end;

            Loaded_Channels.Next;
         end loop;
      end Load_Persisted_Channels;
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

   task body Accept_Connections_Ssl is
   begin
      -- Wait for socket open
      accept Start;

      loop
         declare
            use Ssl;

            The_Client : Client (Is_Ssl => True);
            Status : Selector_Status;
         begin
            Accept_Socket(Socket_Ssl, The_Client.Socket, The_Client.Address,
                          Timeout => Forever, Status => Status);
            if Status = Completed then
               The_Client.Socket_Ssl := Create_Ssl;
               Set_Ssl_Fd(The_Client.Socket_Ssl, The_Client.Socket);
               Accept_Ssl(The_Client.Socket_Ssl);

               Protected_Clients.Append(The_Client);
            end if;
         exception
            when E : others =>
               Put_Line(Exception_Information(E));

               Ssl.Free_Ssl(The_Client.Socket_Ssl);
               Close_Socket(The_Client.Socket);
         end;
      end loop;
   end Accept_Connections_Ssl;

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
         Put_Line(Exception_Information(E));
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

      Protected_Clients.Load_Persisted_Channels;
      Accept_Connections.Start;
      Process_Connections.Start;

      Put_Line("Started IRC server on " & Irc_Host.Element & ":" & Trim(Port'Image, Ada.Strings.Left));

      if not Ssl_Cert_Path.Is_Empty and not Ssl_Privkey_Path.Is_Empty then
         -- SSL
         declare
            Addr_Ssl : Sock_Addr_Type;
         begin
            Ssl.Initialize(Ssl_Cert_Path.Element, Ssl_Privkey_Path.Element);

            Addr_Ssl.Addr := (Family => Family_Inet, Sin_V4 => (others => 0));
            Addr_Ssl.Port := Port_Type(Port_Ssl);

            Create_Socket(Socket_Ssl);
            Set_Socket_Option(Socket_Ssl, Socket_Level, (Reuse_Address, True));
            Bind_Socket(Socket_Ssl, Addr_Ssl);
            Listen_Socket(Socket_Ssl, Queue_Size);

            Set_Specific_Handler(Accept_Connections_Ssl'Identity, Termination_Handler.Handler'Access);
            Accept_Connections_Ssl.Start;

            Put_Line("Started SSL IRC server on " & Irc_Host.Element & ":" & Trim(Port_Ssl'Image, Ada.Strings.Left));
         end;
      end if;
   end Start;
end Hellish_Irc;
