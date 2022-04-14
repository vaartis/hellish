with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Hash;
with Ada.Containers.Vectors;
with Ada.Containers.Indefinite_Vectors;
with Ada.Containers.Indefinite_Hashed_Sets;
with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Indefinite_Holders;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Containers; use Ada.Containers;
with Ada.Characters.Latin_1; use Ada.Characters;

with Orm; use Orm;

with Gnat.Sockets; use Gnat.Sockets;

package Hellish_Irc is
   procedure Start;
   
   Port : Natural := 16697;
   Queue_Size : Positive := 10;
private   
   Socket : Socket_Type;
   
   package String_Holders is new Ada.Containers.Indefinite_Holders(String);
   use String_Holders;
   
   Irc_Host : String_Holders.Holder;
   
   task Accept_Connections is 
      entry Start;
   end Accept_Connections;
   task Process_Connections is
      entry Start;
   end Process_Connections;
   
   -- The parts that make up the message
   package String_Vectors is new Ada.Containers.Indefinite_Vectors(Index_Type => Natural, Element_Type => String);
   -- The message queue of the client
   package Message_Vectors is new Ada.Containers.Vectors(Index_Type => Natural, Element_Type => String_Vectors.Vector, 
                                                         "=" => String_Vectors."=");
   use String_Vectors, Message_Vectors;
   
   function Join_Parts(Parts : String_Vectors.Vector) return String;
   
   function Hash(X : Natural) return Hash_Type is (Hash_Type(X));
   -- Set of IDs
   package User_Hashed_Sets is new Ada.Containers.Indefinite_Hashed_Sets(Element_Type => Natural,
                                                                         Hash => Hash, Equivalent_Elements => "=");
   use User_Hashed_Sets;
   -- Username to ID
   package User_Maps is new Ada.Containers.Indefinite_Ordered_Maps(Key_Type => String, Element_Type => Natural);
   use User_Maps;
   
   package String_Sets is new Ada.Containers.Indefinite_Hashed_Sets(Element_Type => String,
                                                                    Hash => Ada.Strings.Hash, Equivalent_Elements => "=");   
   
   type Client is record
      Socket : Socket_Type;
      Id : Natural;
      Address : Sock_Addr_Type;

      Unfinished_Messages : Unbounded_String;
      Message_Queue : Message_Vectors.Vector;
      
      Nick : String_Holders.Holder;
      Username : String_Holders.Holder;
      Tracker_User : Detached_User;
      
      Caps_Negotiated : Boolean := True;
      Motd_Sent : Boolean := False;
   end record;  
   
   type Channel is record
      Name : String_Holders.Holder;
      Topic : String_Holders.Holder;
      
      Modes : String_Sets.Set;
      
      Users : User_Hashed_Sets.Set;
   end record;   
   
   -- Channel name to channel
   package Channel_Maps is new Ada.Containers.Indefinite_Ordered_Maps(Key_Type => String, Element_Type => Channel);
   use Channel_Maps;
   
   -- Client ID to client
   package Client_Maps is new Ada.Containers.Indefinite_Ordered_Maps(Key_Type => Natural, Element_Type => Client);
   use Client_Maps;
   subtype Client_Cref is Client_Maps.Constant_Reference_Type;
   
   function Hash(X : Socket_Type) return Hash_Type is (Hash_Type(To_C(X)));
   package Socket_Client_Maps is new Ada.Containers.Indefinite_Hashed_Maps(Key_Type => Socket_Type, Element_Type => Natural, 
                                                                           Hash => Hash, Equivalent_Keys => "=");
   use Socket_Client_Maps;

   type Iterate_Procedure is access procedure (Client_Ref : Client_Maps.Reference_Type;
                                               Remove_On_Exit : in out Boolean);
   protected Protected_Clients is
      procedure Append(The_Client : in out Client);
      
      procedure Process_Clients;
   private
      procedure Process_Message_Queues;
      procedure Process_Select_Connections;
      
      procedure Remove(To_Remove : User_Hashed_Sets.Set);
      
      procedure Send(The_Client : Client; Message : String; From : String := Irc_Host.Element);
   
      procedure Send_Topic(The_Client : Client; Channel_Name : String);
      procedure Send_Names(The_Client : Client; Channel_Name : String);
      procedure Join_Channel(The_Client : Client; Channel_Name : String);
      
      procedure Special_Message(The_Client : in out Client; Message : String);
      
      Next_Id : Natural := 0;

      Clients : Client_Maps.Map;
      Socket_To_Client : Socket_Client_Maps.Map;
   end Protected_Clients;
   
   Channels : Channel_Maps.Map;
   Users : User_Maps.Map;
   
   Cr_Lf : constant String := Latin_1.Cr & Latin_1.Lf;
   
   Rpl_Motd_Start : String := "375";
   Rpl_Motd : String := "372";
   Rpl_End_Of_Motd : String := "376";
   
   Rpl_Whois_User : String := "311";
   Rpl_End_Of_Whois : String := "318";
   Rpl_Notopic : String := "331";
   Rpl_Topic : String := "332";
   Rpl_Name_Reply : String := "353";
   Rpl_End_Of_Names : String := "366";
   Rpl_Channel_Mode_Is : String := "324";
   
   Err_No_Such_Nick : String := "401";
   Err_Nickname_In_Use : String := "433";
   Err_No_Such_Channel : String := "403";
   Err_Chan_Op_Privs_Needed : String := "482";
   
   Err_Invite_Only_Chan : String := "473";
end Hellish_Irc;