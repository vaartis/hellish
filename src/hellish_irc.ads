pragma Unsuppress(All_Checks);

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with
  Ada.Strings.Hash,
  Ada.Strings.Less_Case_Insensitive;
with Ada.Containers.Vectors;
with Ada.Containers.Indefinite_Vectors;
with Ada.Containers.Indefinite_Hashed_Sets;
with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Indefinite_Holders;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Containers; use Ada.Containers;
with Ada.Characters.Latin_1; use Ada.Characters;
with Ada.Synchronous_Task_Control; use Ada.Synchronous_Task_Control;

with System;

with Orm; use Orm;

with Gnat.Sockets; use Gnat.Sockets;

package Hellish_Irc is
   procedure Start;

   Port : Natural := 16697;
   Port_Ssl : Natural := 16698;
   Queue_Size : Positive := 10;
   History_Size : Positive := 50;

   package String_Holders is new Ada.Containers.Indefinite_Holders(String);
   use String_Holders;

   Ssl_Cert_Path : String_Holders.Holder;
   Ssl_Privkey_Path : String_Holders.Holder;

   Irc_Host : String_Holders.Holder;
private
   package Ssl is
      Ssl_Error : exception;

      procedure Initialize(Cert, Key : String);
      procedure Reload(Cert, Key : String);

      type Ssl is new System.Address;
      function Create_Ssl return Ssl;
      procedure Set_Ssl_Fd(The_Ssl : Ssl; Socket : Socket_Type);
      procedure Accept_Ssl(The_Ssl : Ssl);
      function Send_Ssl(The_Ssl : Ssl; Reply : String) return Integer;
      function Receive_Ssl(The_Ssl : Ssl; Max : Natural; Amount : out Integer) return String;
      procedure Free_Ssl(The_Ssl : Ssl);
   end Ssl;

   Socket : Socket_Type;
   Socket_Ssl : Socket_Type;
   Selector : Selector_Type;

   task Accept_Connections_Ssl is
      entry Start;
   end Accept_Connections_Ssl;
   task Accept_Connections is
      entry Start;
   end Accept_Connections;
   task Process_Connections is
      entry Start;
   end Process_Connections;
   task Link_Preview;

   Link_Preview_Suspension_Obj : Suspension_Object;

   type Link_Preview_Request is record
      Channel, Link : String_Holders.Holder;
   end record;
   package Link_Preview_Vectors is new Ada.Containers.Vectors (Index_Type => Natural, Element_Type => Link_Preview_Request);
   use Link_Preview_Vectors;

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
   package User_Maps is new Ada.Containers.Indefinite_Ordered_Maps(Key_Type => String, Element_Type => Natural,
                                                                   "<" => Ada.Strings.Less_Case_Insensitive);
   use User_Maps;

   package String_Sets is new Ada.Containers.Indefinite_Hashed_Sets(Element_Type => String,
                                                                    Hash => Ada.Strings.Hash, Equivalent_Elements => "=");

   type History_Entry is record
      Sent : String_Holders.Holder;
      Sender : String_Holders.Holder;
      Message: String_Holders.Holder;
   end record;
   package History_Vectors is new Ada.Containers.Vectors(Index_Type => Natural, Element_Type => History_Entry);
   use History_Vectors;

   type Client ( Is_Ssl : Boolean := False ) is record
      Socket : Socket_Type;

      Id : Natural;
      Address : Sock_Addr_Type;

      Unfinished_Messages : Unbounded_String;
      Message_Queue : Message_Vectors.Vector;

      Nick : String_Holders.Holder;
      Username : String_Holders.Holder;
      Real_Name : String_Holders.Holder;
      Tracker_User : Detached_User;

      Caps_Negotiated : Boolean := True;
      Motd_Sent : Boolean := False;

      Away_Message : String_Holders.Holder;

      Joined_Channels : String_Sets.Set;

      Server_Time_Supported : Boolean := False;

      case Is_Ssl is
         when True =>
            Socket_Ssl : Ssl.Ssl;
         when False => null;
      end case;
   end record;

   type Channel is record
      Name : String_Holders.Holder;

      Topic : String_Holders.Holder;
      Topic_Set_By : String_Holders.Holder;
      Topic_Set_At : Positive;

      Modes : String_Sets.Set;

      Users : User_Hashed_Sets.Set;

      History : History_Vectors.Vector;
   end record;

   -- Channel name to channel
   package Channel_Maps is new Ada.Containers.Indefinite_Ordered_Maps(Key_Type => String, Element_Type => Channel,
                                                                      "<" => Ada.Strings.Less_Case_Insensitive);
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

      procedure Process_Clients(R_Selector_Set : in out Socket_Set_Type);
      procedure Load_Persisted_Channels;
      procedure Send_Special_Message(To, Message : String);

      function R_Selector_Set return Socket_Set_Type;
   private
      procedure Process_Message_Queues;
      procedure Process_Select_Connections(R_Selector_Set : in out Socket_Set_Type);

      procedure Remove(To_Remove : User_Hashed_Sets.Set);

      procedure Send(The_Client : Client; Message : String; From : String := Irc_Host.Element; Tag : String := "");

      procedure Send_Whois(The_Client : Client; Message_Parts : String_Vectors.Vector);
      procedure Send_Topic(The_Client : Client; Channel_Name : String; From : String := Irc_Host.Element);
      procedure Send_Names(The_Client : Client; Channel_Name : String);
      procedure Send_List(The_Client : Client; Channel_Name : String);

      procedure Join_Channel(The_Client : in out Client; Channel_Name : String);
      procedure Leave_Channel(The_Client : in out Client; The_Channel : in out Channel);

      procedure Special_Message(The_Client : in out Client; Message : String);

      procedure Persist_Channel(The_Channel : Channel);

      Next_Id : Natural := 0;

      Clients : Client_Maps.Map;
      Socket_To_Client : Socket_Client_Maps.Map;
   end Protected_Clients;

   function Client_From(C : Client) return String is (C.Nick.Element & "!" & C.Username.Element & "@" & "unknown.host");

   Channels : Channel_Maps.Map;
   Users : User_Maps.Map;

   Link_Preview_Queue : Link_Preview_Vectors.Vector;

   Cr_Lf : constant String := Latin_1.Cr & Latin_1.Lf;

   Rpl_Motd_Start : String := "375";
   Rpl_Motd : String := "372";
   Rpl_End_Of_Motd : String := "376";

   Rpl_Whois_User : String := "311";
   Rpl_End_Of_Whois : String := "318";
   Rpl_Notopic : String := "331";
   Rpl_Topic : String := "332";
   Rpl_Topic_Who_Time : String := "333";
   Rpl_Name_Reply : String := "353";
   Rpl_End_Of_Names : String := "366";
   Rpl_Channel_Mode_Is : String := "324";
   Rpl_List_Start : String := "321";
   Rpl_List : String := "322";
   Rpl_List_End : String := "323";
   Rpl_Now_Away : String := "306";
   Rpl_Unaway : String := "305";
   Rpl_Away : String := "301";
   Rpl_Whois_Secure : String := "671";
   Rpl_Whois_Account : String := "330";
   Rpl_Whois_Operator : String := "313";
   Rpl_Whois_Actually : String := "338";
   Rpl_Whois_Channels : String := "319";
   Rpl_Who_Reply : String := "352";
   Rpl_End_Of_Who : String := "315";
   Rpl_Banlist : String := "367";
   Rpl_End_Of_Banlist : String := "368";

   Err_Unknown_Error : String := "400";
   Err_No_Such_Nick : String := "401";
   Err_Nickname_In_Use : String := "433";
   Err_No_Such_Channel : String := "403";
   Err_Chan_Op_Privs_Needed : String := "482";
   Err_Not_On_Channel : String := "442";
   Err_No_Op_Rivileges : String := "481";

   Err_Invite_Only_Chan : String := "473";

   Text_Bold : Character := Character'Val(16#02#);
   Text_Italic : Character := Character'Val(16#1D#);
end Hellish_Irc;
