with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Gnatcoll.Sql.Exec; use Gnatcoll.Sql.Exec;
with Gnatcoll.Sql.Postgres; use Gnatcoll.Sql.Postgres;
with Gnatcoll.Sql.Sessions; use Gnatcoll.Sql.Sessions;

with Orm; use Orm;

package Hellish_Web.Database is
   Db_Desc : Database_Description := Setup("hellish", User => "postgres");

   procedure Init;

   function Create_User(Name, Password : String; Created_User : out Detached_User'Class) return Boolean;
   function Verify_User_Credentials(Name, Password : String) return Boolean;
   function Get_User(Name : String) return Detached_User'Class;
   function Get_User(Id : Natural) return Detached_User'Class;
   function User_Exists(Name : String; Session : Session_Type := Get_New_Session) return Boolean;
   function Get_User_By_Passkey(Passkey : String) return Detached_User'Class;
   function Get_Invited_Users(By_User : Detached_User'Class) return Invite_List;
   procedure Update_User(The_User : Detached_User'Class);

   procedure Create_Torrent(The_Torrent : in out Detached_Torrent'Class);
   procedure Update_Torrent_Up_Down(User : Detached_User'Class; Info_Hash : String;
                                    Uploaded_Diff : Long_Long_Integer; Downloaded_Diff : Long_Long_Integer);
   function Get_Torrent_By_Hash(Info_Hash : String; Session : Session_Type := Get_New_Session) return Detached_Torrent'Class;
   function Get_Torrent(Id : Natural) return Detached_Torrent'Class;
   procedure Snatch_Torrent(Info_Hash : String; The_User : Detached_User'Class);
   function Torrent_Snatches(The_Torrent : Detached_Torrent'Class) return Integer;
   function Search_Torrents(Query : String;
                            Uploader : Natural;
                            Category : Integer;
                            Snatched_By : Integer;

                            Offset : Natural;
                            Limit : Natural;
                            Total_Count : out Natural) return Direct_Torrent_List;
   procedure Delete_Torrent(Id : Natural);
   function Torrent_Comments(Parent_Torrent : Integer;
                             Offset : Natural;
                             Limit : Integer;
                             Total_Count : out Natural) return Post_List;

   function Get_User_Stats_For_Torrent(User: Detached_User'Class; Torrent: Detached_Torrent'Class)
                                      return Detached_User_Torrent_Stat'Class;

   function Create_Invite(From_User : Detached_User'Class) return String;
   function Invite_Valid(Invite : String) return Boolean;
   procedure Invite_Use(Invite : String; Invited_User : Detached_User'Class);

   procedure Create_Post(The_Post : in out Detached_Post'Class);
   function Get_Post(Id : Natural; Parent_Post : out Detached_Post'Class) return Detached_Post'Class;
   function Post_Replies(Parent_Post : Integer;
                         Offset : Natural;
                         Limit : Integer;
                         Total_Count : out Natural) return Post_List;
   function Get_Latest_News return Detached_Post'Class;
   function Search_Posts(Query : String;
                         Flag : Integer;
                         Author : Integer;

                         Offset : Natural;
                         Limit : Natural;
                         Total_Count : out Natural) return Post_List;

   procedure Persist_Peers(Info_Hash : String; Data : String);
   function Persisted_Peers return Peer_Data_List;

   function Add_Uploaded_Image(Username : String; Filename : String) return Detached_Image_Upload'Class;
   function Delete_Uploaded_Image(Id : Integer) return Boolean;
   function User_Images(Username : String) return Image_Upload_List;
   function Get_Image(Id : Natural) return Detached_Image_Upload'Class;
end Hellish_Web.Database;
