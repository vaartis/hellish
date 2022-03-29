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

   procedure Create_Torrent(The_Torrent : in out Detached_Torrent'Class);
   procedure Update_Torrent_Up_Down(User : Detached_User'Class; Info_Hash : String;
                                    Uploaded_Diff : Long_Long_Integer; Downloaded_Diff : Long_Long_Integer);
   function Get_Torrent_By_Hash(Info_Hash : String; Session : Session_Type := Get_New_Session) return Detached_Torrent'Class;
   function Get_Torrent(Id : Natural) return Detached_Torrent'Class;
   procedure Snatch_Torrent(Info_Hash : String);
   function Search_Torrents(Query : String;
                            Uploader : Natural;

                            Offset : Natural;
                            Limit : Natural;
                            Total_Count : out Natural) return Torrent_List;

   function Create_Invite(From_User : Detached_User'Class) return String;
   function Invite_Valid(Invite : String) return Boolean;
   procedure Invite_Use(Invite : String; Invited_User : Detached_User'Class);
end;
