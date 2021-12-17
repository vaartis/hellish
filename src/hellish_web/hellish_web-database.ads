with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Gnatcoll.Sql.Exec; use Gnatcoll.Sql.Exec;
with Gnatcoll.Sql.Postgres; use Gnatcoll.Sql.Postgres;
with Gnatcoll.Sql.Sessions; use Gnatcoll.Sql.Sessions;

with Orm; use Orm;

package Hellish_Web.Database is
   Db_Desc : Database_Description := Setup("hellish", User => "postgres");

   procedure Init;

   function Create_User(Name, Password : String) return Boolean;
   function Verify_User_Credentials(Name, Password : String) return Boolean;
   function Get_User(Name : String) return Detached_User'Class;
   function Get_User_By_Passkey(Passkey : String) return Detached_User'Class;

   procedure Create_Torrent(Username, Info_Hash : String);
   procedure Update_Torrent_Up_Down(User : Detached_User'Class; Info_Hash : String;
                                    Uploaded_Diff : Natural; Downloaded_Diff : Natural);
end;
