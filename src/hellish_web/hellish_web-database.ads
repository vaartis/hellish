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
   function Get_User(Name : String; Session : Session_Type := Get_New_Session) return Detached_User'Class;

   procedure Create_Torrent(Filename, Username, Torrent_File : String);
end;
