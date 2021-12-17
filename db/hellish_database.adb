with GNATCOLL.SQL.Inspect; use GNATCOLL.SQL.Inspect;
with GNATCOLL.SQL.Exec; use GNATCOLL.SQL.Exec;
package body Hellish_Database is
   pragma Style_Checks (Off);

   function FK (Self : T_Torrents'Class; Foreign : T_Users'Class) return SQL_Criteria is
   begin
      return Self.Created_By = Foreign.Id;
   end FK;

   function FK (Self : T_User_Torrent_Stats'Class; Foreign : T_Users'Class) return SQL_Criteria is
   begin
      return Self.By_User = Foreign.Id;
   end FK;

   function FK (Self : T_User_Torrent_Stats'Class; Foreign : T_Torrents'Class) return SQL_Criteria is
   begin
      return Self.Of_Torrent = Foreign.Id;
   end FK;

   procedure Create_Database
      (DB : not null access GNATCOLL.SQL.Exec.Database_Connection_Record'Class)
   is
      DbSchema : constant String := "|TABLE| torrents" & ASCII.LF
         & "|id|AUTOINCREMENT|PK||" & ASCII.LF
         & "|info_hash|Text|NOT NULL,INDEX,UNIQUE||" & ASCII.LF
         & "|created_by|FK users|NOT NULL||" & ASCII.LF
         & "" & ASCII.LF
         & "|TABLE| users" & ASCII.LF
         & "|id|AUTOINCREMENT|PK||" & ASCII.LF
         & "|username|Text|NOT NULL,INDEX,UNIQUE||" & ASCII.LF
         & "|password|Text|NOT NULL||" & ASCII.LF
         & "|passkey|Text|NOT NULL,INDEX,UNIQUE||" & ASCII.LF
         & "|uploaded|Integer||0|" & ASCII.LF
         & "|downloaded|Integer||0|" & ASCII.LF
         & "" & ASCII.LF
         & "|TABLE| config" & ASCII.LF
         & "|id|AUTOINCREMENT|PK||" & ASCII.LF
         & "|version|Integer|NOT NULL||" & ASCII.LF
         & "" & ASCII.LF
         & "|TABLE| user_torrent_stats" & ASCII.LF
         & "|by_user|FK users|NOT NULL||" & ASCII.LF
         & "|of_torrent|FK torrents|NOT NULL||" & ASCII.LF
         & "|uploaded|Integer|||" & ASCII.LF
         & "|downloaded|Integer|||" & ASCII.LF
         & "|UNIQUE:|by_user,of_torrent|user_torrent_unique" & ASCII.LF
         & "" & ASCII.LF
         & "";
      F : File_Schema_IO;
      D : DB_Schema_IO;
      Schema : DB_Schema;
   begin
      Schema := Read_Schema (F, DbSchema);
      D.DB := Database_Connection (DB);
      Write_Schema (D, Schema);
   end Create_Database;
end Hellish_Database;
