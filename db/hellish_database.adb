with GNATCOLL.SQL.Inspect; use GNATCOLL.SQL.Inspect;
with GNATCOLL.SQL.Exec; use GNATCOLL.SQL.Exec;
package body Hellish_Database is
   pragma Style_Checks (Off);

   function FK (Self : T_Image_Uploads'Class; Foreign : T_Users'Class) return SQL_Criteria is
   begin
      return Self.By_User = Foreign.Id;
   end FK;

   function FK (Self : T_Peer_Data'Class; Foreign : T_Torrents'Class) return SQL_Criteria is
   begin
      return Self.Torrent_Id = Foreign.Id;
   end FK;

   function FK (Self : T_Posts'Class; Foreign : T_Users'Class) return SQL_Criteria is
   begin
      return Self.By_User = Foreign.Id;
   end FK;

   function FK (Self : T_Posts'Class; Foreign : T_Posts'Class) return SQL_Criteria is
   begin
      return Self.Parent_Post = Foreign.Id;
   end FK;

   function FK (Self : T_Posts'Class; Foreign : T_Torrents'Class) return SQL_Criteria is
   begin
      return Self.Parent_Torrent = Foreign.Id;
   end FK;

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
         & "|created_by|FK users|NOT NULL,INDEX||" & ASCII.LF
         & "|display_name|Text|NOT NULL||" & ASCII.LF
         & "|description|Text|NOT NULL||" & ASCII.LF
         & "|category|Integer|NOT NULL,INDEX|0|" & ASCII.LF
         & "" & ASCII.LF
         & "|TABLE| users" & ASCII.LF
         & "|id|AUTOINCREMENT|PK||" & ASCII.LF
         & "|username|Text|NOT NULL,INDEX,UNIQUE||" & ASCII.LF
         & "|password|Text|NOT NULL||" & ASCII.LF
         & "|passkey|Text|NOT NULL,INDEX,UNIQUE||" & ASCII.LF
         & "|uploaded|bigint||0|" & ASCII.LF
         & "|downloaded|bigint||0|" & ASCII.LF
         & "|role|Integer|NOT NULL|0|" & ASCII.LF
         & "" & ASCII.LF
         & "|TABLE| config" & ASCII.LF
         & "|id|AUTOINCREMENT|PK||" & ASCII.LF
         & "|version|Integer|NOT NULL||" & ASCII.LF
         & "" & ASCII.LF
         & "|TABLE| user_torrent_stats" & ASCII.LF
         & "|by_user|FK users|NOT NULL||" & ASCII.LF
         & "|of_torrent|FK torrents|NOT NULL||" & ASCII.LF
         & "|uploaded|bigint|||" & ASCII.LF
         & "|downloaded|bigint|||" & ASCII.LF
         & "|snatched|boolean|NOT NULL|FALSE|" & ASCII.LF
         & "|UNIQUE:|by_user,of_torrent|user_torrent_unique" & ASCII.LF
         & "" & ASCII.LF
         & "|TABLE| invites" & ASCII.LF
         & "|id|AUTOINCREMENT|PK||" & ASCII.LF
         & "|value|Text|NOT NULL,INDEX,UNIQUE||" & ASCII.LF
         & "|activated|boolean|NOT NULL||" & ASCII.LF
         & "|by_user|FK users|NOT NULL||" & ASCII.LF
         & "|for_user|FK users|,UNIQUE||" & ASCII.LF
         & "" & ASCII.LF
         & "|TABLE| posts" & ASCII.LF
         & "|id|AUTOINCREMENT|PK||" & ASCII.LF
         & "|title|Text|||" & ASCII.LF
         & "|content|Text|NOT NULL||" & ASCII.LF
         & "|by_user|FK users|NOT NULL||" & ASCII.LF
         & "|parent_post|FK posts|||" & ASCII.LF
         & "|flag|Integer|NOT NULL|0|" & ASCII.LF
         & "|parent_torrent|FK torrents|||" & ASCII.LF
         & "" & ASCII.LF
         & "|TABLE| peer_data" & ASCII.LF
         & "|torrent_id|FK torrents|PK||" & ASCII.LF
         & "|data|json|||" & ASCII.LF
         & "" & ASCII.LF
         & "|TABLE| image_uploads" & ASCII.LF
         & "|id|AUTOINCREMENT|PK||" & ASCII.LF
         & "|by_user|FK users|NOT NULL||" & ASCII.LF
         & "|filename|Text|NOT NULL,INDEX,UNIQUE||" & ASCII.LF
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
