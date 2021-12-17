with GNATCOLL.SQL.Inspect; use GNATCOLL.SQL.Inspect;
with GNATCOLL.SQL.Exec; use GNATCOLL.SQL.Exec;
package body Hellish_Database is
   pragma Style_Checks (Off);

   function FK (Self : T_Torrents'Class; Foreign : T_Users'Class) return SQL_Criteria is
   begin
      return Self.Created_By = Foreign.Id;
   end FK;

   procedure Create_Database
      (DB : not null access GNATCOLL.SQL.Exec.Database_Connection_Record'Class)
   is
      DbSchema : constant String := "|TABLE| torrents" & ASCII.LF
         & "|id|AUTOINCREMENT|PK||" & ASCII.LF
         & "|torrent_file|Text|NOT NULL||" & ASCII.LF
         & "|filename|Text|NOT NULL||" & ASCII.LF
         & "|created_by|FK users|||" & ASCII.LF
         & "" & ASCII.LF
         & "|TABLE| users" & ASCII.LF
         & "|id|AUTOINCREMENT|PK||" & ASCII.LF
         & "|username|Text|NOT NULL,UNIQUE||" & ASCII.LF
         & "|password|Text|NOT NULL||" & ASCII.LF
         & "" & ASCII.LF
         & "|TABLE| config" & ASCII.LF
         & "|id|AUTOINCREMENT|PK||" & ASCII.LF
         & "|version|Integer|NOT NULL||" & ASCII.LF
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
