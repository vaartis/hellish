with GNATCOLL.SQL.Inspect; use GNATCOLL.SQL.Inspect;
with GNATCOLL.SQL.Exec; use GNATCOLL.SQL.Exec;
package body Hellish_Database is
   pragma Style_Checks (Off);

   procedure Create_Database
      (DB : not null access GNATCOLL.SQL.Exec.Database_Connection_Record'Class)
   is
      DbSchema : constant String := "|TABLE| torrents" & ASCII.LF
         & "|id|AUTOINCREMENT|PK||" & ASCII.LF
         & "|bencoded|Text|NOT NULL||" & ASCII.LF
         & "|filename|Text|NOT NULL||" & ASCII.LF
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
