with Ada.Text_Io; use Ada.Text_Io;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Gnatcoll.Traces; use Gnatcoll.Traces;
with Gnatcoll.Sql.Sessions; use Gnatcoll.Sql.Sessions;
with Gnatcoll.Sql.Inspect; use Gnatcoll.Sql.Inspect;
with Gnatcoll.Vfs; use Gnatcoll.Vfs;
with Ada.Directories; use Ada.Directories;

with Hellish_Database;
with Orm; use Orm;

package body Hellish_Web.Database is
   Latest_Version : Natural := 1;

   procedure Migrate(Session : Session_Type) is
      Version_Query : Prepared_Statement :=
        Prepare("SELECT version from config;");
      Cur : Direct_Cursor;

      Current_Version : Natural := 0;

      Dir : Directory_Entry_Type;
      Dir_Search : Search_Type;

      Config : Detached_Config'Class := New_Config;

      Database_Exists_Query : Prepared_Statement :=
           Prepare("SELECT EXISTS(SELECT * FROM information_schema.tables WHERE table_schema = 'public' AND table_name = 'config');");
   begin
      Cur.Fetch(Session.Db, Database_Exists_Query);

      -- Database doesn't exist
      if not Cur.Boolean_Value(0) then
         Hellish_Database.Create_Database(Session.Db);
         Session.Commit;

         Config.Set_Version(Latest_Version);
         Session.Persist(Config);

         Put_Line("Database initialized at version" & Latest_Version'Image);

         Current_Version := Latest_Version;
      end if;

      Config := Get_Config(Session, Id => 1);

      Current_Version := Config.Version;

      while Current_Version < Latest_Version loop
         Current_Version := Current_Version + 1;

         Start_Search(Search => Dir_Search, Directory => "db/migrations", Pattern => Trim(Current_Version'Image, Ada.Strings.Left) & "_*.sql");

         Get_Next_Entry(Dir_Search, Dir);
         declare
            Sql_File : File_Type;
            Sql_Contents : Unbounded_String;
            Char_String : String(1..1);

            Migration_Stmt : Prepared_Statement;
         begin
            Put_Line("Applying " & Simple_Name(Dir));

            Open(Sql_File, Mode => In_File, Name => Full_Name(Dir));
            while not End_Of_File(Sql_File) loop
               Get_Immediate(Sql_File, Char_String(1));
               Append(Sql_Contents, Char_String);
            end loop;
            Close(Sql_File);

            Migration_Stmt := Prepare(To_String(Sql_Contents));
            Session.Db.Execute(Migration_Stmt);
            if not Session.Db.Success then
               Put_Line("Migration failed!");

               return;
            end if;

            Config.Set_Version(Current_Version);
            Session.Commit;
         end;
         End_Search(Dir_Search);
      end loop;
   end Migrate;

   procedure Init is
   begin
      GNATCOLL.SQL.Sessions.Setup(Db_Desc, Max_Sessions => 5);

      declare
         Session : Session_Type := Get_New_Session;
         --The_Torrent : Detached_Torrent'Class := Get_Torrent(Session, Id => 1);
      begin



         GNATCOLL.Traces.Parse_Config("+");

         Migrate(Session);
         Session.Commit;

         --The_Torrent.Set_Bencoded("test");
         --Session.Commit;
         null;
      end;
   end Init;
end;
