with Ada.Text_Io; use Ada.Text_Io;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Directories; use Ada.Directories;

with Gnatcoll.Traces; use Gnatcoll.Traces;
with Gnatcoll.Sql; use Gnatcoll.Sql;
with Gnatcoll.Sql.Sessions; use Gnatcoll.Sql.Sessions;
with Gnatcoll.Sql.Inspect; use Gnatcoll.Sql.Inspect;
with Gnatcoll.Vfs; use Gnatcoll.Vfs;

with Sodium.Functions;

with Hellish_Database;
with Orm; use Orm;

package body Hellish_Web.Database is
   Latest_Version : Natural := 0;

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

         if not More_Entries(Dir_Search) then
            Put_Line("No migrations for version" & Current_Version'Image);

            goto After_Migration;
         end if;

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

         <<After_Migration>>
         End_Search(Dir_Search);
      end loop;
   end Migrate;

   procedure Init is
   begin
      GNATCOLL.SQL.Sessions.Setup(Db_Desc, Max_Sessions => 5);

      declare
         Session : Session_Type := Get_New_Session;
      begin
         GNATCOLL.Traces.Parse_Config("+");

         Migrate(Session);
         Session.Commit;
      end;
   end Init;

   function User_Exists(Name : String; Session : Session_Type) return Boolean is
      Query : Prepared_Statement :=
        Prepare("SELECT * FROM users WHERE LOWER(username) = LOWER($1);", On_Server => True);
      Cur : Direct_Cursor;
   begin
      Cur.Fetch(Session.Db, Query, Params => (1 => +Name));

      return Cur.Has_Row;
   end;

   function Create_User(Name, Password : String) return Boolean is
      use Sodium.Functions;

      Password_Hash : Any_Hash :=
        Generate_Password_Hash(Criticality => Online_Interactive, Password => Password);

      The_User: Detached_User'Class := New_User;

      Session : Session_Type := Get_New_Session;
   begin
      if not User_Exists(Name, Session) then
         The_User.Set_Username(Name);
         The_User.Set_Password(Password_Hash);

         Session.Persist(The_User);
         Session.Commit;

         return True;
      end if;

      return False;
   end Create_User;

   function Get_User(Name : String; Session : Session_Type := Get_New_Session) return Detached_User'Class is
      use Hellish_Database;

      Query : Prepared_Statement :=
        Prepare("SELECT * FROM users WHERE LOWER(username) = LOWER($1);", On_Server => True);

      Manager : Users_Managers := All_Users;
      Cur : Direct_User_List := Manager.Get_Direct(Session);
   begin
      -- Stupid workaround to actually run the fetch instead of whatever
      -- is done by the manager. Probably runs the query twice.
      Cur.Fetch(Session.Db, Query, Params => (1 => +Name));

      if Cur.Has_Row then
         return Cur.Element.Detach;
      else
         return No_Detached_User;
      end if;
   end Get_User;

   function Verify_User_Credentials(Name, Password : String) return Boolean is
      use Sodium.Functions;
      use Hellish_Database;

      User_Data : Detached_User'Class := Get_User(Name);
   begin
      return Detached_User(User_Data) /= No_Detached_User and then Password_Hash_Matches(User_Data.Password, Password);
   end Verify_User_Credentials;

   procedure Create_Torrent(Filename, Username, Torrent_File : String) is
      Session : Session_Type := Get_New_Session;

      Created_By : Detached_User'Class := Get_User(Username, Session);
      The_Torrent : Detached_Torrent'Class := New_Torrent;
   begin
      The_Torrent.Set_Filename(Filename);
      The_Torrent.Set_Torrent_File(Torrent_File);
      The_Torrent.Set_Created_By(Created_By);

      Session.Persist(The_Torrent);
      Session.Commit;
   end Create_Torrent;
end;
