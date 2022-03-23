with Ada.Text_Io; use Ada.Text_Io;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Directories; use Ada.Directories;

with Gnatcoll.Traces; use Gnatcoll.Traces;
with Gnatcoll.Tribooleans; use Gnatcoll.Tribooleans;
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

   function User_Exists(Name : String; Session : Session_Type := Get_New_Session) return Boolean is
      Query : Prepared_Statement :=
        Prepare("SELECT * FROM users WHERE LOWER(username) = LOWER($1);", On_Server => True);
      Cur : Direct_Cursor;
   begin
      Cur.Fetch(Session.Db, Query, Params => (1 => +Name));

      return Cur.Has_Row;
   end;

   function Create_User(Name, Password : String; Created_User : out Detached_User'Class) return Boolean is
      use Sodium.Functions;

      Password_Hash : Any_Hash :=
        Generate_Password_Hash(Criticality => Online_Interactive, Password => Password);

      The_User: Detached_User'Class := New_User;

      Session : Session_Type := Get_New_Session;

      Passkey : String := As_Hexidecimal(Random_Hash_Key(16));
   begin
      if not User_Exists(Name, Session) then
         The_User.Set_Username(Name);
         The_User.Set_Password(Password_Hash);
         The_User.Set_Passkey(Passkey);

         Session.Persist(The_User);
         Session.Commit;

         Created_User := The_User;

         return True;
      end if;

      Created_User := Detached_User'Class(No_Detached_User);

      return False;
   end Create_User;

   function Get_User(Name : String) return Detached_User'Class is
      use Hellish_Database;

      Session : Session_Type := Get_New_Session;

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

   function Get_User_By_Passkey(Passkey : String) return Detached_User'Class is
      use Hellish_Database;

      Session : Session_Type := Get_New_Session;

      Manager : Users_Managers := All_Users.Filter(Passkey => Passkey);
      List : User_List := Manager.Get(Session);
   begin
      if List.Has_Row then
         return List.Element.Detach;
      else
         return No_Detached_User;
      end if;
   end Get_User_By_Passkey;

   function Verify_User_Credentials(Name, Password : String) return Boolean is
      use Sodium.Functions;
      use Hellish_Database;

      User_Data : Detached_User'Class := Get_User(Name);
   begin
      return Detached_User(User_Data) /= No_Detached_User and then Password_Hash_Matches(User_Data.Password, Password);
   end Verify_User_Credentials;

   procedure Create_Torrent(Username, Info_Hash : String) is
      Session : Session_Type := Get_New_Session;

      Created_By : Detached_User'Class := Get_User(Username);
      The_Torrent : Detached_Torrent'Class := New_Torrent;
   begin
      The_Torrent.Set_Info_Hash(Info_Hash);
      The_Torrent.Set_Created_By(Created_By);

      Session.Persist(The_Torrent);
      Session.Commit;
   end Create_Torrent;

   procedure Update_Torrent_Up_Down(User : Detached_User'Class; Info_Hash : String;
                                    Uploaded_Diff : Long_Long_Integer; Downloaded_Diff : Long_Long_Integer) is
      use Hellish_Database;

      Session : Session_Type := Get_New_Session;

      T_List : Torrent_List := All_Torrents.Filter(Info_Hash => Info_Hash).Get(Session);
      S_List : User_Torrent_Stat_List := All_User_Torrent_Stats.Filter(By_User => User.Id, Of_Torrent => T_List.Element.Id).Get(Session);

      Stats : Detached_User_Torrent_Stat'Class := (if S_List.Has_Row
                                                   then S_List.Element.Detach
                                                   else New_User_Torrent_Stat);
   begin
      if not S_List.Has_Row then
         Stats.Set_By_User(User.Id);
         Stats.Set_Of_Torrent(T_List.Element.Id);
         Stats.Set_Uploaded(Uploaded_diff);
         Stats.Set_Downloaded(Downloaded_Diff);

         Session.Persist(Stats);
         Session.Commit;
      else
         -- Updating fields in gnatcoll sql just does not work with orm, no matter how I try it.
         -- So SQL queries are used instead. Also, commit doesn't work properly if not called
         -- after every operation explicitly, so do that too.
         declare
            -- Use whatever value we had for addition instead of letting the database do it,
            -- because this form of addition just doesn't work with long integers for some reason.
            Query : Sql_Query := Sql_Update
              (Table => User_Torrent_Stats,
               Set => (User_Torrent_Stats.Uploaded = S_List.Element.Uploaded + Uploaded_Diff)
                 & (User_Torrent_Stats.Downloaded = S_List.Element.Downloaded + Downloaded_Diff),
               Where => (User_Torrent_Stats.By_User = User.Id) and (User_Torrent_Stats.Of_Torrent = T_List.Element.Id));
         begin
            Execute(Session.Db, Query);
            Session.Commit;
         end;
      end if;

      -- Also add it to the total user credit
      declare
         Query : Sql_Query :=
           Sql_Update
             (Table => Users,
              Set => (Users.Uploaded = User.Uploaded + Uploaded_diff)
                & (Users.Downloaded = User.Downloaded + Downloaded_Diff),
             Where => Users.Id = User.Id);
      begin
         Execute(Session.Db, Query);
         Session.Commit;
      end;
   end Update_Torrent_Up_Down;

   function Get_Torrent_By_Hash(Info_Hash : String) return Detached_Torrent'Class is
      use Hellish_Database;

      Session : Session_Type := Get_New_Session;

      Manager : Torrents_Managers := All_Torrents.Filter(Info_Hash => Info_Hash);
      List : Torrent_List := Manager.Get(Session);
   begin
      if List.Has_Row then
         return List.Element.Detach;
      else
         return No_Detached_Torrent;
      end if;
   end Get_Torrent_By_Hash;

   function Create_Invite(From_User : Detached_User'Class) return String is
      use Sodium.Functions;

      Session : Session_Type := Get_New_Session;
      Manager : Invites_Managers := All_Invites.Filter(By_User => From_User.Id, Activated => To_Triboolean(False));
      List : Invite_List := Manager.Get(Session);
   begin
      if List.Has_Row then
         return List.Element.Value;
      end if;

      declare
         The_Invite : Detached_Invite'Class := New_Invite;
      begin
         The_Invite.Set_By_User(From_User);
         The_Invite.Set_Value(As_Hexidecimal(Random_Hash_Key(32)));
         The_Invite.Set_Activated(False);

         Session.Persist(The_Invite);
         Session.Commit;

         return The_Invite.Value;
      end;
   end Create_Invite;

   function Invite_Valid(Invite : String) return Boolean is
      use Hellish_Database;

      Session : Session_Type := Get_New_Session;
      Manager : Invites_Managers := All_Invites.Filter(Value => Invite);
      List : Invite_List := Manager.Get(Session);
   begin
      if List.Has_Row then
         return not List.Element.Activated;
      end if;

      return False;
   end Invite_Valid;

   procedure Invite_Use(Invite : String; Invited_User : Detached_User'Class) is
      Session : Session_Type := Get_New_Session;

      Manager : Invites_Managers := All_Invites.Filter(Value => Invite);
      List : Invite_List := Manager.Get(Session);
   begin
      if List.Has_Row then
         declare
            The_Invite : Detached_Invite'Class := List.Element.Detach;
         begin
            The_Invite.Set_Activated(True);
            The_Invite.Set_For_User(Invited_User.Id);
            Session.Persist(The_Invite);
            Session.Commit;
         end;
      end if;
   end Invite_Use;
end;
