with Ada.Text_Io; use Ada.Text_Io;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Directories; use Ada.Directories;
with Ada.Characters.Latin_1;

with Gnatcoll.Traces; use Gnatcoll.Traces;
with Gnatcoll.Tribooleans; use Gnatcoll.Tribooleans;
with
  Gnatcoll.Sql,
  Gnatcoll.Sql.Sessions,
  Gnatcoll.Sql.Inspect;
use
  Gnatcoll.Sql,
  Gnatcoll.Sql.Sessions,
  Gnatcoll.Sql.Inspect;
use type
  Gnatcoll.Sql.Text_Fields.Field,
  Gnatcoll.Sql.Integer_Fields.Field;

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
      GNATCOLL.SQL.Sessions.Setup(Db_Desc, Max_Sessions => 128);

      declare
         Session : Session_Type := Get_New_Session;
      begin
         -- GNATCOLL.Traces.Parse_Config("+" & Ada.Characters.Latin_1.Lf & "SQL.*=yes");

         Migrate(Session);
         Session.Commit;
      end;
   end Init;

   User_Exists_Query : Prepared_Statement :=
     Prepare("SELECT * FROM users WHERE LOWER(username) = LOWER($1);", On_Server => True);
   function User_Exists(Name : String; Session : Session_Type := Get_New_Session) return Boolean is
      Cur : Direct_Cursor;
   begin
      Cur.Fetch(Session.Db, User_Exists_Query, Params => (1 => +Name));

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

   Get_User_Query : Prepared_Statement :=
        Prepare("SELECT * FROM users WHERE LOWER(username) = LOWER($1);", On_Server => True);
   function Get_User(Name : String) return Detached_User'Class is
      use Hellish_Database;

      Session : Session_Type := Get_New_Session;

      Manager : Users_Managers := All_Users;
      Cur : Direct_User_List := Manager.Get_Direct(Session);
   begin
      -- Stupid workaround to actually run the fetch instead of whatever
      -- is done by the manager. Probably runs the query twice.
      Cur.Fetch(Session.Db, Get_User_Query, Params => (1 => +Name));

      if Cur.Has_Row then
         return Cur.Element.Detach;
      else
         return No_Detached_User;
      end if;
   end Get_User;

   function Get_User(Id : Natural) return Detached_User'Class is
      use Hellish_Database;

      Session : Session_Type := Get_New_Session;
   begin
      return Orm.Get_User(Session, Id);
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

   function Get_Invited_Users(By_User : Detached_User'Class) return Invite_List is
      use Hellish_Database;

      Session : Session_Type := Get_New_Session;
      Invites_M : Invites_Managers := All_Invites.Filter(By_User => By_User.Id, Activated => True);
   begin
      return Invites_M.Get(Session);
   end;

   function Verify_User_Credentials(Name, Password : String) return Boolean is
      use Sodium.Functions;
      use Hellish_Database;

      User_Data : Detached_User'Class := Get_User(Name);
   begin
      return Detached_User(User_Data) /= No_Detached_User and then Password_Hash_Matches(User_Data.Password, Password);
   end Verify_User_Credentials;

   procedure Create_Torrent(The_Torrent : in out Detached_Torrent'Class) is
      Session : Session_Type := Get_New_Session;
   begin
      Session.Persist(The_Torrent);
      Session.Commit;
   end Create_Torrent;

   Update_User_Torrent_Stats_Stmt : Prepared_Statement :=
     Prepare("UPDATE user_torrent_stats SET " &
               "uploaded=uploaded + $3, downloaded=downloaded + $4 WHERE by_user = $1 AND of_torrent = $2;",
             On_Server => True);
   Update_User_Stats : Prepared_Statement :=
     Prepare("UPDATE users SET " &
               "uploaded=uploaded + $2, downloaded=downloaded + $3 WHERE id = $1;",
             On_Server => True);
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
      else
         -- Updating fields in gnatcoll sql just does not work with orm, no matter how I try it.
         -- So SQL queries are used instead.
            Execute(Session.Db, Update_User_Torrent_Stats_Stmt,
                 Params => (1 => +User.Id, 2 => +T_List.Element.Id,
                            3 => As_Bigint(Uploaded_Diff), 4 => As_Bigint(Downloaded_Diff)));
      end if;

      -- Also add it to the total user credit
      Execute(Session.Db, Update_User_Stats,
              Params => ( 1 => +User.Id, 2 => As_Bigint(Uploaded_Diff), 3 => As_Bigint(Downloaded_Diff)));

      Session.Commit;
   end Update_Torrent_Up_Down;

   function Get_Torrent_By_Hash(Info_Hash : String; Session : Session_Type := Get_New_Session) return Detached_Torrent'Class is
      use Hellish_Database;

      Manager : Torrents_Managers := All_Torrents.Filter(Info_Hash => Info_Hash);
      List : Torrent_List := Manager.Get(Session);
   begin
      if List.Has_Row then
         return List.Element.Detach;
      else
         return No_Detached_Torrent;
      end if;
   end Get_Torrent_By_Hash;

   function Get_Torrent(Id : Natural) return Detached_Torrent'Class is
      use Hellish_Database;

      Session : Session_Type := Get_New_Session;
   begin
      return Get_Torrent(Session, Id => Id);
   end Get_Torrent;

   procedure Snatch_Torrent(Info_Hash : String) is
      Session : Session_Type := Get_New_Session;

      The_Torrent : Detached_Torrent'Class := Get_Torrent_By_Hash(Info_Hash, Session);
   begin
      The_Torrent.Set_Snatches(The_Torrent.Snatches + 1);
      Session.Persist(The_Torrent);
      Session.Commit;
   end Snatch_Torrent;

   function Search_Torrents(Query : String;
                            Uploader : Natural;

                            Offset : Natural;
                            Limit : Natural;
                            Total_Count : out Natural) return Torrent_List is
      use Hellish_Database;

      Search_Criteria : Sql_Criteria :=
        -- Only search by display_name of query is not empty
        ((Text_Param(1) = "")
           or Ilike(Hellish_Database.Torrents.Display_Name, Concat("%" & Text_Param(1) & "%"))) and
        -- Only search by uploader if not 0
        ((Integer_Param(2) = 0)
           or Hellish_Database.Torrents.Created_By = Integer_Param(2));

      Session : Session_Type := Get_New_Session;
      The_Query_Managers : Torrents_Managers := All_Torrents
        .Limit(Limit, From => Offset)
        .Order_By(Desc(Torrents.Id))
        .Filter(Search_Criteria);

      Count_Cur : Direct_Cursor;
      Count_Query : Sql_Query :=
        Sql_Select(From => Torrents, Fields => Apply(Func_Count, Torrents.Id), Where => Search_Criteria);
      Params : Sql_Parameters := (1 => +Query, 2 => +Uploader);
   begin
      Count_Cur.Fetch(Session.Db, Count_Query, Params => Params);
      Total_Count := Count_Cur.Integer_Value(0);

      return The_Query_Managers.Get(Session, Params => Params);
   end Search_Torrents;

   procedure Delete_Torrent(Id : Natural) is
      use Hellish_Database;

      Session : Session_Type := Get_New_Session;

      Stats_Delete : Sql_Query :=
        Sql_Delete(From => User_Torrent_Stats,
                   Where => User_Torrent_Stats.Of_Torrent = Id);
      Torrent_Delete : Sql_Query :=
           Sql_Delete(From => Torrents, Where => Torrents.Id = Id);
   begin
      Session.Db.Execute(Stats_Delete);
      Session.Db.Execute(Torrent_Delete);

      Session.Commit;
   end Delete_Torrent;

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

   procedure Create_Post(The_Post : in out Detached_Post'Class) is
      Session : Session_Type := Get_New_Session;
   begin
      Session.Persist(The_Post);
      Session.Commit;
   end Create_Post;

   function Get_Post(Id : Natural; Parent_Post : out Detached_Post'Class) return Detached_Post'Class is
      Session : Session_Type := Get_New_Session;

      Result : Detached_Post'Class := Orm.Get_Post(Session, Id);
   begin
      Parent_Post := Result.Parent_Post;
      return Result;
   end Get_Post;

   function Post_Replies(Parent_Post : Integer;

                         Offset : Natural;
                         Limit : Integer;
                         Total_Count : out Natural) return Post_List is
      use Hellish_Database;

      Session : Session_Type := Get_New_Session;
      The_Post_Managers : Posts_Managers := All_Posts
        .Order_By(Asc(Posts.Id))
        .Filter(Parent_Post => Parent_Post);

      Count_Cur : Direct_Cursor;
      Count_Query : Sql_Query :=
        Sql_Select(From => Posts, Fields => Apply(Func_Count, Posts.Id), Where => (Posts.Parent_Post = Parent_Post));
   begin
      Count_Cur.Fetch(Session.Db, Count_Query);
      Total_Count := Count_Cur.Integer_Value(0);
      if Limit /= -1 then
         The_Post_Managers := The_Post_Managers.Limit(Limit, From => Offset);
      end if;

      return The_Post_Managers.Get(Session);
   end Post_Replies;

   function Get_Latest_News return Detached_Post'Class is
      use Hellish_Database;

      Session : Session_Type := Get_New_Session;

      Result : Detached_Post'Class := No_Detached_Post;
      News_List : Post_List := All_Posts
        .Filter(Flag => 1)
        .Limit(1)
        .Order_By(Desc(Posts.Id))
        .Get(Session);
   begin
      if News_List.Has_Row then
         Result := News_List.Element.Detach;
      end if;

      return Result;
   end Get_Latest_News;

   function Search_Posts(Query : String;

                         Offset : Natural;
                         Limit : Natural;
                         Total_Count : out Natural) return Post_List is
      use Hellish_Database;

      Search_Criteria : Sql_Criteria :=
        -- Only search by display_name of query is not empty
        ((Text_Param(1) = "")
           or Ilike(Hellish_Database.Posts.Title, Concat("%" & Text_Param(1) & "%")))
        -- Only show posts without parents
        and (Is_Null(Posts.Parent_Post));

      Session : Session_Type := Get_New_Session;
      The_Query_Managers : Posts_Managers := All_Posts
        .Limit(Limit, From => Offset)
        .Order_By(Desc(Posts.Id))
        .Filter(Search_Criteria);

      Count_Cur : Direct_Cursor;
      Count_Query : Sql_Query :=
        Sql_Select(From => Posts, Fields => Apply(Func_Count, Posts.Id), Where => Search_Criteria);
      Params : Sql_Parameters := (1 => +Query);
   begin
      Count_Cur.Fetch(Session.Db, Count_Query, Params => Params);
      Total_Count := Count_Cur.Integer_Value(0);

      return The_Query_Managers.Get(Session, Params => Params);
   end Search_Posts;
end;
