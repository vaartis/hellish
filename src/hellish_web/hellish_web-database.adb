with Ada.Text_Io; use Ada.Text_Io;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Directories; use Ada.Directories;

with Gnatcoll.Json,
--  Gnatcoll.Traces,
  Gnatcoll.Tribooleans;
use
--  Gnatcoll.Traces,
  Gnatcoll.Tribooleans;
with
  Gnatcoll.Sql,
  Gnatcoll.Sql.Inspect;
use
  Gnatcoll.Sql,
  Gnatcoll.Sql.Inspect;
use type
  Gnatcoll.Sql.Text_Fields.Field,
  Gnatcoll.Sql.Integer_Fields.Field;

with Aws.Smtp;

with Sodium.Functions;

with Hellish_Database;
with Hellish_Mail;

package body Hellish_Web.Database is
   Latest_Version : Natural := 10;

   procedure Migrate(Session : Session_Type) is
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
      Cur.Fetch(Session.Db, User_Exists_Query, Params => [+Name]);

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
      Session : Session_Type := Get_New_Session;

      Manager : Users_Managers := All_Users;
      Cur : Direct_User_List := Manager.Get_Direct(Session);
   begin
      -- Stupid workaround to actually run the fetch instead of whatever
      -- is done by the manager. Probably runs the query twice.
      Cur.Fetch(Session.Db, Get_User_Query, Params => [+Name]);

      if Cur.Has_Row then
         return Cur.Element.Detach;
      else
         return No_Detached_User;
      end if;
   end Get_User;

   function Get_User(Id : Integer) return Detached_User'Class is
      Session : Session_Type := Get_New_Session;
   begin
      return Orm.Get_User(Session, Id);
   end Get_User;

   function Get_User_By_Passkey(Passkey : String) return Detached_User'Class is
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
      Session : Session_Type := Get_New_Session;
      Invites_M : Invites_Managers := All_Invites.Filter(By_User => By_User.Id, Activated => True);
   begin
      return Invites_M.Get(Session);
   end;

   function Verify_User_Credentials(Name, Password : String) return Boolean is
      use Sodium.Functions;

      User_Data : Detached_User'Class := Get_User(Name);
   begin
      return Detached_User(User_Data) /= No_Detached_User and then Password_Hash_Matches(User_Data.Password, Password);
   end Verify_User_Credentials;

   procedure Update_User(The_User : Detached_User'Class) is
      Session : Session_Type := Get_New_Session;
   begin
      Session.Persist(The_User);
      Session.Commit;
   end;

   procedure Notify_User(The_User : Detached_User'Class; Notification : String) is
      use Gnatcoll.Json;

      Session : Session_Type := Get_New_Session;
      Sub_Profile : Json_Value := Read(The_User.Profile);
      Sub_Notifications : Json_Array := (if Has_Field(Sub_Profile, "notifications")
                                         then Get(Sub_Profile, "notifications")
                                         else Empty_Array);
   begin
      Prepend(Sub_Notifications, Create(Notification));
      Sub_Profile.Set_Field("notifications", Sub_Notifications);
      The_User.Set_Profile(Sub_Profile.Write);

      Session.Persist(The_User);
      Session.Commit;

      if Sub_Profile.Has_Field("email") then
         declare
            Profile_Email : Json_Value := Sub_Profile.Get("email");
         begin
            if Profile_Email.Has_Field("address") and Profile_Email.Has_Field("notifications") and
              Boolean'(Profile_Email.Get("notifications")) then

               declare
                  Address : String := Profile_Email.Get("address");
               begin
                  Hellish_Mail.Send(Aws.Smtp.E_Mail(The_User.Username, Address),
                                    Subject => "Hellish notification",
                                    Message => Notification);
               end;
            end if;
         end;
      end if;
   end;

   procedure Create_Torrent(The_Torrent : in out Detached_Torrent'Class) is
      Session : Session_Type := Get_New_Session;
   begin
      Session.Persist(The_Torrent);
      Session.Commit;
   end Create_Torrent;

   procedure Set_Torrent_Group(The_Torrent : in out Detached_Torrent'Class; Group : Integer) is
      use Hellish_Database;

      Session : Session_Type := Get_New_Session;
   begin
      -- Have to manually set the field to NULL because ORM has no way of doing this for some reason
      Session.Db.Execute(Sql_Update(Table => Torrents,
                                    Where => (Torrents.Id = The_Torrent.Id),
                                    Set => (Torrents.Group = (if Group = -1
                                                              then Null_Field_Integer
                                                              else Expression(Group)))));
      Session.Commit;
   end Set_Torrent_Group;

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
                 Params => [+User.Id, +T_List.Element.Id,
                            As_Bigint(Uploaded_Diff), As_Bigint(Downloaded_Diff)]);
      end if;

      -- Also add it to the total user credit
      Execute(Session.Db, Update_User_Stats,
              Params => [+User.Id, As_Bigint(Uploaded_Diff), As_Bigint(Downloaded_Diff)]);

      Session.Commit;
   end Update_Torrent_Up_Down;

   function Get_Torrent_By_Hash(Info_Hash : String; Session : Session_Type := Get_New_Session) return Detached_Torrent'Class is
      Manager : Torrents_Managers := All_Torrents.Filter(Info_Hash => Info_Hash);
      List : Torrent_List := Manager.Get(Session);
   begin
      if List.Has_Row then
         return List.Element.Detach;
      else
         return No_Detached_Torrent;
      end if;
   end Get_Torrent_By_Hash;

   function Get_Torrent(Id : Integer) return Detached_Torrent'Class is
      Session : Session_Type := Get_New_Session;
   begin
      return Get_Torrent(Session, Id => Id);
   end Get_Torrent;

   procedure Snatch_Torrent(Info_Hash : String; The_User : Detached_User'Class) is
      use Hellish_Database;

      Session : Session_Type := Get_New_Session;

      The_Torrent : Detached_Torrent'Class := Get_Torrent_By_Hash(Info_Hash, Session);

      Torrent_Stat_Update : Sql_Query :=
        Sql_Update(Table => User_Torrent_Stats,
                   Where => (User_Torrent_Stats.By_User = The_User.Id) and (User_Torrent_Stats.Of_Torrent = The_Torrent.Id),
                   Set => (User_Torrent_Stats.Snatched = True));
   begin
      Session.Db.Execute(Torrent_Stat_Update);

      Session.Commit;
   end Snatch_Torrent;

   function Torrent_Snatches(The_Torrent : Detached_Torrent'Class) return Integer is
      use Hellish_Database;

      Session : Session_Type := Get_New_Session;

      Cur : Direct_Cursor;
      Snatches_Query : Sql_Query :=
        Sql_Select(From => User_Torrent_Stats,
                   Fields => Apply(Func_Count, User_Torrent_Stats.Snatched),
                   Where => (User_Torrent_Stats.Of_Torrent = The_Torrent.Id)
                     and (User_Torrent_Stats.Snatched = True));
   begin
      Cur.Fetch(Session.Db, Snatches_Query);
      return Cur.Integer_Value(0);
   end Torrent_Snatches;

   function Search_Torrents(Query : String;
                            Uploader : Integer;
                            Category : Integer;
                            Snatched_By : Integer;

                            Offset : Integer;
                            Limit : Integer;
                            Total_Count : out Integer) return Direct_Torrent_List is
      use Hellish_Database;

      Search_Criteria : Sql_Criteria :=
        -- Only search by display_name if query is not empty
        ((Text_Param(1) = "")
           or Ilike(Torrents.Display_Name, Concat("%" & Text_Param(1) & "%"))) and
        -- Only search by uploader if not 0
        ((Integer_Param(2) = 0)
           or Torrents.Created_By = Integer_Param(2)) and
        -- Only search by category if it's not -1
        ((Integer_Param(3) = -1)
           or Torrents.Category = Integer_Param(3)) and
        -- Only search by snatcher if it's not -1
        ((Integer_Param(4) = -1)
           or (User_Torrent_Stats.By_User = Integer_Param(4) and
                 User_Torrent_Stats.Snatched = True));

      Session : Session_Type := Get_New_Session;

      Search_Query : Sql_Query :=
        Sql_Select(From => Left_Join(Torrents, User_Torrent_Stats, Torrents.Id = User_Torrent_Stats.Of_Torrent),
                   -- This is idiotic, but the library doesn't generate the list of all fields
                   Fields => From_String("DISTINCT ON (torrents.id) torrents.*"),
                   Limit => Limit, Offset => Offset,
                   Order_By => Desc(Torrents.Id),
                   Where => Search_Criteria);

      Count_Cur : Direct_Cursor;
      Count_Query : Sql_Query :=
        Sql_Select(From => Left_Join(Torrents, User_Torrent_Stats, Torrents.Id = User_Torrent_Stats.Of_Torrent),
                   Fields => Apply(Func_Count, From_String("DISTINCT torrents.id")),
                   Where => Search_Criteria);
      Params : Sql_Parameters := [+Query, +Uploader, +Category, +Snatched_By];
   begin
      Count_Cur.Fetch(Session.Db, Count_Query, Params => Params);
      Total_Count := Count_Cur.Integer_Value(0);

      return Result_List : Direct_Torrent_List do
         Result_List.Fetch(Session.Db, Search_Query, Params);
      end return;
   end Search_Torrents;

   procedure Delete_Torrent(Id : Integer) is
      use Hellish_Database;

      Session : Session_Type := Get_New_Session;

      Stats_Delete : Sql_Query :=
        Sql_Delete(From => User_Torrent_Stats,
                   Where => User_Torrent_Stats.Of_Torrent = Id);
      Torrent_Delete : Sql_Query :=
        Sql_Delete(From => Torrents, Where => Torrents.Id = Id);
      Comments_Delete : Sql_Query :=
        Sql_Delete(From => Posts, Where => Posts.Parent_Torrent = Id);
      Peers_Delete : Sql_Query :=
        Sql_Delete(From => Hellish_Database.Peer_Data, Where => Hellish_Database.Peer_Data.Torrent_Id = Id);
   begin
      Session.Db.Execute(Stats_Delete);
      Session.Db.Execute(Comments_Delete);
      Session.Db.Execute(Peers_Delete);
      Session.Db.Execute(Torrent_Delete);

      Session.Commit;
   end Delete_Torrent;

   function Torrent_Comments(Parent_Torrent : Integer;

                             Offset : Integer;
                             Limit : Integer;
                             Total_Count : out Integer) return Post_List is
      use Hellish_Database;

      Session : Session_Type := Get_New_Session;
      The_Post_Managers : Posts_Managers := All_Posts
        .Order_By(Asc(Posts.Id))
        .Filter(Parent_Torrent => Parent_Torrent);

      Count_Cur : Direct_Cursor;
      Count_Query : Sql_Query :=
        Sql_Select(From => Posts, Fields => Apply(Func_Count, Posts.Id), Where => (Posts.Parent_Torrent = Parent_Torrent));
   begin
      Count_Cur.Fetch(Session.Db, Count_Query);
      Total_Count := Count_Cur.Integer_Value(0);
      if Limit /= -1 then
         The_Post_Managers := @.Limit(Limit, From => Offset);
      end if;

      return The_Post_Managers.Get(Session);
   end Torrent_Comments;

   function Get_User_Stats_For_Torrent(User: Detached_User'Class; Torrent: Integer)
                                      return Detached_User_Torrent_Stat'Class is
      Session : Session_Type := Get_New_Session;
      Stats_List : User_Torrent_Stat_List := All_User_Torrent_Stats
        .Filter(By_User => User.Id, Of_Torrent => Torrent)
        .Get(Session);
   begin
         return (if Stats_List.Has_Row then Stats_List.Element.Detach else No_Detached_User_Torrent_Stat);
   end;

   function Get_Group(Id : Integer) return Detached_Torrent_Group'Class is (Get_Torrent_Group(Get_New_Session, Id));
   function Get_Group(Name : String) return Detached_Torrent_Group'Class is
      use Hellish_Database;

      Session : Session_Type := Get_New_Session;
      Group_List : Torrent_Group_List :=  All_Torrent_Groups.Filter(Torrent_Groups.Name = Name).Get(Session);
   begin
      return (if Group_List.Has_Row
              then Group_List.Element.Detach
              else No_Detached_Torrent_Group);
   end Get_Group;

   procedure Create_Group(The_Group : in out Detached_Torrent_Group'Class) is
      Session : Session_Type := Get_New_Session;
   begin
      Session.Persist(The_Group);
      Session.Commit;
   end;

   function Get_Group_Torrents(Id : Integer) return Direct_Torrent_List is
      use Hellish_Database;

      Session : Session_Type := Get_New_Session;
   begin
      return All_Torrents
          .Filter(Group => Id)
          .Order_By(Desc(Torrents.Id))
          .Get_Direct(Session);
   end;

   function Search_Torrent_Groups(Query : String;

                                  Offset : Integer;
                                  Limit : Integer;
                                  Total_Count : out Integer) return Torrent_Group_List is
      use Hellish_Database;

      Search_Criteria : Sql_Criteria :=
        -- Only search by name if query is not empty
        ((Text_Param(1) = "")
           or Ilike(Torrent_Groups.Name, Concat("%" & Text_Param(1) & "%")));

      Session : Session_Type := Get_New_Session;
      The_Query_Managers : Torrent_Groups_Managers := All_Torrent_Groups
        .Limit(Limit, From => Offset)
        .Order_By(Desc(Torrent_Groups.Id))
        .Filter(Search_Criteria);

      Count_Cur : Direct_Cursor;
      Count_Query : Sql_Query :=
        Sql_Select(From => Torrent_Groups, Fields => Apply(Func_Count, Torrent_Groups.Id), Where => Search_Criteria);
      Params : Sql_Parameters := [+Query];
   begin
      Count_Cur.Fetch(Session.Db, Count_Query, Params => Params);
      Total_Count := Count_Cur.Integer_Value(0);

      return The_Query_Managers.Get(Session, Params => Params);
   end Search_Torrent_Groups;

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

   function Get_Post(Id : Integer) return Detached_Post'Class is (Orm.Get_Post(Get_New_Session, Id));

   function Get_Post(Id : Integer; Parent_Post : out Detached_Post'Class) return Detached_Post'Class is
      Session : Session_Type := Get_New_Session;
   begin
      return Result : Detached_Post'Class := Orm.Get_Post(Session, Id) do
         Parent_Post := Result.Parent_Post;
      end return;
   end Get_Post;

   function Post_Replies(Parent_Post : Integer;

                         Offset : Integer;
                         Limit : Integer;
                         Total_Count : out Integer) return Post_List is
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
         The_Post_Managers := @.Limit(Limit, From => Offset);
      end if;

      return The_Post_Managers.Get(Session);
   end Post_Replies;

   function Get_Latest_News return Detached_Post'Class is
      use Hellish_Database;

      Session : Session_Type := Get_New_Session;

      News_List : Post_List := All_Posts
        .Filter(Flag => 1)
        .Limit(1)
        .Order_By(Desc(Posts.Id))
        .Get(Session);
   begin
      return Result : Detached_Post'Class := No_Detached_Post do
         if News_List.Has_Row then
            Result := News_List.Element.Detach;
         end if;
      end return;
   end Get_Latest_News;

   function Search_Posts(Query : String;
                         Flag : Integer;
                         Author : Integer;

                         Offset : Integer;
                         Limit : Integer;
                         Total_Count : out Integer) return Post_List is
      use Hellish_Database;

      Search_Criteria : Sql_Criteria :=
        -- Only search by display_name if query is not empty
        ((Text_Param(1) = "")
           or Ilike(Hellish_Database.Posts.Title, Concat("%" & Text_Param(1) & "%"))) and
        -- Only show posts without parents
        (Is_Null(Posts.Parent_Post) and Is_Null(Posts.Parent_Torrent)) and
        ((Integer_Param(2) = -1) or
           Hellish_Database.Posts.Flag = Integer_Param(2)) and
        -- Only search by author if name is not zero
        ((Integer_Param(3) = 0) or
           Hellish_Database.Posts.By_User = Integer_Param(3));

      Session : Session_Type := Get_New_Session;
      The_Query_Managers : Posts_Managers := All_Posts
        .Limit(Limit, From => Offset)
        .Order_By(Desc(Posts.Id))
        .Filter(Search_Criteria);

      Count_Cur : Direct_Cursor;
      Count_Query : Sql_Query :=
        Sql_Select(From => Posts, Fields => Apply(Func_Count, Posts.Id), Where => Search_Criteria);
      Params : Sql_Parameters := [+Query, +Flag, +Author];
   begin
      Count_Cur.Fetch(Session.Db, Count_Query, Params => Params);
      Total_Count := Count_Cur.Integer_Value(0);

      return The_Query_Managers.Get(Session, Params => Params);
   end Search_Posts;

   Peers_Insert_Statement : Prepared_Statement :=
     Prepare("INSERT INTO peer_data (torrent_id, data) VALUES ($1, $2) " &
               "ON CONFLICT (torrent_id) DO UPDATE SET data = EXCLUDED.data;");
   procedure Persist_Peers(Info_Hash : String; Data : String) is
      Session : Session_Type := Get_New_Session;
      The_Torrent : Detached_Torrent'Class := Get_Torrent_By_Hash(Info_Hash, Session);
   begin
      Session.Db.Execute(Peers_Insert_Statement, Params => [+The_Torrent.Id, +Data]);

      Session.Commit;
   end Persist_Peers;

   function Persisted_Peers return Peer_Data_List is
      Session : Session_Type := Get_New_Session;
   begin
      return All_Peer_Data.Get(Session);
   end Persisted_Peers;

   function Add_Uploaded_Image(Username : String; Filename : String) return Detached_Image_Upload'Class is
      Session : Session_Type := Get_New_Session;
      The_User : Detached_User'Class := Get_User(Username);
      Image_List : Image_Upload_List := All_Image_Uploads
        .Filter(By_User => The_User.Id, Filename => Filename)
        .Get(Session);
      The_Image : Detached_Image_Upload'Class := New_Image_Upload;
   begin
      if Image_List.Has_Row then
         The_Image := Image_List.Element.Detach;
      else
         The_Image.Set_By_User(The_User);
         The_Image.Set_Filename(Filename);

         Session.Persist(The_Image);
         Session.Commit;
      end if;

      return The_Image;
   end Add_Uploaded_Image;

   function Delete_Uploaded_Image(Id : Integer) return Boolean is
      use Hellish_Database;

      Session : Session_Type := Get_New_Session;

      The_Image : Detached_Image_Upload'Class := Get_Image_Upload(Session, Id);
      Delete_Image : Sql_Query := Sql_Delete(From => Image_Uploads, Where => (Image_Uploads.Id = Id));
   begin
      Session.Db.Execute(Delete_Image);
      Session.Commit;

      -- False = no images left; True = this image is used by others, don't delete the file
      return All_Image_Uploads
          .Filter(Filename => The_Image.Filename)
          .Get(Session)
          .Has_row;
   end Delete_Uploaded_Image;

   function User_Images(Username : String) return Image_Upload_List is
      Session : Session_Type := Get_New_Session;
      The_User : Detached_User'Class := Get_User(Username);
   begin
      return All_Image_Uploads.Filter(By_User => The_User.Id).Get(Session);
   end User_Images;

   function Get_Image(Id : Integer) return Detached_Image_Upload'Class is
      Session : Session_Type := Get_New_Session;
   begin
      return Get_Image_Upload(Session, Id);
   end Get_Image;

   function Admin_Recently_Invited(Limit : Integer) return Invite_List is
      use Hellish_Database;

      Session : Session_Type := Get_New_Session;
   begin
      return All_Invites.Filter(Activated => True)
          .Limit(Limit)
          .Select_Related(Depth => 1)
          .Order_By(Desc(Hellish_Database.Invites.For_User))
          .Get(Session);
   end;

   Channel_Insert_Statement : Prepared_Statement :=
     Prepare("INSERT INTO irc_channels (name, data) VALUES ($1, $2) " &
               "ON CONFLICT (name) DO UPDATE SET data = EXCLUDED.data;");
   procedure Persist_Channel(Name, Data : String) is
      Session : Session_Type := Get_New_Session;
   begin
      Session.Db.Execute(Channel_Insert_Statement, Params => [+Name, +Data]);
      Session.Commit;
   end Persist_Channel;
   function Persisted_Channels return Irc_Channel_List is (All_Irc_Channels.Get(Get_New_Session));

   package body Subscriptions is
      procedure Subscribe(User : Detached_User'Class; To : T) is
         use Gnatcoll.Json;

         The_Meta : Json_Value := Read(Meta(To));
         Subscriptions : Json_Array := (if Has_Field(The_Meta, "subscribed")
                                        then Get(The_Meta, "subscribed")
                                        else Empty_Array);
         Unsubscribed : Json_Array := (if Has_Field(The_Meta, "unsubscribed")
                                       then Get(The_Meta, "unsubscribed")
                                       else Empty_Array);
         New_Unsubscribed : Json_Array := Empty_Array;
         User_Id : Integer := User.Id;

         Session : Session_Type := Get_New_Session;
      begin
         -- If already subscribed, don't do it again
         if (for some Subscriber of Subscriptions => Get(Subscriber) = User_Id) then
            return;
         end if;

         for Subscriber of Unsubscribed loop
            -- Copy everything except the original ID, because this stupid API has no way
            -- of deleting elements from arrays
            if Get(Subscriber) /= User_Id then
               Append(New_Unsubscribed, Subscriber);
            end if;
         end loop;

        Append(Subscriptions, Create(User_Id));
        Set_Field(The_Meta, "subscribed", Create(Subscriptions));
        Set_Field(The_Meta, "unsubscribed", Create(New_Unsubscribed));
        Set_Meta(To, The_Meta.Write);

        Session.Persist(To);
        Session.Commit;
      end Subscribe;

      function Subscribed(User : Detached_User'Class; To : T) return Boolean is
         use Gnatcoll.Json;

         The_Meta : Json_Value := Read(Meta(To));
         Subscriptions : Json_Array := (if Has_Field(The_Meta, "subscribed")
                                        then Get(The_Meta, "subscribed")
                                        else Empty_Array);
         User_Id : Integer := User.Id;
      begin
         for Subscriber of Subscriptions loop
            if Get(Subscriber) = User_Id then return True; end if;
         end loop;

         return False;
      end Subscribed;

      procedure Unsubscribe(User : Detached_User'Class; From : T) is
         use Gnatcoll.Json;

         The_Meta : Json_Value := Read(Meta(From));
         Subscriptions : Json_Array := (if Has_Field(The_Meta, "subscribed")
                                        then Get(The_Meta, "subscribed")
                                        else Empty_Array);
         New_Subscriptions : Json_Array := Empty_Array;
         Unsubscribed : Json_Array := (if Has_Field(The_Meta, "unsubscribed")
                                       then Get(The_Meta, "unsubscribed")
                                       else Empty_Array);

         User_Id : Integer := User.Id;

         Session : Session_Type := Get_New_Session;
      begin
         -- If already unsubscribed, don't do it again
         if (for some Subscriber of Unsubscribed => Get(Subscriber) = User_Id) then
            return;
         end if;

         for Subscriber of Subscriptions loop
            -- Copy everything except the original ID, because this stupid API has no way
            -- of deleting elements from arrays
            if Get(Subscriber) /= User_Id then
               Append(New_Subscriptions, Subscriber);
            end if;
         end loop;

         Append(Unsubscribed, Create(User_Id));
         Set_Field(The_Meta, "subscribed", Create(New_Subscriptions));
         Set_Field(The_Meta, "unsubscribed", Create(Unsubscribed));

         Set_Meta(From, The_Meta.Write);

         Session.Persist(From);
         Session.Commit;
      end Unsubscribe;

      function Explicitly_Unsubscribed(User : Detached_User'Class; From : T) return Boolean is
         use Gnatcoll.Json;

         The_Meta : Json_Value := Read(Meta(From));
         Unsubscribed : Json_Array := (if Has_Field(The_Meta, "unsubscribed")
                                       then Get(The_Meta, "unsubscribed")
                                       else Empty_Array);
         User_Id : Integer := User.Id;
      begin
         return (for some Subscriber of Unsubscribed => Get(Subscriber) = User_Id);
      end Explicitly_Unsubscribed;

      procedure Notify(Creator : Detached_User'Class; From : T; Text : String) is
         use Gnatcoll.Json;

         The_Meta : Json_Value := Read(Meta(From));
         Subscriptions : Json_Array := (if Has_Field(The_Meta, "subscribed")
                                        then Get(The_Meta, "subscribed")
                                        else Empty_Array);
      begin
         for Subscriber of Subscriptions loop
            if Get(Subscriber) /= Creator.Id then
               Notify_User(Database.Get_User(Integer'(Get(Subscriber))), Text);
            end if;
         end loop;
      end Notify;
   end Subscriptions;
end;
