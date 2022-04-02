pragma Warnings (Off);
with Ada.Calendar; use Ada.Calendar;
with Ada.Finalization; use Ada.Finalization;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with GNAT.Calendar; use GNAT.Calendar;
with GNAT.Strings; use GNAT.Strings;
with GNATCOLL.SQL; use GNATCOLL.SQL;
with GNATCOLL.SQL.Exec; use GNATCOLL.SQL.Exec;
with GNATCOLL.SQL.Orm; use GNATCOLL.SQL.Orm;
with GNATCOLL.SQL.Orm.Impl; use GNATCOLL.SQL.Orm.Impl;
with GNATCOLL.SQL.Sessions; use GNATCOLL.SQL.Sessions;
with GNATCOLL.SQL_Fields; use GNATCOLL.SQL_Fields;
with GNATCOLL.Tribooleans; use GNATCOLL.Tribooleans;
with Hellish_Database; use Hellish_Database;
with System.Address_Image;
pragma Warnings (On);
pragma Style_Checks (Off);

package Orm is
   package DBA renames Hellish_Database;
   subtype Related_Depth is Integer range 0 .. 3;

   -----------
   -- Types --
   -----------
   --  Detached_* elements extract the value from the list and store them
    --  locally. As a result, they remain valid even if the list is modified,
    --  but require more memory to store.
    --
    --  Other elements are only valid while the list from which they are
    --  created is not modified(see Element below). As soon as you iterate the
    --  list this element becomes invalid.
    --
    --  Direct lists are stored in memory, and can be traversed in any order.
    --  Forward lists can only be iterated forward. With some database backends
    --  this is much more efficient since only the current element needs to be
    --  stored in memory(and retrieved from the server).

   type Config is new Orm_Element with null record;
   type Config_DDR is new Detached_Data (2) with private;
   type Detached_Config is  --  Get() returns a Config_DDR
   new Sessions.Detached_Element with private;
   type Detached_Config_Access is access all Detached_Config'Class;
   No_Detached_Config : constant Detached_Config;
   No_Config : constant Config;

   type Image_Upload is new Orm_Element with null record;
   type Image_Upload_DDR is new Detached_Data (4) with private;
   type Detached_Image_Upload is  --  Get() returns a Image_Upload_DDR
   new Sessions.Detached_Element with private;
   type Detached_Image_Upload_Access is access all Detached_Image_Upload'Class;
   No_Detached_Image_Upload : constant Detached_Image_Upload;
   No_Image_Upload : constant Image_Upload;

   type Invite is new Orm_Element with null record;
   type Invite_DDR is new Detached_Data (7) with private;
   type Detached_Invite is  --  Get() returns a Invite_DDR
   new Sessions.Detached_Element with private;
   type Detached_Invite_Access is access all Detached_Invite'Class;
   No_Detached_Invite : constant Detached_Invite;
   No_Invite : constant Invite;

   type Peer_Data is new Orm_Element with null record;
   type Peer_Data_DDR is new Detached_Data (3) with private;
   type Detached_Peer_Data is  --  Get() returns a Peer_Data_DDR
   new Sessions.Detached_Element with private;
   type Detached_Peer_Data_Access is access all Detached_Peer_Data'Class;
   No_Detached_Peer_Data : constant Detached_Peer_Data;
   No_Peer_Data : constant Peer_Data;

   type Post is new Orm_Element with null record;
   type Post_DDR is new Detached_Data (10) with private;
   type Detached_Post is  --  Get() returns a Post_DDR
   new Sessions.Detached_Element with private;
   type Detached_Post_Access is access all Detached_Post'Class;
   No_Detached_Post : constant Detached_Post;
   No_Post : constant Post;

   type Torrent is new Orm_Element with null record;
   type Torrent_DDR is new Detached_Data (7) with private;
   type Detached_Torrent is  --  Get() returns a Torrent_DDR
   new Sessions.Detached_Element with private;
   type Detached_Torrent_Access is access all Detached_Torrent'Class;
   No_Detached_Torrent : constant Detached_Torrent;
   No_Torrent : constant Torrent;

   type User_Torrent_Stat is new Orm_Element with null record;
   type User_Torrent_Stat_DDR is new Detached_Data (7) with private;
   type Detached_User_Torrent_Stat is  --  Get() returns a User_Torrent_Stat_DDR
   new Sessions.Detached_Element with private;
   type Detached_User_Torrent_Stat_Access is access all Detached_User_Torrent_Stat'Class;
   No_Detached_User_Torrent_Stat : constant Detached_User_Torrent_Stat;
   No_User_Torrent_Stat : constant User_Torrent_Stat;

   type User is new Orm_Element with null record;
   type User_DDR is new Detached_Data (7) with private;
   type Detached_User is  --  Get() returns a User_DDR
   new Sessions.Detached_Element with private;
   type Detached_User_Access is access all Detached_User'Class;
   No_Detached_User : constant Detached_User;
   No_User : constant User;


   ------------------------
   -- Elements: Torrents --
   ------------------------

   function "=" (Op1 : Torrent; Op2 : Torrent) return Boolean;
   function "=" (Op1 : Detached_Torrent; Op2 : Detached_Torrent) return Boolean;
   --  Compares two elements using only the primary keys. All other fields are
   --  ignored

   function Category (Self : Torrent) return Integer;
   function Category (Self : Detached_Torrent) return Integer;
   procedure Set_Category (Self : Detached_Torrent; Value : Integer);
   --  0 = Other

   function Created_By (Self : Torrent) return Integer;
   function Created_By (Self : Detached_Torrent) return Integer;
   procedure Set_Created_By (Self : Detached_Torrent; Value : Integer);
   function Created_By (Self : Torrent) return User'Class;
   function Created_By (Self : Detached_Torrent) return Detached_User'Class;
   procedure Set_Created_By
     (Self  : Detached_Torrent;
      Value : Detached_User'Class);

   function Description (Self : Torrent) return String;
   function Description (Self : Detached_Torrent) return String;
   procedure Set_Description (Self : Detached_Torrent; Value : String);

   function Display_Name (Self : Torrent) return String;
   function Display_Name (Self : Detached_Torrent) return String;
   procedure Set_Display_Name (Self : Detached_Torrent; Value : String);

   function Id (Self : Torrent) return Integer;
   function Id (Self : Detached_Torrent) return Integer;

   function Info_Hash (Self : Torrent) return String;
   function Info_Hash (Self : Detached_Torrent) return String;
   procedure Set_Info_Hash (Self : Detached_Torrent; Value : String);
   --  The SHA1 hash of the torrent

   function Detach (Self : Torrent'Class) return Detached_Torrent'Class;

   function From_Cache
     (Session : Session_Type;
      Id      : Integer)
     return Detached_Torrent'Class;
   --  Check whether there is already an element with this primary key. If
   --  not, the returned value will be a null element (test with Is_Null)

   function New_Torrent return Detached_Torrent'Class;

   ---------------------
   -- Elements: Users --
   ---------------------

   function "=" (Op1 : User; Op2 : User) return Boolean;
   function "=" (Op1 : Detached_User; Op2 : Detached_User) return Boolean;
   --  Compares two elements using only the primary keys. All other fields are
   --  ignored

   function Downloaded (Self : User) return Long_Long_Integer;
   function Downloaded (Self : Detached_User) return Long_Long_Integer;
   procedure Set_Downloaded (Self : Detached_User; Value : Long_Long_Integer);

   function Id (Self : User) return Integer;
   function Id (Self : Detached_User) return Integer;

   function Passkey (Self : User) return String;
   function Passkey (Self : Detached_User) return String;
   procedure Set_Passkey (Self : Detached_User; Value : String);

   function Password (Self : User) return String;
   function Password (Self : Detached_User) return String;
   procedure Set_Password (Self : Detached_User; Value : String);

   function Role (Self : User) return Integer;
   function Role (Self : Detached_User) return Integer;
   procedure Set_Role (Self : Detached_User; Value : Integer);
   --  0 = user, 1 = admin

   function Uploaded (Self : User) return Long_Long_Integer;
   function Uploaded (Self : Detached_User) return Long_Long_Integer;
   procedure Set_Uploaded (Self : Detached_User; Value : Long_Long_Integer);

   function Username (Self : User) return String;
   function Username (Self : Detached_User) return String;
   procedure Set_Username (Self : Detached_User; Value : String);

   function Detach (Self : User'Class) return Detached_User'Class;

   function From_Cache
     (Session : Session_Type;
      Id      : Integer)
     return Detached_User'Class;
   --  Check whether there is already an element with this primary key. If
   --  not, the returned value will be a null element (test with Is_Null)

   function New_User return Detached_User'Class;

   ----------------------
   -- Elements: Config --
   ----------------------

   function "=" (Op1 : Config; Op2 : Config) return Boolean;
   function "=" (Op1 : Detached_Config; Op2 : Detached_Config) return Boolean;
   --  Compares two elements using only the primary keys. All other fields are
   --  ignored

   function Id (Self : Config) return Integer;
   function Id (Self : Detached_Config) return Integer;

   function Version (Self : Config) return Integer;
   function Version (Self : Detached_Config) return Integer;
   procedure Set_Version (Self : Detached_Config; Value : Integer);

   function Detach (Self : Config'Class) return Detached_Config'Class;

   function From_Cache
     (Session : Session_Type;
      Id      : Integer)
     return Detached_Config'Class;
   --  Check whether there is already an element with this primary key. If
   --  not, the returned value will be a null element (test with Is_Null)

   function New_Config return Detached_Config'Class;

   ----------------------------------
   -- Elements: User_Torrent_Stats --
   ----------------------------------

   function By_User (Self : User_Torrent_Stat) return Integer;
   function By_User (Self : Detached_User_Torrent_Stat) return Integer;
   procedure Set_By_User (Self : Detached_User_Torrent_Stat; Value : Integer);
   function By_User (Self : User_Torrent_Stat) return User'Class;
   function By_User
     (Self : Detached_User_Torrent_Stat)
     return Detached_User'Class;
   procedure Set_By_User
     (Self  : Detached_User_Torrent_Stat;
      Value : Detached_User'Class);

   function Downloaded (Self : User_Torrent_Stat) return Long_Long_Integer;
   function Downloaded
     (Self : Detached_User_Torrent_Stat)
     return Long_Long_Integer;
   procedure Set_Downloaded
     (Self  : Detached_User_Torrent_Stat;
      Value : Long_Long_Integer);

   function Of_Torrent (Self : User_Torrent_Stat) return Integer;
   function Of_Torrent (Self : Detached_User_Torrent_Stat) return Integer;
   procedure Set_Of_Torrent (Self : Detached_User_Torrent_Stat; Value : Integer);
   function Of_Torrent (Self : User_Torrent_Stat) return Torrent'Class;
   function Of_Torrent
     (Self : Detached_User_Torrent_Stat)
     return Detached_Torrent'Class;
   procedure Set_Of_Torrent
     (Self  : Detached_User_Torrent_Stat;
      Value : Detached_Torrent'Class);

   function Snatched (Self : User_Torrent_Stat) return Boolean;
   function Snatched (Self : Detached_User_Torrent_Stat) return Boolean;
   procedure Set_Snatched (Self : Detached_User_Torrent_Stat; Value : Boolean);

   function Uploaded (Self : User_Torrent_Stat) return Long_Long_Integer;
   function Uploaded
     (Self : Detached_User_Torrent_Stat)
     return Long_Long_Integer;
   procedure Set_Uploaded
     (Self  : Detached_User_Torrent_Stat;
      Value : Long_Long_Integer);

   function Detach
     (Self : User_Torrent_Stat'Class)
     return Detached_User_Torrent_Stat'Class;

   function New_User_Torrent_Stat return Detached_User_Torrent_Stat'Class;

   -----------------------
   -- Elements: Invites --
   -----------------------

   function "=" (Op1 : Invite; Op2 : Invite) return Boolean;
   function "=" (Op1 : Detached_Invite; Op2 : Detached_Invite) return Boolean;
   --  Compares two elements using only the primary keys. All other fields are
   --  ignored

   function Activated (Self : Invite) return Boolean;
   function Activated (Self : Detached_Invite) return Boolean;
   procedure Set_Activated (Self : Detached_Invite; Value : Boolean);

   function By_User (Self : Invite) return Integer;
   function By_User (Self : Detached_Invite) return Integer;
   procedure Set_By_User (Self : Detached_Invite; Value : Integer);
   function By_User (Self : Invite) return User'Class;
   function By_User (Self : Detached_Invite) return Detached_User'Class;
   procedure Set_By_User (Self : Detached_Invite; Value : Detached_User'Class);

   function For_User (Self : Invite) return Integer;
   function For_User (Self : Detached_Invite) return Integer;
   procedure Set_For_User (Self : Detached_Invite; Value : Integer);
   function For_User (Self : Invite) return User'Class;
   function For_User (Self : Detached_Invite) return Detached_User'Class;
   procedure Set_For_User (Self : Detached_Invite; Value : Detached_User'Class);

   function Id (Self : Invite) return Integer;
   function Id (Self : Detached_Invite) return Integer;

   function Value (Self : Invite) return String;
   function Value (Self : Detached_Invite) return String;
   procedure Set_Value (Self : Detached_Invite; Value : String);

   function Detach (Self : Invite'Class) return Detached_Invite'Class;

   function From_Cache
     (Session : Session_Type;
      Id      : Integer)
     return Detached_Invite'Class;
   --  Check whether there is already an element with this primary key. If
   --  not, the returned value will be a null element (test with Is_Null)

   function New_Invite return Detached_Invite'Class;

   ---------------------
   -- Elements: Posts --
   ---------------------

   function "=" (Op1 : Post; Op2 : Post) return Boolean;
   function "=" (Op1 : Detached_Post; Op2 : Detached_Post) return Boolean;
   --  Compares two elements using only the primary keys. All other fields are
   --  ignored

   function By_User (Self : Post) return Integer;
   function By_User (Self : Detached_Post) return Integer;
   procedure Set_By_User (Self : Detached_Post; Value : Integer);
   function By_User (Self : Post) return User'Class;
   function By_User (Self : Detached_Post) return Detached_User'Class;
   procedure Set_By_User (Self : Detached_Post; Value : Detached_User'Class);

   function Content (Self : Post) return String;
   function Content (Self : Detached_Post) return String;
   procedure Set_Content (Self : Detached_Post; Value : String);

   function Flag (Self : Post) return Integer;
   function Flag (Self : Detached_Post) return Integer;
   procedure Set_Flag (Self : Detached_Post; Value : Integer);
   --  0 = nothing, 1 = news, 2 = request/offer

   function Id (Self : Post) return Integer;
   function Id (Self : Detached_Post) return Integer;

   function Parent_Post (Self : Post) return Integer;
   function Parent_Post (Self : Detached_Post) return Integer;
   procedure Set_Parent_Post (Self : Detached_Post; Value : Integer);
   function Parent_Post (Self : Post) return Post'Class;
   function Parent_Post (Self : Detached_Post) return Detached_Post'Class;
   procedure Set_Parent_Post (Self : Detached_Post; Value : Detached_Post'Class);
   --  The ID of the post that started the thread

   function Parent_Torrent (Self : Post) return Integer;
   function Parent_Torrent (Self : Detached_Post) return Integer;
   procedure Set_Parent_Torrent (Self : Detached_Post; Value : Integer);
   function Parent_Torrent (Self : Post) return Torrent'Class;
   function Parent_Torrent (Self : Detached_Post) return Detached_Torrent'Class;
   procedure Set_Parent_Torrent
     (Self  : Detached_Post;
      Value : Detached_Torrent'Class);
   --  The ID of the torrent that started the thread

   function Title (Self : Post) return String;
   function Title (Self : Detached_Post) return String;
   procedure Set_Title (Self : Detached_Post; Value : String);
   --  NULL for thread replies

   function Detach (Self : Post'Class) return Detached_Post'Class;

   function From_Cache
     (Session : Session_Type;
      Id      : Integer)
     return Detached_Post'Class;
   --  Check whether there is already an element with this primary key. If
   --  not, the returned value will be a null element (test with Is_Null)

   function New_Post return Detached_Post'Class;

   -------------------------
   -- Elements: Peer_Data --
   -------------------------

   function "=" (Op1 : Peer_Data; Op2 : Peer_Data) return Boolean;
   function "="
     (Op1 : Detached_Peer_Data;
      Op2 : Detached_Peer_Data)
     return Boolean;
   --  Compares two elements using only the primary keys. All other fields are
   --  ignored

   function Data (Self : Peer_Data) return String;
   function Data (Self : Detached_Peer_Data) return String;
   procedure Set_Data (Self : Detached_Peer_Data; Value : String);

   function Torrent_Id (Self : Peer_Data) return Integer;
   function Torrent_Id (Self : Detached_Peer_Data) return Integer;
   function Torrent_Id (Self : Peer_Data) return Torrent'Class;
   function Torrent_Id (Self : Detached_Peer_Data) return Detached_Torrent'Class;
   procedure Set_Torrent_Id
     (Self  : Detached_Peer_Data;
      Value : Detached_Torrent'Class);

   function Detach (Self : Peer_Data'Class) return Detached_Peer_Data'Class;

   function From_Cache
     (Session    : Session_Type;
      Torrent_Id : Integer)
     return Detached_Peer_Data'Class;
   --  Check whether there is already an element with this primary key. If
   --  not, the returned value will be a null element (test with Is_Null)

   function New_Peer_Data return Detached_Peer_Data'Class;

   -----------------------------
   -- Elements: Image_Uploads --
   -----------------------------

   function "=" (Op1 : Image_Upload; Op2 : Image_Upload) return Boolean;
   function "="
     (Op1 : Detached_Image_Upload;
      Op2 : Detached_Image_Upload)
     return Boolean;
   --  Compares two elements using only the primary keys. All other fields are
   --  ignored

   function By_User (Self : Image_Upload) return Integer;
   function By_User (Self : Detached_Image_Upload) return Integer;
   procedure Set_By_User (Self : Detached_Image_Upload; Value : Integer);
   function By_User (Self : Image_Upload) return User'Class;
   function By_User (Self : Detached_Image_Upload) return Detached_User'Class;
   procedure Set_By_User
     (Self  : Detached_Image_Upload;
      Value : Detached_User'Class);

   function Filename (Self : Image_Upload) return String;
   function Filename (Self : Detached_Image_Upload) return String;
   procedure Set_Filename (Self : Detached_Image_Upload; Value : String);

   function Id (Self : Image_Upload) return Integer;
   function Id (Self : Detached_Image_Upload) return Integer;

   function Detach
     (Self : Image_Upload'Class)
     return Detached_Image_Upload'Class;

   function From_Cache
     (Session : Session_Type;
      Id      : Integer)
     return Detached_Image_Upload'Class;
   --  Check whether there is already an element with this primary key. If
   --  not, the returned value will be a null element (test with Is_Null)

   function New_Image_Upload return Detached_Image_Upload'Class;

   --------------------------------------
   -- Managers(Implementation Details) --
   --------------------------------------

   procedure Internal_Query_Config
     (Fields    : in out SQL_Field_List;
      From      : out SQL_Table_List;
      Criteria  : in out Sql_Criteria;
      Depth     : Natural;
      Follow_LJ : Boolean;
      Pk_Only   : Boolean := False);

   procedure Internal_Query_Image_Uploads
     (Fields    : in out SQL_Field_List;
      From      : out SQL_Table_List;
      Criteria  : in out Sql_Criteria;
      Depth     : Natural;
      Follow_LJ : Boolean;
      Pk_Only   : Boolean := False);

   procedure Internal_Query_Invites
     (Fields    : in out SQL_Field_List;
      From      : out SQL_Table_List;
      Criteria  : in out Sql_Criteria;
      Depth     : Natural;
      Follow_LJ : Boolean;
      Pk_Only   : Boolean := False);

   procedure Internal_Query_Peer_Data
     (Fields    : in out SQL_Field_List;
      From      : out SQL_Table_List;
      Criteria  : in out Sql_Criteria;
      Depth     : Natural;
      Follow_LJ : Boolean;
      Pk_Only   : Boolean := False);

   procedure Internal_Query_Posts
     (Fields    : in out SQL_Field_List;
      From      : out SQL_Table_List;
      Criteria  : in out Sql_Criteria;
      Depth     : Natural;
      Follow_LJ : Boolean;
      Pk_Only   : Boolean := False);

   procedure Internal_Query_Torrents
     (Fields    : in out SQL_Field_List;
      From      : out SQL_Table_List;
      Criteria  : in out Sql_Criteria;
      Depth     : Natural;
      Follow_LJ : Boolean;
      Pk_Only   : Boolean := False);

   procedure Internal_Query_User_Torrent_Stats
     (Fields    : in out SQL_Field_List;
      From      : out SQL_Table_List;
      Criteria  : in out Sql_Criteria;
      Depth     : Natural;
      Follow_LJ : Boolean;
      Pk_Only   : Boolean := False);

   procedure Internal_Query_Users
     (Fields    : in out SQL_Field_List;
      From      : out SQL_Table_List;
      Criteria  : in out Sql_Criteria;
      Depth     : Natural;
      Follow_LJ : Boolean;
      Pk_Only   : Boolean := False);

   -------------------
   -- Manager Types --
   -------------------

   type I_Config_Managers is abstract new Manager with null record;
   package I_Config is new Generic_Managers
     (I_Config_Managers, Config, Related_Depth, DBA.Config,
      Internal_Query_Config);
   type Config_Managers is new I_Config.Manager with null record;
   subtype Config_Stmt is I_Config.ORM_Prepared_Statement;

   subtype Config_List is I_Config.List;
   subtype Direct_Config_List is I_Config.Direct_List;
   Empty_Config_List : constant Config_List := I_Config.Empty_List;
   Empty_Direct_Config_List : constant Direct_Config_List :=
   I_Config.Empty_Direct_List;

   type I_Image_Uploads_Managers is abstract new Manager with null record;
   package I_Image_Uploads is new Generic_Managers
     (I_Image_Uploads_Managers, Image_Upload, Related_Depth, DBA.Image_Uploads,
      Internal_Query_Image_Uploads);
   type Image_Uploads_Managers is new I_Image_Uploads.Manager with null record;
   subtype Image_Uploads_Stmt is I_Image_Uploads.ORM_Prepared_Statement;

   subtype Image_Upload_List is I_Image_Uploads.List;
   subtype Direct_Image_Upload_List is I_Image_Uploads.Direct_List;
   Empty_Image_Upload_List : constant Image_Upload_List := I_Image_Uploads.Empty_List;
   Empty_Direct_Image_Upload_List : constant Direct_Image_Upload_List :=
   I_Image_Uploads.Empty_Direct_List;

   type I_Invites_Managers is abstract new Manager with null record;
   package I_Invites is new Generic_Managers
     (I_Invites_Managers, Invite, Related_Depth, DBA.Invites,
      Internal_Query_Invites);
   type Invites_Managers is new I_Invites.Manager with null record;
   subtype Invites_Stmt is I_Invites.ORM_Prepared_Statement;

   subtype Invite_List is I_Invites.List;
   subtype Direct_Invite_List is I_Invites.Direct_List;
   Empty_Invite_List : constant Invite_List := I_Invites.Empty_List;
   Empty_Direct_Invite_List : constant Direct_Invite_List :=
   I_Invites.Empty_Direct_List;

   type I_Peer_Data_Managers is abstract new Manager with null record;
   package I_Peer_Data is new Generic_Managers
     (I_Peer_Data_Managers, Peer_Data, Related_Depth, DBA.Peer_Data,
      Internal_Query_Peer_Data);
   type Peer_Data_Managers is new I_Peer_Data.Manager with null record;
   subtype Peer_Data_Stmt is I_Peer_Data.ORM_Prepared_Statement;

   subtype Peer_Data_List is I_Peer_Data.List;
   subtype Direct_Peer_Data_List is I_Peer_Data.Direct_List;
   Empty_Peer_Data_List : constant Peer_Data_List := I_Peer_Data.Empty_List;
   Empty_Direct_Peer_Data_List : constant Direct_Peer_Data_List :=
   I_Peer_Data.Empty_Direct_List;

   type I_Posts_Managers is abstract new Manager with null record;
   package I_Posts is new Generic_Managers
     (I_Posts_Managers, Post, Related_Depth, DBA.Posts,
      Internal_Query_Posts);
   type Posts_Managers is new I_Posts.Manager with null record;
   subtype Posts_Stmt is I_Posts.ORM_Prepared_Statement;

   subtype Post_List is I_Posts.List;
   subtype Direct_Post_List is I_Posts.Direct_List;
   Empty_Post_List : constant Post_List := I_Posts.Empty_List;
   Empty_Direct_Post_List : constant Direct_Post_List :=
   I_Posts.Empty_Direct_List;

   type I_Torrents_Managers is abstract new Manager with null record;
   package I_Torrents is new Generic_Managers
     (I_Torrents_Managers, Torrent, Related_Depth, DBA.Torrents,
      Internal_Query_Torrents);
   type Torrents_Managers is new I_Torrents.Manager with null record;
   subtype Torrents_Stmt is I_Torrents.ORM_Prepared_Statement;

   subtype Torrent_List is I_Torrents.List;
   subtype Direct_Torrent_List is I_Torrents.Direct_List;
   Empty_Torrent_List : constant Torrent_List := I_Torrents.Empty_List;
   Empty_Direct_Torrent_List : constant Direct_Torrent_List :=
   I_Torrents.Empty_Direct_List;

   type I_User_Torrent_Stats_Managers is abstract new Manager with null record;
   package I_User_Torrent_Stats is new Generic_Managers
     (I_User_Torrent_Stats_Managers, User_Torrent_Stat, Related_Depth, DBA.User_Torrent_Stats,
      Internal_Query_User_Torrent_Stats);
   type User_Torrent_Stats_Managers is new I_User_Torrent_Stats.Manager with null record;
   subtype User_Torrent_Stats_Stmt is I_User_Torrent_Stats.ORM_Prepared_Statement;

   subtype User_Torrent_Stat_List is I_User_Torrent_Stats.List;
   subtype Direct_User_Torrent_Stat_List is I_User_Torrent_Stats.Direct_List;
   Empty_User_Torrent_Stat_List : constant User_Torrent_Stat_List := I_User_Torrent_Stats.Empty_List;
   Empty_Direct_User_Torrent_Stat_List : constant Direct_User_Torrent_Stat_List :=
   I_User_Torrent_Stats.Empty_Direct_List;

   type I_Users_Managers is abstract new Manager with null record;
   package I_Users is new Generic_Managers
     (I_Users_Managers, User, Related_Depth, DBA.Users,
      Internal_Query_Users);
   type Users_Managers is new I_Users.Manager with null record;
   subtype Users_Stmt is I_Users.ORM_Prepared_Statement;

   subtype User_List is I_Users.List;
   subtype Direct_User_List is I_Users.Direct_List;
   Empty_User_List : constant User_List := I_Users.Empty_List;
   Empty_Direct_User_List : constant Direct_User_List :=
   I_Users.Empty_Direct_List;


   -----------------------
   -- Manager: Torrents --
   -----------------------

   function User_Torrent_Stats_Of_Torrent_Id
     (Self : Torrent'Class)
     return User_Torrent_Stats_Managers;
   function User_Torrent_Stats_Of_Torrent_Id
     (Self : Detached_Torrent'Class)
     return User_Torrent_Stats_Managers;
   function User_Torrent_Stats_Of_Torrent_Id
     (Self : I_Torrents_Managers'Class)
     return User_Torrent_Stats_Managers;

   function Comments (Self : Torrent'Class) return Posts_Managers;
   function Comments (Self : Detached_Torrent'Class) return Posts_Managers;
   function Comments (Self : I_Torrents_Managers'Class) return Posts_Managers;

   function Filter
     (Self         : Torrents_Managers'Class;
      Id           : Integer := -1;
      Info_Hash    : String := No_Update;
      Created_By   : Integer := -1;
      Display_Name : String := No_Update;
      Description  : String := No_Update;
      Category     : Integer := -1)
     return Torrents_Managers;

   function Get_Torrent
     (Session          : Session_Type;
      Id               : Integer;
      Depth            : Related_Depth := 0;
      Follow_Left_Join : Boolean := False)
     return Detached_Torrent'Class;

   function Torrent_Peers (Self : Torrent'Class) return Peer_Data_Managers;
   function Torrent_Peers
     (Self : Detached_Torrent'Class)
     return Peer_Data_Managers;
   function Torrent_Peers
     (Self : I_Torrents_Managers'Class)
     return Peer_Data_Managers;

   --------------------
   -- Manager: Users --
   --------------------

   function Created_Posts (Self : User'Class) return Posts_Managers;
   function Created_Posts (Self : Detached_User'Class) return Posts_Managers;
   function Created_Posts (Self : I_Users_Managers'Class) return Posts_Managers;

   function Created_Torrents (Self : User'Class) return Torrents_Managers;
   function Created_Torrents
     (Self : Detached_User'Class)
     return Torrents_Managers;
   function Created_Torrents
     (Self : I_Users_Managers'Class)
     return Torrents_Managers;

   function Filter
     (Self       : Users_Managers'Class;
      Id         : Integer := -1;
      Username   : String := No_Update;
      Password   : String := No_Update;
      Passkey    : String := No_Update;
      Uploaded   : Long_Long_Integer := -1;
      Downloaded : Long_Long_Integer := -1;
      Role       : Integer := -1)
     return Users_Managers;

   function Get_User
     (Session          : Session_Type;
      Id               : Integer;
      Depth            : Related_Depth := 0;
      Follow_Left_Join : Boolean := False)
     return Detached_User'Class;

   function Invited_By (Self : User'Class) return Invites_Managers;
   function Invited_By (Self : Detached_User'Class) return Invites_Managers;
   function Invited_By (Self : I_Users_Managers'Class) return Invites_Managers;

   function Invites (Self : User'Class) return Invites_Managers;
   function Invites (Self : Detached_User'Class) return Invites_Managers;
   function Invites (Self : I_Users_Managers'Class) return Invites_Managers;

   function Torrent_Stats (Self : User'Class) return User_Torrent_Stats_Managers;
   function Torrent_Stats
     (Self : Detached_User'Class)
     return User_Torrent_Stats_Managers;
   function Torrent_Stats
     (Self : I_Users_Managers'Class)
     return User_Torrent_Stats_Managers;

   function Uploaded_Images (Self : User'Class) return Image_Uploads_Managers;
   function Uploaded_Images
     (Self : Detached_User'Class)
     return Image_Uploads_Managers;
   function Uploaded_Images
     (Self : I_Users_Managers'Class)
     return Image_Uploads_Managers;

   ---------------------
   -- Manager: Config --
   ---------------------

   function Filter
     (Self    : Config_Managers'Class;
      Id      : Integer := -1;
      Version : Integer := -1)
     return Config_Managers;

   function Get_Config
     (Session          : Session_Type;
      Id               : Integer;
      Depth            : Related_Depth := 0;
      Follow_Left_Join : Boolean := False)
     return Detached_Config'Class;

   ---------------------------------
   -- Manager: User_Torrent_Stats --
   ---------------------------------

   function Filter
     (Self       : User_Torrent_Stats_Managers'Class;
      By_User    : Integer := -1;
      Of_Torrent : Integer := -1;
      Uploaded   : Long_Long_Integer := -1;
      Downloaded : Long_Long_Integer := -1;
      Snatched   : Triboolean := Indeterminate)
     return User_Torrent_Stats_Managers;

   ----------------------
   -- Manager: Invites --
   ----------------------

   function Filter
     (Self      : Invites_Managers'Class;
      Id        : Integer := -1;
      Value     : String := No_Update;
      Activated : Triboolean := Indeterminate;
      By_User   : Integer := -1;
      For_User  : Integer := -1)
     return Invites_Managers;

   function Get_Invite
     (Session          : Session_Type;
      Id               : Integer;
      Depth            : Related_Depth := 0;
      Follow_Left_Join : Boolean := False)
     return Detached_Invite'Class;

   --------------------
   -- Manager: Posts --
   --------------------

   function Children_Posts (Self : Post'Class) return Posts_Managers;
   function Children_Posts (Self : Detached_Post'Class) return Posts_Managers;
   function Children_Posts (Self : I_Posts_Managers'Class) return Posts_Managers;

   function Filter
     (Self           : Posts_Managers'Class;
      Id             : Integer := -1;
      Title          : String := No_Update;
      Content        : String := No_Update;
      By_User        : Integer := -1;
      Parent_Post    : Integer := -1;
      Flag           : Integer := -1;
      Parent_Torrent : Integer := -1)
     return Posts_Managers;

   function Get_Post
     (Session          : Session_Type;
      Id               : Integer;
      Depth            : Related_Depth := 0;
      Follow_Left_Join : Boolean := False)
     return Detached_Post'Class;

   ------------------------
   -- Manager: Peer_Data --
   ------------------------

   function Filter
     (Self       : Peer_Data_Managers'Class;
      Torrent_Id : Integer := -1;
      Data       : String := No_Update)
     return Peer_Data_Managers;

   function Get_Peer_Data
     (Session          : Session_Type;
      Torrent_Id       : Integer;
      Depth            : Related_Depth := 0;
      Follow_Left_Join : Boolean := False)
     return Detached_Peer_Data'Class;

   ----------------------------
   -- Manager: Image_Uploads --
   ----------------------------

   function Filter
     (Self     : Image_Uploads_Managers'Class;
      Id       : Integer := -1;
      By_User  : Integer := -1;
      Filename : String := No_Update)
     return Image_Uploads_Managers;

   function Get_Image_Upload
     (Session          : Session_Type;
      Id               : Integer;
      Depth            : Related_Depth := 0;
      Follow_Left_Join : Boolean := False)
     return Detached_Image_Upload'Class;

   --------------
   -- Managers --
   --------------

   All_Config : constant Config_Managers :=
     (I_Config.All_Managers with null record);

   All_Image_Uploads : constant Image_Uploads_Managers :=
     (I_Image_Uploads.All_Managers with null record);

   All_Invites : constant Invites_Managers :=
     (I_Invites.All_Managers with null record);

   All_Peer_Data : constant Peer_Data_Managers :=
     (I_Peer_Data.All_Managers with null record);

   All_Posts : constant Posts_Managers :=
     (I_Posts.All_Managers with null record);

   All_Torrents : constant Torrents_Managers :=
     (I_Torrents.All_Managers with null record);

   All_User_Torrent_Stats : constant User_Torrent_Stats_Managers :=
     (I_User_Torrent_Stats.All_Managers with null record);

   All_Users : constant Users_Managers :=
     (I_Users.All_Managers with null record);


   --------------
   -- Internal --
   --------------

   overriding procedure Free (Self : in out Config_Ddr);
   overriding procedure Free (Self : in out Image_Upload_Ddr);
   overriding procedure Free (Self : in out Invite_Ddr);
   overriding procedure Free (Self : in out Peer_Data_Ddr);
   overriding procedure Free (Self : in out Post_Ddr);
   overriding procedure Free (Self : in out Torrent_Ddr);
   overriding procedure Free (Self : in out User_Torrent_Stat_Ddr);
   overriding procedure Free (Self : in out User_Ddr);

   overriding procedure Insert_Or_Update
     (Self        : in out Detached_Config;
      Pk_Modified : in out Boolean;
      Mask        : Dirty_Mask);
   overriding procedure Insert_Or_Update
     (Self        : in out Detached_Image_Upload;
      Pk_Modified : in out Boolean;
      Mask        : Dirty_Mask);
   overriding procedure Insert_Or_Update
     (Self        : in out Detached_Invite;
      Pk_Modified : in out Boolean;
      Mask        : Dirty_Mask);
   overriding procedure Insert_Or_Update
     (Self        : in out Detached_Peer_Data;
      Pk_Modified : in out Boolean;
      Mask        : Dirty_Mask);
   overriding procedure Insert_Or_Update
     (Self        : in out Detached_Post;
      Pk_Modified : in out Boolean;
      Mask        : Dirty_Mask);
   overriding procedure Insert_Or_Update
     (Self        : in out Detached_Torrent;
      Pk_Modified : in out Boolean;
      Mask        : Dirty_Mask);
   overriding procedure Insert_Or_Update
     (Self        : in out Detached_User_Torrent_Stat;
      Pk_Modified : in out Boolean;
      Mask        : Dirty_Mask);
   overriding procedure Insert_Or_Update
     (Self        : in out Detached_User;
      Pk_Modified : in out Boolean;
      Mask        : Dirty_Mask);

   overriding procedure Internal_Delete (Self : Detached_Config);
   overriding procedure Internal_Delete (Self : Detached_Image_Upload);
   overriding procedure Internal_Delete (Self : Detached_Invite);
   overriding procedure Internal_Delete (Self : Detached_Peer_Data);
   overriding procedure Internal_Delete (Self : Detached_Post);
   overriding procedure Internal_Delete (Self : Detached_Torrent);
   overriding procedure Internal_Delete (Self : Detached_User_Torrent_Stat);
   overriding procedure Internal_Delete (Self : Detached_User);

   overriding function Key (Self : Config_Ddr) return Element_Key;
   overriding function Key (Self : Image_Upload_Ddr) return Element_Key;
   overriding function Key (Self : Invite_Ddr) return Element_Key;
   overriding function Key (Self : Peer_Data_Ddr) return Element_Key;
   overriding function Key (Self : Post_Ddr) return Element_Key;
   overriding function Key (Self : Torrent_Ddr) return Element_Key;
   overriding function Key (Self : User_Torrent_Stat_Ddr) return Element_Key;
   overriding function Key (Self : User_Ddr) return Element_Key;

   overriding procedure On_Persist (Self : Detached_Image_Upload);
   overriding procedure On_Persist (Self : Detached_Invite);
   overriding procedure On_Persist (Self : Detached_Peer_Data);
   overriding procedure On_Persist (Self : Detached_Post);
   overriding procedure On_Persist (Self : Detached_Torrent);
   overriding procedure On_Persist (Self : Detached_User_Torrent_Stat);

private

    type Config_DDR is new Detached_Data (2) with record
       ORM_Id         : Integer := -1;
       ORM_Version    : Integer := -1;
    end record;
    type Config_Data is access all Config_DDR;
    
    type Image_Upload_DDR is new Detached_Data (4) with record
       ORM_By_User     : Integer := -1;
       ORM_FK_By_User  : Detached_User_Access := null;
       ORM_Filename    : Unbounded_String := Null_Unbounded_String;
       ORM_Id          : Integer := -1;
    end record;
    type Image_Upload_Data is access all Image_Upload_DDR;
    
    type Invite_DDR is new Detached_Data (7) with record
       ORM_Activated    : Boolean := False;
       ORM_By_User      : Integer := -1;
       ORM_FK_By_User   : Detached_User_Access := null;
       ORM_FK_For_User  : Detached_User_Access := null;
       ORM_For_User     : Integer := -1;
       ORM_Id           : Integer := -1;
       ORM_Value        : Unbounded_String := Null_Unbounded_String;
    end record;
    type Invite_Data is access all Invite_DDR;
    
    type Peer_Data_DDR is new Detached_Data (3) with record
       ORM_Data          : Unbounded_String := Null_Unbounded_String;
       ORM_FK_Torrent_Id : Detached_Torrent_Access := null;
       ORM_Torrent_Id    : Integer := -1;
    end record;
    type Peer_Data_Data is access all Peer_Data_DDR;
    
    type Post_DDR is new Detached_Data (10) with record
       ORM_By_User           : Integer := -1;
       ORM_Content           : Unbounded_String := Null_Unbounded_String;
       ORM_FK_By_User        : Detached_User_Access := null;
       ORM_FK_Parent_Post    : Detached_Post_Access := null;
       ORM_FK_Parent_Torrent : Detached_Torrent_Access := null;
       ORM_Flag              : Integer := 0;
       ORM_Id                : Integer := -1;
       ORM_Parent_Post       : Integer := -1;
       ORM_Parent_Torrent    : Integer := -1;
       ORM_Title             : Unbounded_String := Null_Unbounded_String;
    end record;
    type Post_Data is access all Post_DDR;
    
    type Torrent_DDR is new Detached_Data (7) with record
       ORM_Category        : Integer := 0;
       ORM_Created_By      : Integer := -1;
       ORM_Description     : Unbounded_String := Null_Unbounded_String;
       ORM_Display_Name    : Unbounded_String := Null_Unbounded_String;
       ORM_FK_Created_By   : Detached_User_Access := null;
       ORM_Id              : Integer := -1;
       ORM_Info_Hash       : Unbounded_String := Null_Unbounded_String;
    end record;
    type Torrent_Data is access all Torrent_DDR;
    
    type User_Torrent_Stat_DDR is new Detached_Data (7) with record
       ORM_By_User       : Integer := -1;
       ORM_Downloaded    : Long_Long_Integer := -1;
       ORM_FK_By_User    : Detached_User_Access := null;
       ORM_FK_Of_Torrent : Detached_Torrent_Access := null;
       ORM_Of_Torrent    : Integer := -1;
       ORM_Snatched      : Boolean := False;
       ORM_Uploaded      : Long_Long_Integer := -1;
    end record;
    type User_Torrent_Stat_Data is access all User_Torrent_Stat_DDR;
    
    type User_DDR is new Detached_Data (7) with record
       ORM_Downloaded    : Long_Long_Integer := 0;
       ORM_Id            : Integer := -1;
       ORM_Passkey       : Unbounded_String := Null_Unbounded_String;
       ORM_Password      : Unbounded_String := Null_Unbounded_String;
       ORM_Role          : Integer := 0;
       ORM_Uploaded      : Long_Long_Integer := 0;
       ORM_Username      : Unbounded_String := Null_Unbounded_String;
    end record;
    type User_Data is access all User_DDR;
    

    type Detached_Config
       is new Sessions.Detached_Element with null record;
    No_Config : constant Config :=(No_Orm_Element with null record);
    No_Detached_Config : constant Detached_Config :=
      (Sessions.Detached_Element with null record);
 
    type Detached_Image_Upload
       is new Sessions.Detached_Element with null record;
    No_Image_Upload : constant Image_Upload :=(No_Orm_Element with null record);
    No_Detached_Image_Upload : constant Detached_Image_Upload :=
      (Sessions.Detached_Element with null record);
 
    type Detached_Invite
       is new Sessions.Detached_Element with null record;
    No_Invite : constant Invite :=(No_Orm_Element with null record);
    No_Detached_Invite : constant Detached_Invite :=
      (Sessions.Detached_Element with null record);
 
    type Detached_Peer_Data
       is new Sessions.Detached_Element with null record;
    No_Peer_Data : constant Peer_Data :=(No_Orm_Element with null record);
    No_Detached_Peer_Data : constant Detached_Peer_Data :=
      (Sessions.Detached_Element with null record);
 
    type Detached_Post
       is new Sessions.Detached_Element with null record;
    No_Post : constant Post :=(No_Orm_Element with null record);
    No_Detached_Post : constant Detached_Post :=
      (Sessions.Detached_Element with null record);
 
    type Detached_Torrent
       is new Sessions.Detached_Element with null record;
    No_Torrent : constant Torrent :=(No_Orm_Element with null record);
    No_Detached_Torrent : constant Detached_Torrent :=
      (Sessions.Detached_Element with null record);
 
    type Detached_User_Torrent_Stat
       is new Sessions.Detached_Element with null record;
    No_User_Torrent_Stat : constant User_Torrent_Stat :=(No_Orm_Element with null record);
    No_Detached_User_Torrent_Stat : constant Detached_User_Torrent_Stat :=
      (Sessions.Detached_Element with null record);
 
    type Detached_User
       is new Sessions.Detached_Element with null record;
    No_User : constant User :=(No_Orm_Element with null record);
    No_Detached_User : constant Detached_User :=
      (Sessions.Detached_Element with null record);
 
end Orm;
