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

   type Invite is new Orm_Element with null record;
   type Invite_DDR is new Detached_Data (7) with private;
   type Detached_Invite is  --  Get() returns a Invite_DDR
   new Sessions.Detached_Element with private;
   type Detached_Invite_Access is access all Detached_Invite'Class;
   No_Detached_Invite : constant Detached_Invite;
   No_Invite : constant Invite;

   type Torrent is new Orm_Element with null record;
   type Torrent_DDR is new Detached_Data (4) with private;
   type Detached_Torrent is  --  Get() returns a Torrent_DDR
   new Sessions.Detached_Element with private;
   type Detached_Torrent_Access is access all Detached_Torrent'Class;
   No_Detached_Torrent : constant Detached_Torrent;
   No_Torrent : constant Torrent;

   type User_Torrent_Stat is new Orm_Element with null record;
   type User_Torrent_Stat_DDR is new Detached_Data (6) with private;
   type Detached_User_Torrent_Stat is  --  Get() returns a User_Torrent_Stat_DDR
   new Sessions.Detached_Element with private;
   type Detached_User_Torrent_Stat_Access is access all Detached_User_Torrent_Stat'Class;
   No_Detached_User_Torrent_Stat : constant Detached_User_Torrent_Stat;
   No_User_Torrent_Stat : constant User_Torrent_Stat;

   type User is new Orm_Element with null record;
   type User_DDR is new Detached_Data (6) with private;
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

   function Created_By (Self : Torrent) return Integer;
   function Created_By (Self : Detached_Torrent) return Integer;
   procedure Set_Created_By (Self : Detached_Torrent; Value : Integer);
   function Created_By (Self : Torrent) return User'Class;
   function Created_By (Self : Detached_Torrent) return Detached_User'Class;
   procedure Set_Created_By
     (Self  : Detached_Torrent;
      Value : Detached_User'Class);

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

   function Downloaded (Self : User) return Integer;
   function Downloaded (Self : Detached_User) return Integer;
   procedure Set_Downloaded (Self : Detached_User; Value : Integer);

   function Id (Self : User) return Integer;
   function Id (Self : Detached_User) return Integer;

   function Passkey (Self : User) return String;
   function Passkey (Self : Detached_User) return String;
   procedure Set_Passkey (Self : Detached_User; Value : String);

   function Password (Self : User) return String;
   function Password (Self : Detached_User) return String;
   procedure Set_Password (Self : Detached_User; Value : String);

   function Uploaded (Self : User) return Integer;
   function Uploaded (Self : Detached_User) return Integer;
   procedure Set_Uploaded (Self : Detached_User; Value : Integer);

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

   function Downloaded (Self : User_Torrent_Stat) return Integer;
   function Downloaded (Self : Detached_User_Torrent_Stat) return Integer;
   procedure Set_Downloaded (Self : Detached_User_Torrent_Stat; Value : Integer);

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

   function Uploaded (Self : User_Torrent_Stat) return Integer;
   function Uploaded (Self : Detached_User_Torrent_Stat) return Integer;
   procedure Set_Uploaded (Self : Detached_User_Torrent_Stat; Value : Integer);

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

   procedure Internal_Query_Invites
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

   function Filter
     (Self       : Torrents_Managers'Class;
      Id         : Integer := -1;
      Info_Hash  : String := No_Update;
      Created_By : Integer := -1)
     return Torrents_Managers;

   function Get_Torrent
     (Session          : Session_Type;
      Id               : Integer;
      Depth            : Related_Depth := 0;
      Follow_Left_Join : Boolean := False)
     return Detached_Torrent'Class;

   --------------------
   -- Manager: Users --
   --------------------

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
      Uploaded   : Integer := -1;
      Downloaded : Integer := -1)
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
      Uploaded   : Integer := -1;
      Downloaded : Integer := -1)
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

   --------------
   -- Managers --
   --------------

   All_Config : constant Config_Managers :=
     (I_Config.All_Managers with null record);

   All_Invites : constant Invites_Managers :=
     (I_Invites.All_Managers with null record);

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
   overriding procedure Free (Self : in out Invite_Ddr);
   overriding procedure Free (Self : in out Torrent_Ddr);
   overriding procedure Free (Self : in out User_Torrent_Stat_Ddr);
   overriding procedure Free (Self : in out User_Ddr);

   overriding procedure Insert_Or_Update
     (Self        : in out Detached_Config;
      Pk_Modified : in out Boolean;
      Mask        : Dirty_Mask);
   overriding procedure Insert_Or_Update
     (Self        : in out Detached_Invite;
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
   overriding procedure Internal_Delete (Self : Detached_Invite);
   overriding procedure Internal_Delete (Self : Detached_Torrent);
   overriding procedure Internal_Delete (Self : Detached_User_Torrent_Stat);
   overriding procedure Internal_Delete (Self : Detached_User);

   overriding function Key (Self : Config_Ddr) return Element_Key;
   overriding function Key (Self : Invite_Ddr) return Element_Key;
   overriding function Key (Self : Torrent_Ddr) return Element_Key;
   overriding function Key (Self : User_Torrent_Stat_Ddr) return Element_Key;
   overriding function Key (Self : User_Ddr) return Element_Key;

   overriding procedure On_Persist (Self : Detached_Invite);
   overriding procedure On_Persist (Self : Detached_Torrent);
   overriding procedure On_Persist (Self : Detached_User_Torrent_Stat);

private

    type Config_DDR is new Detached_Data (2) with record
       ORM_Id         : Integer := -1;
       ORM_Version    : Integer := -1;
    end record;
    type Config_Data is access all Config_DDR;
    
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
    
    type Torrent_DDR is new Detached_Data (4) with record
       ORM_Created_By    : Integer := -1;
       ORM_FK_Created_By : Detached_User_Access := null;
       ORM_Id            : Integer := -1;
       ORM_Info_Hash     : Unbounded_String := Null_Unbounded_String;
    end record;
    type Torrent_Data is access all Torrent_DDR;
    
    type User_Torrent_Stat_DDR is new Detached_Data (6) with record
       ORM_By_User       : Integer := -1;
       ORM_Downloaded    : Integer := -1;
       ORM_FK_By_User    : Detached_User_Access := null;
       ORM_FK_Of_Torrent : Detached_Torrent_Access := null;
       ORM_Of_Torrent    : Integer := -1;
       ORM_Uploaded      : Integer := -1;
    end record;
    type User_Torrent_Stat_Data is access all User_Torrent_Stat_DDR;
    
    type User_DDR is new Detached_Data (6) with record
       ORM_Downloaded    : Integer := 0;
       ORM_Id            : Integer := -1;
       ORM_Passkey       : Unbounded_String := Null_Unbounded_String;
       ORM_Password      : Unbounded_String := Null_Unbounded_String;
       ORM_Uploaded      : Integer := 0;
       ORM_Username      : Unbounded_String := Null_Unbounded_String;
    end record;
    type User_Data is access all User_DDR;
    

    type Detached_Config
       is new Sessions.Detached_Element with null record;
    No_Config : constant Config :=(No_Orm_Element with null record);
    No_Detached_Config : constant Detached_Config :=
      (Sessions.Detached_Element with null record);
 
    type Detached_Invite
       is new Sessions.Detached_Element with null record;
    No_Invite : constant Invite :=(No_Orm_Element with null record);
    No_Detached_Invite : constant Detached_Invite :=
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
