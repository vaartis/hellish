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

   type Torrent is new Orm_Element with null record;
   type Torrent_DDR is new Detached_Data (5) with private;
   type Detached_Torrent is  --  Get() returns a Torrent_DDR
   new Sessions.Detached_Element with private;
   type Detached_Torrent_Access is access all Detached_Torrent'Class;
   No_Detached_Torrent : constant Detached_Torrent;
   No_Torrent : constant Torrent;

   type User is new Orm_Element with null record;
   type User_DDR is new Detached_Data (3) with private;
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

   function Filename (Self : Torrent) return String;
   function Filename (Self : Detached_Torrent) return String;
   procedure Set_Filename (Self : Detached_Torrent; Value : String);

   function Id (Self : Torrent) return Integer;
   function Id (Self : Detached_Torrent) return Integer;

   function Torrent_File (Self : Torrent) return String;
   function Torrent_File (Self : Detached_Torrent) return String;
   procedure Set_Torrent_File (Self : Detached_Torrent; Value : String);

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

   function Id (Self : User) return Integer;
   function Id (Self : Detached_User) return Integer;

   function Password (Self : User) return String;
   function Password (Self : Detached_User) return String;
   procedure Set_Password (Self : Detached_User; Value : String);

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

   procedure Internal_Query_Torrents
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

   function Filter
     (Self         : Torrents_Managers'Class;
      Id           : Integer := -1;
      Torrent_File : String := No_Update;
      Filename     : String := No_Update;
      Created_By   : Integer := -1)
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
     (Self     : Users_Managers'Class;
      Id       : Integer := -1;
      Username : String := No_Update;
      Password : String := No_Update)
     return Users_Managers;

   function Get_User
     (Session          : Session_Type;
      Id               : Integer;
      Depth            : Related_Depth := 0;
      Follow_Left_Join : Boolean := False)
     return Detached_User'Class;

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

   --------------
   -- Managers --
   --------------

   All_Config : constant Config_Managers :=
     (I_Config.All_Managers with null record);

   All_Torrents : constant Torrents_Managers :=
     (I_Torrents.All_Managers with null record);

   All_Users : constant Users_Managers :=
     (I_Users.All_Managers with null record);


   --------------
   -- Internal --
   --------------

   overriding procedure Free (Self : in out Config_Ddr);
   overriding procedure Free (Self : in out Torrent_Ddr);
   overriding procedure Free (Self : in out User_Ddr);

   overriding procedure Insert_Or_Update
     (Self        : in out Detached_Config;
      Pk_Modified : in out Boolean;
      Mask        : Dirty_Mask);
   overriding procedure Insert_Or_Update
     (Self        : in out Detached_Torrent;
      Pk_Modified : in out Boolean;
      Mask        : Dirty_Mask);
   overriding procedure Insert_Or_Update
     (Self        : in out Detached_User;
      Pk_Modified : in out Boolean;
      Mask        : Dirty_Mask);

   overriding procedure Internal_Delete (Self : Detached_Config);
   overriding procedure Internal_Delete (Self : Detached_Torrent);
   overriding procedure Internal_Delete (Self : Detached_User);

   overriding function Key (Self : Config_Ddr) return Element_Key;
   overriding function Key (Self : Torrent_Ddr) return Element_Key;
   overriding function Key (Self : User_Ddr) return Element_Key;

   overriding procedure On_Persist (Self : Detached_Torrent);

private

    type Config_DDR is new Detached_Data (2) with record
       ORM_Id         : Integer := -1;
       ORM_Version    : Integer := -1;
    end record;
    type Config_Data is access all Config_DDR;
    
    type Torrent_DDR is new Detached_Data (5) with record
       ORM_Created_By      : Integer := -1;
       ORM_FK_Created_By   : Detached_User_Access := null;
       ORM_Filename        : Unbounded_String := Null_Unbounded_String;
       ORM_Id              : Integer := -1;
       ORM_Torrent_File    : Unbounded_String := Null_Unbounded_String;
    end record;
    type Torrent_Data is access all Torrent_DDR;
    
    type User_DDR is new Detached_Data (3) with record
       ORM_Id          : Integer := -1;
       ORM_Password    : Unbounded_String := Null_Unbounded_String;
       ORM_Username    : Unbounded_String := Null_Unbounded_String;
    end record;
    type User_Data is access all User_DDR;
    

    type Detached_Config
       is new Sessions.Detached_Element with null record;
    No_Config : constant Config :=(No_Orm_Element with null record);
    No_Detached_Config : constant Detached_Config :=
      (Sessions.Detached_Element with null record);
 
    type Detached_Torrent
       is new Sessions.Detached_Element with null record;
    No_Torrent : constant Torrent :=(No_Orm_Element with null record);
    No_Detached_Torrent : constant Detached_Torrent :=
      (Sessions.Detached_Element with null record);
 
    type Detached_User
       is new Sessions.Detached_Element with null record;
    No_User : constant User :=(No_Orm_Element with null record);
    No_Detached_User : constant Detached_User :=
      (Sessions.Detached_Element with null record);
 
end Orm;
