
pragma Warnings (Off);
with Ada.Containers; use Ada.Containers;
with Ada.Unchecked_Deallocation;
pragma Warnings (On);
pragma Style_Checks (Off);

package body Orm is
   pragma Warnings (Off);
   use Sessions.Pointers;

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
      ( Config_DDR, Config_Data);
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
      ( Invite_DDR, Invite_Data);
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
      ( Torrent_DDR, Torrent_Data);
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
      ( User_DDR, User_Data);
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
      ( User_Torrent_Stat_DDR, User_Torrent_Stat_Data);
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Detached_Torrent'Class, Detached_Torrent_Access);
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Detached_User'Class, Detached_User_Access);

   F_Config_Id      : constant := 0;
   F_Config_Version : constant := 1;
   Alias_Config : constant Alias_Array := (0 => -1);
   F_Invites_Id        : constant := 0;
   F_Invites_Value     : constant := 1;
   F_Invites_Activated : constant := 2;
   F_Invites_By_User   : constant := 3;
   F_Invites_For_User  : constant := 4;
   Upto_Invites_0 : constant Counts := ((5,5),(5,5),(5,5),(5,5));
   Upto_Invites_1 : constant Counts := ((5,5),(11,11),(11,11),(11,11));
   Alias_Invites : constant Alias_Array := (-1,3,4,-1,0);
   F_Torrents_Id         : constant := 0;
   F_Torrents_Info_Hash  : constant := 1;
   F_Torrents_Created_By : constant := 2;
   Counts_Torrents : constant Counts := ((3,3),(9,9),(9,9),(9,9));
   Upto_Torrents_0 : constant Counts := ((3,3),(3,3),(3,3),(3,3));
   Alias_Torrents : constant Alias_Array := (-1,2,-1);
   F_User_Torrent_Stats_By_User    : constant := 0;
   F_User_Torrent_Stats_Of_Torrent : constant := 1;
   F_User_Torrent_Stats_Uploaded   : constant := 2;
   F_User_Torrent_Stats_Downloaded : constant := 3;
   Upto_User_Torrent_Stats_0 : constant Counts := ((4,4),(4,4),(4,4),(4,4));
   Upto_User_Torrent_Stats_1 : constant Counts := ((4,4),(10,10),(10,10),(10,10));
   Alias_User_Torrent_Stats : constant Alias_Array := (-1,3,4,-1,-1,6,0);
   F_Users_Id         : constant := 0;
   F_Users_Username   : constant := 1;
   F_Users_Password   : constant := 2;
   F_Users_Passkey    : constant := 3;
   F_Users_Uploaded   : constant := 4;
   F_Users_Downloaded : constant := 5;
   Counts_Users : constant Counts := ((6,6),(6,6),(6,6),(6,6));
   Alias_Users : constant Alias_Array := (0 => -1);

   pragma Warnings (On);
   function Detach_No_Lookup
     (Self    : Config'Class;
      Session : Session_Type)
     return Detached_Config'Class;
   function Detach_No_Lookup
     (Self    : Invite'Class;
      Session : Session_Type)
     return Detached_Invite'Class;
   function Detach_No_Lookup
     (Self    : Torrent'Class;
      Session : Session_Type)
     return Detached_Torrent'Class;
   function Detach_No_Lookup
     (Self    : User_Torrent_Stat'Class;
      Session : Session_Type)
     return Detached_User_Torrent_Stat'Class;
   function Detach_No_Lookup
     (Self    : User'Class;
      Session : Session_Type)
     return Detached_User'Class;
   --  Same as Detach, but does not check the session cache Same as Detach,
   --  but does not check the session cache Same as Detach, but does not check
   --  the session cache Same as Detach, but does not check the session cache
   --  Same as Detach, but does not check the session cache

   procedure Do_Query_Config
     (Fields    : in out SQL_Field_List;
      From      : out SQL_Table_List;
      Criteria  : in out Sql_Criteria;
      Base      : Natural;
      Aliases   : Alias_Array;
      Depth     : Natural;
      Follow_LJ : Boolean;
      Pk_Only   : Boolean := False);

   procedure Do_Query_Invites
     (Fields    : in out SQL_Field_List;
      From      : out SQL_Table_List;
      Criteria  : in out Sql_Criteria;
      Base      : Natural;
      Aliases   : Alias_Array;
      Depth     : Natural;
      Follow_LJ : Boolean;
      Pk_Only   : Boolean := False);

   procedure Do_Query_Torrents
     (Fields    : in out SQL_Field_List;
      From      : out SQL_Table_List;
      Criteria  : in out Sql_Criteria;
      Base      : Natural;
      Aliases   : Alias_Array;
      Depth     : Natural;
      Follow_LJ : Boolean;
      Pk_Only   : Boolean := False);

   procedure Do_Query_User_Torrent_Stats
     (Fields    : in out SQL_Field_List;
      From      : out SQL_Table_List;
      Criteria  : in out Sql_Criteria;
      Base      : Natural;
      Aliases   : Alias_Array;
      Depth     : Natural;
      Follow_LJ : Boolean);

   procedure Do_Query_Users
     (Fields    : in out SQL_Field_List;
      From      : out SQL_Table_List;
      Criteria  : in out Sql_Criteria;
      Base      : Natural;
      Aliases   : Alias_Array;
      Depth     : Natural;
      Follow_LJ : Boolean;
      Pk_Only   : Boolean := False);

   ---------
   -- "=" --
   ---------

   function "=" (Op1 : Torrent; Op2 : Torrent) return Boolean is
   begin
      return Integer'(Op1.Id) = Op2.Id;
   end "=";

   ---------
   -- "=" --
   ---------

   function "=" (Op1 : Detached_Torrent; Op2 : Detached_Torrent) return Boolean
   is
   begin
      if Op1.Is_Null then
         return Op2.Is_Null;
      elsif Op2.Is_Null then
         return False;
      else
         return Integer'(Op1.Id) = Op2.Id;
      end if;
   end "=";

   ---------
   -- "=" --
   ---------

   function "=" (Op1 : User; Op2 : User) return Boolean is
   begin
      return Integer'(Op1.Id) = Op2.Id;
   end "=";

   ---------
   -- "=" --
   ---------

   function "=" (Op1 : Detached_User; Op2 : Detached_User) return Boolean is
   begin
      if Op1.Is_Null then
         return Op2.Is_Null;
      elsif Op2.Is_Null then
         return False;
      else
         return Integer'(Op1.Id) = Op2.Id;
      end if;
   end "=";

   ---------
   -- "=" --
   ---------

   function "=" (Op1 : Config; Op2 : Config) return Boolean is
   begin
      return Integer'(Op1.Id) = Op2.Id;
   end "=";

   ---------
   -- "=" --
   ---------

   function "=" (Op1 : Detached_Config; Op2 : Detached_Config) return Boolean
   is
   begin
      if Op1.Is_Null then
         return Op2.Is_Null;
      elsif Op2.Is_Null then
         return False;
      else
         return Integer'(Op1.Id) = Op2.Id;
      end if;
   end "=";

   ---------
   -- "=" --
   ---------

   function "=" (Op1 : Invite; Op2 : Invite) return Boolean is
   begin
      return Integer'(Op1.Id) = Op2.Id;
   end "=";

   ---------
   -- "=" --
   ---------

   function "=" (Op1 : Detached_Invite; Op2 : Detached_Invite) return Boolean
   is
   begin
      if Op1.Is_Null then
         return Op2.Is_Null;
      elsif Op2.Is_Null then
         return False;
      else
         return Integer'(Op1.Id) = Op2.Id;
      end if;
   end "=";

   ---------------
   -- Activated --
   ---------------

   function Activated (Self : Invite) return Boolean is
   begin
      return Boolean_Value (Self, F_Invites_Activated);
   end Activated;

   ---------------
   -- Activated --
   ---------------

   function Activated (Self : Detached_Invite) return Boolean is
   begin
      return Invite_Data (Self.Unchecked_Get).ORM_Activated;
   end Activated;

   -------------
   -- By_User --
   -------------

   function By_User (Self : User_Torrent_Stat) return Integer is
   begin
      return Integer_Value (Self, F_User_Torrent_Stats_By_User);
   end By_User;

   -------------
   -- By_User --
   -------------

   function By_User (Self : Detached_User_Torrent_Stat) return Integer is
   begin
      return User_Torrent_Stat_Data (Self.Unchecked_Get).ORM_By_User;
   end By_User;

   -------------
   -- By_User --
   -------------

   function By_User (Self : User_Torrent_Stat) return User'Class is
   begin
      if Current (Self.Current) /= Self.Index then
         raise Cursor_Has_Moved;
      end if;

      if Self.Depth > 0 then
         return I_Users.Internal_Element
           (Self,
            Upto_User_Torrent_Stats_0 (Self.Depth, Self.Data.Follow_LJ));
      else
         if not Dynamic_Fetching then
            raise Field_Not_Available with
            "Dynamic fetching disabled for By_User";
         end if;

         return Filter (All_Users, Id => Self.By_User)
         .Limit (1).Get (Self.Data.Session).Element;
      end if;
   end By_User;

   -------------
   -- By_User --
   -------------

   function By_User
     (Self : Detached_User_Torrent_Stat)
     return Detached_User'Class
   is
      D : constant User_Torrent_Stat_Data := User_Torrent_Stat_Data (Self.Unchecked_Get);
      S : Session_Type;
   begin
      if D.ORM_FK_By_User = null then
         if not Dynamic_Fetching then
            raise Field_Not_Available with
            "Dynamic fetching disabled for By_User";
         end if;
         S := Session (Self);
         if S = No_Session then
            raise Field_Not_Available with
            "Element is detached from any session";
         end if;
         D.ORM_FK_By_User := new Detached_User'Class'
           (Get_User (S, Id => D.ORM_By_User));
      end if;
      return D.ORM_FK_By_User.all;
   end By_User;

   -------------
   -- By_User --
   -------------

   function By_User (Self : Invite) return Integer is
   begin
      return Integer_Value (Self, F_Invites_By_User);
   end By_User;

   -------------
   -- By_User --
   -------------

   function By_User (Self : Detached_Invite) return Integer is
   begin
      return Invite_Data (Self.Unchecked_Get).ORM_By_User;
   end By_User;

   -------------
   -- By_User --
   -------------

   function By_User (Self : Invite) return User'Class is
   begin
      if Current (Self.Current) /= Self.Index then
         raise Cursor_Has_Moved;
      end if;

      if Self.Depth > 0 then
         return I_Users.Internal_Element
           (Self,
            Upto_Invites_0 (Self.Depth, Self.Data.Follow_LJ));
      else
         if not Dynamic_Fetching then
            raise Field_Not_Available with
            "Dynamic fetching disabled for By_User";
         end if;

         return Filter (All_Users, Id => Self.By_User)
         .Limit (1).Get (Self.Data.Session).Element;
      end if;
   end By_User;

   -------------
   -- By_User --
   -------------

   function By_User (Self : Detached_Invite) return Detached_User'Class
   is
      D : constant Invite_Data := Invite_Data (Self.Unchecked_Get);
      S : Session_Type;
   begin
      if D.ORM_FK_By_User = null then
         if not Dynamic_Fetching then
            raise Field_Not_Available with
            "Dynamic fetching disabled for By_User";
         end if;
         S := Session (Self);
         if S = No_Session then
            raise Field_Not_Available with
            "Element is detached from any session";
         end if;
         D.ORM_FK_By_User := new Detached_User'Class'
           (Get_User (S, Id => D.ORM_By_User));
      end if;
      return D.ORM_FK_By_User.all;
   end By_User;

   ----------------
   -- Created_By --
   ----------------

   function Created_By (Self : Torrent) return Integer is
   begin
      return Integer_Value (Self, F_Torrents_Created_By);
   end Created_By;

   ----------------
   -- Created_By --
   ----------------

   function Created_By (Self : Detached_Torrent) return Integer is
   begin
      return Torrent_Data (Self.Unchecked_Get).ORM_Created_By;
   end Created_By;

   ----------------
   -- Created_By --
   ----------------

   function Created_By (Self : Torrent) return User'Class is
   begin
      if Current (Self.Current) /= Self.Index then
         raise Cursor_Has_Moved;
      end if;

      if Self.Depth > 0 then
         return I_Users.Internal_Element
           (Self,
            Upto_Torrents_0 (Self.Depth, Self.Data.Follow_LJ));
      else
         if not Dynamic_Fetching then
            raise Field_Not_Available with
            "Dynamic fetching disabled for Created_By";
         end if;

         return Filter (All_Users, Id => Self.Created_By)
         .Limit (1).Get (Self.Data.Session).Element;
      end if;
   end Created_By;

   ----------------
   -- Created_By --
   ----------------

   function Created_By (Self : Detached_Torrent) return Detached_User'Class
   is
      D : constant Torrent_Data := Torrent_Data (Self.Unchecked_Get);
      S : Session_Type;
   begin
      if D.ORM_FK_Created_By = null then
         if not Dynamic_Fetching then
            raise Field_Not_Available with
            "Dynamic fetching disabled for Created_By";
         end if;
         S := Session (Self);
         if S = No_Session then
            raise Field_Not_Available with
            "Element is detached from any session";
         end if;
         D.ORM_FK_Created_By := new Detached_User'Class'
           (Get_User (S, Id => D.ORM_Created_By));
      end if;
      return D.ORM_FK_Created_By.all;
   end Created_By;

   ----------------
   -- Downloaded --
   ----------------

   function Downloaded (Self : User) return Long_Long_Integer is
   begin
      return Bigint_Value (Self, F_Users_Downloaded);
   end Downloaded;

   ----------------
   -- Downloaded --
   ----------------

   function Downloaded (Self : Detached_User) return Long_Long_Integer is
   begin
      return User_Data (Self.Unchecked_Get).ORM_Downloaded;
   end Downloaded;

   ----------------
   -- Downloaded --
   ----------------

   function Downloaded (Self : User_Torrent_Stat) return Long_Long_Integer is
   begin
      return Bigint_Value (Self, F_User_Torrent_Stats_Downloaded);
   end Downloaded;

   ----------------
   -- Downloaded --
   ----------------

   function Downloaded
     (Self : Detached_User_Torrent_Stat)
     return Long_Long_Integer is
   begin
      return User_Torrent_Stat_Data (Self.Unchecked_Get).ORM_Downloaded;
   end Downloaded;

   --------------
   -- For_User --
   --------------

   function For_User (Self : Invite) return Integer is
   begin
      return Integer_Value (Self, F_Invites_For_User);
   end For_User;

   --------------
   -- For_User --
   --------------

   function For_User (Self : Detached_Invite) return Integer is
   begin
      return Invite_Data (Self.Unchecked_Get).ORM_For_User;
   end For_User;

   --------------
   -- For_User --
   --------------

   function For_User (Self : Invite) return User'Class is
   begin
      if Current (Self.Current) /= Self.Index then
         raise Cursor_Has_Moved;
      end if;

      if Self.Depth > 0 and then Self.Data.Follow_LJ then
         return I_Users.Internal_Element
           (Self,
            Upto_Invites_1 (Self.Depth, Self.Data.Follow_LJ));
      else
         if not Dynamic_Fetching then
            raise Field_Not_Available with
            "Dynamic fetching disabled for For_User";
         end if;

         return Filter (All_Users, Id => Self.For_User)
         .Limit (1).Get (Self.Data.Session).Element;
      end if;
   end For_User;

   --------------
   -- For_User --
   --------------

   function For_User (Self : Detached_Invite) return Detached_User'Class
   is
      D : constant Invite_Data := Invite_Data (Self.Unchecked_Get);
      S : Session_Type;
   begin
      if D.ORM_FK_For_User = null then
         if not Dynamic_Fetching then
            raise Field_Not_Available with
            "Dynamic fetching disabled for For_User";
         end if;
         S := Session (Self);
         if S = No_Session then
            raise Field_Not_Available with
            "Element is detached from any session";
         end if;
         D.ORM_FK_For_User := new Detached_User'Class'
           (Get_User (S, Id => D.ORM_For_User));
      end if;
      return D.ORM_FK_For_User.all;
   end For_User;

   --------
   -- Id --
   --------

   function Id (Self : Torrent) return Integer is
   begin
      return Integer_Value (Self, F_Torrents_Id);
   end Id;

   --------
   -- Id --
   --------

   function Id (Self : Detached_Torrent) return Integer is
   begin
      return Torrent_Data (Self.Unchecked_Get).ORM_Id;
   end Id;

   --------
   -- Id --
   --------

   function Id (Self : User) return Integer is
   begin
      return Integer_Value (Self, F_Users_Id);
   end Id;

   --------
   -- Id --
   --------

   function Id (Self : Detached_User) return Integer is
   begin
      return User_Data (Self.Unchecked_Get).ORM_Id;
   end Id;

   --------
   -- Id --
   --------

   function Id (Self : Config) return Integer is
   begin
      return Integer_Value (Self, F_Config_Id);
   end Id;

   --------
   -- Id --
   --------

   function Id (Self : Detached_Config) return Integer is
   begin
      return Config_Data (Self.Unchecked_Get).ORM_Id;
   end Id;

   --------
   -- Id --
   --------

   function Id (Self : Invite) return Integer is
   begin
      return Integer_Value (Self, F_Invites_Id);
   end Id;

   --------
   -- Id --
   --------

   function Id (Self : Detached_Invite) return Integer is
   begin
      return Invite_Data (Self.Unchecked_Get).ORM_Id;
   end Id;

   ---------------
   -- Info_Hash --
   ---------------

   function Info_Hash (Self : Torrent) return String is
   begin
      return String_Value (Self, F_Torrents_Info_Hash);
   end Info_Hash;

   ---------------
   -- Info_Hash --
   ---------------

   function Info_Hash (Self : Detached_Torrent) return String is
   begin
      return To_String (Torrent_Data (Self.Unchecked_Get).ORM_Info_Hash);
   end Info_Hash;

   ----------------
   -- Of_Torrent --
   ----------------

   function Of_Torrent (Self : User_Torrent_Stat) return Integer is
   begin
      return Integer_Value (Self, F_User_Torrent_Stats_Of_Torrent);
   end Of_Torrent;

   ----------------
   -- Of_Torrent --
   ----------------

   function Of_Torrent (Self : Detached_User_Torrent_Stat) return Integer is
   begin
      return User_Torrent_Stat_Data (Self.Unchecked_Get).ORM_Of_Torrent;
   end Of_Torrent;

   ----------------
   -- Of_Torrent --
   ----------------

   function Of_Torrent (Self : User_Torrent_Stat) return Torrent'Class is
   begin
      if Current (Self.Current) /= Self.Index then
         raise Cursor_Has_Moved;
      end if;

      if Self.Depth > 0 then
         return I_Torrents.Internal_Element
           (Self,
            Upto_User_Torrent_Stats_1 (Self.Depth, Self.Data.Follow_LJ));
      else
         if not Dynamic_Fetching then
            raise Field_Not_Available with
            "Dynamic fetching disabled for Of_Torrent";
         end if;

         return Filter (All_Torrents, Id => Self.Of_Torrent)
         .Limit (1).Get (Self.Data.Session).Element;
      end if;
   end Of_Torrent;

   ----------------
   -- Of_Torrent --
   ----------------

   function Of_Torrent
     (Self : Detached_User_Torrent_Stat)
     return Detached_Torrent'Class
   is
      D : constant User_Torrent_Stat_Data := User_Torrent_Stat_Data (Self.Unchecked_Get);
      S : Session_Type;
   begin
      if D.ORM_FK_Of_Torrent = null then
         if not Dynamic_Fetching then
            raise Field_Not_Available with
            "Dynamic fetching disabled for Of_Torrent";
         end if;
         S := Session (Self);
         if S = No_Session then
            raise Field_Not_Available with
            "Element is detached from any session";
         end if;
         D.ORM_FK_Of_Torrent := new Detached_Torrent'Class'
           (Get_Torrent (S, Id => D.ORM_Of_Torrent));
      end if;
      return D.ORM_FK_Of_Torrent.all;
   end Of_Torrent;

   -------------
   -- Passkey --
   -------------

   function Passkey (Self : User) return String is
   begin
      return String_Value (Self, F_Users_Passkey);
   end Passkey;

   -------------
   -- Passkey --
   -------------

   function Passkey (Self : Detached_User) return String is
   begin
      return To_String (User_Data (Self.Unchecked_Get).ORM_Passkey);
   end Passkey;

   --------------
   -- Password --
   --------------

   function Password (Self : User) return String is
   begin
      return String_Value (Self, F_Users_Password);
   end Password;

   --------------
   -- Password --
   --------------

   function Password (Self : Detached_User) return String is
   begin
      return To_String (User_Data (Self.Unchecked_Get).ORM_Password);
   end Password;

   --------------
   -- Uploaded --
   --------------

   function Uploaded (Self : User) return Long_Long_Integer is
   begin
      return Bigint_Value (Self, F_Users_Uploaded);
   end Uploaded;

   --------------
   -- Uploaded --
   --------------

   function Uploaded (Self : Detached_User) return Long_Long_Integer is
   begin
      return User_Data (Self.Unchecked_Get).ORM_Uploaded;
   end Uploaded;

   --------------
   -- Uploaded --
   --------------

   function Uploaded (Self : User_Torrent_Stat) return Long_Long_Integer is
   begin
      return Bigint_Value (Self, F_User_Torrent_Stats_Uploaded);
   end Uploaded;

   --------------
   -- Uploaded --
   --------------

   function Uploaded
     (Self : Detached_User_Torrent_Stat)
     return Long_Long_Integer is
   begin
      return User_Torrent_Stat_Data (Self.Unchecked_Get).ORM_Uploaded;
   end Uploaded;

   --------------------------------------
   -- User_Torrent_Stats_Of_Torrent_Id --
   --------------------------------------

   function User_Torrent_Stats_Of_Torrent_Id
     (Self : Torrent'Class)
     return User_Torrent_Stats_Managers is
   begin
      return Filter (All_User_Torrent_Stats, Of_Torrent => Self.Id);
   end User_Torrent_Stats_Of_Torrent_Id;

   --------------------------------------
   -- User_Torrent_Stats_Of_Torrent_Id --
   --------------------------------------

   function User_Torrent_Stats_Of_Torrent_Id
     (Self : Detached_Torrent'Class)
     return User_Torrent_Stats_Managers is
   begin
      return Filter (All_User_Torrent_Stats, Of_Torrent => Self.Id);
   end User_Torrent_Stats_Of_Torrent_Id;

   --------------------------------------
   -- User_Torrent_Stats_Of_Torrent_Id --
   --------------------------------------

   function User_Torrent_Stats_Of_Torrent_Id
     (Self : I_Torrents_Managers'Class)
     return User_Torrent_Stats_Managers
   is
      Q : constant SQL_Query := I_Torrents.Build_Query(Self, +DBA.Torrents.Id);
   begin
      return All_User_Torrent_Stats.Filter
        (SQL_In(DBA.User_Torrent_Stats.Of_Torrent, Q));
   end User_Torrent_Stats_Of_Torrent_Id;

   --------------
   -- Username --
   --------------

   function Username (Self : User) return String is
   begin
      return String_Value (Self, F_Users_Username);
   end Username;

   --------------
   -- Username --
   --------------

   function Username (Self : Detached_User) return String is
   begin
      return To_String (User_Data (Self.Unchecked_Get).ORM_Username);
   end Username;

   -----------
   -- Value --
   -----------

   function Value (Self : Invite) return String is
   begin
      return String_Value (Self, F_Invites_Value);
   end Value;

   -----------
   -- Value --
   -----------

   function Value (Self : Detached_Invite) return String is
   begin
      return To_String (Invite_Data (Self.Unchecked_Get).ORM_Value);
   end Value;

   -------------
   -- Version --
   -------------

   function Version (Self : Config) return Integer is
   begin
      return Integer_Value (Self, F_Config_Version);
   end Version;

   -------------
   -- Version --
   -------------

   function Version (Self : Detached_Config) return Integer is
   begin
      return Config_Data (Self.Unchecked_Get).ORM_Version;
   end Version;

   ----------------------
   -- Created_Torrents --
   ----------------------

   function Created_Torrents (Self : User'Class) return Torrents_Managers is
   begin
      return Filter (All_Torrents, Created_By => Self.Id);
   end Created_Torrents;

   ----------------------
   -- Created_Torrents --
   ----------------------

   function Created_Torrents
     (Self : Detached_User'Class)
     return Torrents_Managers is
   begin
      return Filter (All_Torrents, Created_By => Self.Id);
   end Created_Torrents;

   ----------------------
   -- Created_Torrents --
   ----------------------

   function Created_Torrents
     (Self : I_Users_Managers'Class)
     return Torrents_Managers
   is
      Q : constant SQL_Query := I_Users.Build_Query(Self, +DBA.Users.Id);
   begin
      return All_Torrents.Filter
        (SQL_In(DBA.Torrents.Created_By, Q));
   end Created_Torrents;

   ------------
   -- Detach --
   ------------

   function Detach (Self : Torrent'Class) return Detached_Torrent'Class
   is
      R : constant Detached_Torrent'Class := From_Cache (Self.Data.Session, Self.Id);
   begin
      if R.Is_Null then
         return Detach_No_Lookup (Self, Self.Data.Session);
      else
         return R;
      end if;
   end Detach;

   ------------
   -- Detach --
   ------------

   function Detach (Self : User'Class) return Detached_User'Class
   is
      R : constant Detached_User'Class := From_Cache (Self.Data.Session, Self.Id);
   begin
      if R.Is_Null then
         return Detach_No_Lookup (Self, Self.Data.Session);
      else
         return R;
      end if;
   end Detach;

   ------------
   -- Detach --
   ------------

   function Detach (Self : Config'Class) return Detached_Config'Class
   is
      R : constant Detached_Config'Class := From_Cache (Self.Data.Session, Self.Id);
   begin
      if R.Is_Null then
         return Detach_No_Lookup (Self, Self.Data.Session);
      else
         return R;
      end if;
   end Detach;

   ------------
   -- Detach --
   ------------

   function Detach
     (Self : User_Torrent_Stat'Class)
     return Detached_User_Torrent_Stat'Class is
   begin
      return Detach_No_Lookup (Self, Self.Data.Session);
   end Detach;

   ------------
   -- Detach --
   ------------

   function Detach (Self : Invite'Class) return Detached_Invite'Class
   is
      R : constant Detached_Invite'Class := From_Cache (Self.Data.Session, Self.Id);
   begin
      if R.Is_Null then
         return Detach_No_Lookup (Self, Self.Data.Session);
      else
         return R;
      end if;
   end Detach;

   ----------------------
   -- Detach_No_Lookup --
   ----------------------

   function Detach_No_Lookup
     (Self    : Config'Class;
      Session : Session_Type)
     return Detached_Config'Class
   is
      Default : Detached_Config;
      Result  : Detached_Config'Class := Detached_Config'Class (Session.Factory (Self, Default));
      Tmp     : Config_Data;
   begin
      if Result.Is_Null then
         Result.Set (Config_DDR'
              (Detached_Data with Field_Count => 2, others => <>));
      end if;

      Tmp := Config_Data (Result.Unchecked_Get);

      Tmp.ORM_Id         := Integer_Value (Self, F_Config_Id);
      Tmp.ORM_Version    := Integer_Value (Self, F_Config_Version);
      Session.Persist (Result);
      return Result;
   end Detach_No_Lookup;

   ----------------------
   -- Detach_No_Lookup --
   ----------------------

   function Detach_No_Lookup
     (Self    : Invite'Class;
      Session : Session_Type)
     return Detached_Invite'Class
   is
      Default     : Detached_Invite;
      Result      : Detached_Invite'Class := Detached_Invite'Class (Session.Factory (Self, Default));
      Fk_By_User  : Detached_User_Access;
      Fk_For_User : Detached_User_Access;
      Lj          : constant Boolean := Self.Data.Follow_LJ;
      Tmp         : Invite_Data;
   begin
      if Result.Is_Null then
         Result.Set (Invite_DDR'
              (Detached_Data with Field_Count => 7, others => <>));
      end if;

      Tmp := Invite_Data (Result.Unchecked_Get);
      if Self.Depth > 0 then
         FK_By_User := new Detached_User'Class'(
            I_Users.Internal_Element
              (Self, Upto_Invites_0 (Self.Depth, LJ)).Detach);
         if LJ then
            FK_For_User := new Detached_User'Class'(
               I_Users.Internal_Element
                 (Self, Upto_Invites_1 (Self.Depth, LJ)).Detach);
         end if;

      end if;

      Tmp.ORM_Activated    := Boolean_Value (Self, F_Invites_Activated);
      Tmp.ORM_By_User      := Integer_Value (Self, F_Invites_By_User);
      Tmp.ORM_FK_By_User   := FK_By_User;
      Tmp.ORM_FK_For_User  := FK_For_User;
      Tmp.ORM_For_User     := Integer_Value (Self, F_Invites_For_User);
      Tmp.ORM_Id           := Integer_Value (Self, F_Invites_Id);
      Tmp.ORM_Value        := To_Unbounded_String (String_Value (Self, F_Invites_Value));
      Session.Persist (Result);
      return Result;
   end Detach_No_Lookup;

   ----------------------
   -- Detach_No_Lookup --
   ----------------------

   function Detach_No_Lookup
     (Self    : Torrent'Class;
      Session : Session_Type)
     return Detached_Torrent'Class
   is
      Default       : Detached_Torrent;
      Result        : Detached_Torrent'Class := Detached_Torrent'Class (Session.Factory (Self, Default));
      Fk_Created_By : Detached_User_Access;
      Lj            : constant Boolean := Self.Data.Follow_LJ;
      Tmp           : Torrent_Data;
   begin
      if Result.Is_Null then
         Result.Set (Torrent_DDR'
              (Detached_Data with Field_Count => 4, others => <>));
      end if;

      Tmp := Torrent_Data (Result.Unchecked_Get);
      if Self.Depth > 0 then
         FK_Created_By := new Detached_User'Class'(
            I_Users.Internal_Element
              (Self, Upto_Torrents_0 (Self.Depth, LJ)).Detach);
      end if;

      Tmp.ORM_Created_By    := Integer_Value (Self, F_Torrents_Created_By);
      Tmp.ORM_FK_Created_By := FK_Created_By;
      Tmp.ORM_Id            := Integer_Value (Self, F_Torrents_Id);
      Tmp.ORM_Info_Hash     := To_Unbounded_String (String_Value (Self, F_Torrents_Info_Hash));
      Session.Persist (Result);
      return Result;
   end Detach_No_Lookup;

   ----------------------
   -- Detach_No_Lookup --
   ----------------------

   function Detach_No_Lookup
     (Self    : User_Torrent_Stat'Class;
      Session : Session_Type)
     return Detached_User_Torrent_Stat'Class
   is
      Default       : Detached_User_Torrent_Stat;
      Result        : Detached_User_Torrent_Stat'Class := Detached_User_Torrent_Stat'Class (Session.Factory (Self, Default));
      Fk_By_User    : Detached_User_Access;
      Fk_Of_Torrent : Detached_Torrent_Access;
      Lj            : constant Boolean := Self.Data.Follow_LJ;
      Tmp           : User_Torrent_Stat_Data;
   begin
      if Result.Is_Null then
         Result.Set (User_Torrent_Stat_DDR'
              (Detached_Data with Field_Count => 6, others => <>));
      end if;

      Tmp := User_Torrent_Stat_Data (Result.Unchecked_Get);
      if Self.Depth > 0 then
         FK_By_User := new Detached_User'Class'(
            I_Users.Internal_Element
              (Self, Upto_User_Torrent_Stats_0 (Self.Depth, LJ)).Detach);
         FK_Of_Torrent := new Detached_Torrent'Class'(
            I_Torrents.Internal_Element
              (Self, Upto_User_Torrent_Stats_1 (Self.Depth, LJ)).Detach);
      end if;

      Tmp.ORM_By_User       := Integer_Value (Self, F_User_Torrent_Stats_By_User);
      Tmp.ORM_Downloaded    := Bigint_Value (Self, F_User_Torrent_Stats_Downloaded);
      Tmp.ORM_FK_By_User    := FK_By_User;
      Tmp.ORM_FK_Of_Torrent := FK_Of_Torrent;
      Tmp.ORM_Of_Torrent    := Integer_Value (Self, F_User_Torrent_Stats_Of_Torrent);
      Tmp.ORM_Uploaded      := Bigint_Value (Self, F_User_Torrent_Stats_Uploaded);
      Session.Persist (Result);
      return Result;
   end Detach_No_Lookup;

   ----------------------
   -- Detach_No_Lookup --
   ----------------------

   function Detach_No_Lookup
     (Self    : User'Class;
      Session : Session_Type)
     return Detached_User'Class
   is
      Default : Detached_User;
      Result  : Detached_User'Class := Detached_User'Class (Session.Factory (Self, Default));
      Tmp     : User_Data;
   begin
      if Result.Is_Null then
         Result.Set (User_DDR'
              (Detached_Data with Field_Count => 6, others => <>));
      end if;

      Tmp := User_Data (Result.Unchecked_Get);

      Tmp.ORM_Downloaded    := Bigint_Value (Self, F_Users_Downloaded);
      Tmp.ORM_Id            := Integer_Value (Self, F_Users_Id);
      Tmp.ORM_Passkey       := To_Unbounded_String (String_Value (Self, F_Users_Passkey));
      Tmp.ORM_Password      := To_Unbounded_String (String_Value (Self, F_Users_Password));
      Tmp.ORM_Uploaded      := Bigint_Value (Self, F_Users_Uploaded);
      Tmp.ORM_Username      := To_Unbounded_String (String_Value (Self, F_Users_Username));
      Session.Persist (Result);
      return Result;
   end Detach_No_Lookup;

   ---------------------
   -- Do_Query_Config --
   ---------------------

   procedure Do_Query_Config
     (Fields    : in out SQL_Field_List;
      From      : out SQL_Table_List;
      Criteria  : in out Sql_Criteria;
      Base      : Natural;
      Aliases   : Alias_Array;
      Depth     : Natural;
      Follow_LJ : Boolean;
      Pk_Only   : Boolean := False)
   is
      pragma Unreferenced (Criteria, Depth, Follow_LJ);
      Table : T_Numbered_Config(Aliases(Base));
   begin
      if PK_Only then
         Fields := Fields & Table.Id;
      else
         Fields := Fields & Table.Id
         & Table.Version;
      end if;
      From := Empty_Table_List;
   end Do_Query_Config;

   ----------------------
   -- Do_Query_Invites --
   ----------------------

   procedure Do_Query_Invites
     (Fields    : in out SQL_Field_List;
      From      : out SQL_Table_List;
      Criteria  : in out Sql_Criteria;
      Base      : Natural;
      Aliases   : Alias_Array;
      Depth     : Natural;
      Follow_LJ : Boolean;
      Pk_Only   : Boolean := False)
   is
      Table : T_Numbered_Invites(Aliases(Base));
      C2    : Sql_Criteria;
      T     : SQL_Table_List;
   begin
      if PK_Only then
         Fields := Fields & Table.Id;
      else
         Fields := Fields & Table.Id
         & Table.Value
         & Table.Activated
         & Table.By_User
         & Table.For_User;
      end if;
      From := Empty_Table_List;
      if Depth > 0 then

         declare
            FK1 : T_Numbered_Users(Aliases(Aliases(Base + 1)));
            FK2 : T_Numbered_Users(Aliases(Aliases(Base + 2)));
         begin Criteria := Criteria
         and Table.By_User = FK1.Id;
         if Follow_LJ then
            From := +Left_Join(Table, FK2, Table.For_User=FK2.Id);
         else
            From := +Table;
         end if;
         C2 := No_Criteria;
         Do_Query_Users(Fields, T, C2,Aliases(Base + 1),
            Aliases, Depth - 1, Follow_LJ);
         if Depth > 1 then
            Criteria := Criteria and C2;
         end if;
         From := From & T;

         if Follow_LJ then
            C2 := No_Criteria;
            Do_Query_Users(Fields, T, C2,Aliases(Base + 2),
               Aliases, Depth - 1, Follow_LJ);
            if Depth > 1 then
               Criteria := Criteria and C2;
            end if;
         end if;
      end;
   end if;
   end Do_Query_Invites;

   -----------------------
   -- Do_Query_Torrents --
   -----------------------

   procedure Do_Query_Torrents
     (Fields    : in out SQL_Field_List;
      From      : out SQL_Table_List;
      Criteria  : in out Sql_Criteria;
      Base      : Natural;
      Aliases   : Alias_Array;
      Depth     : Natural;
      Follow_LJ : Boolean;
      Pk_Only   : Boolean := False)
   is
      Table : T_Numbered_Torrents(Aliases(Base));
      C2    : Sql_Criteria;
      T     : SQL_Table_List;
   begin
      if PK_Only then
         Fields := Fields & Table.Id;
      else
         Fields := Fields & Table.Id
         & Table.Info_Hash
         & Table.Created_By;
      end if;
      From := Empty_Table_List;
      if Depth > 0 then

         declare
            FK1 : T_Numbered_Users(Aliases(Aliases(Base + 1)));
         begin Criteria := Criteria
         and Table.Created_By = FK1.Id;
         From := +Table;
         C2 := No_Criteria;
         Do_Query_Users(Fields, T, C2,Aliases(Base + 1),
            Aliases, Depth - 1, Follow_LJ);
         if Depth > 1 then
            Criteria := Criteria and C2;
         end if;
         From := From & T;
      end;
   end if;
   end Do_Query_Torrents;

   ---------------------------------
   -- Do_Query_User_Torrent_Stats --
   ---------------------------------

   procedure Do_Query_User_Torrent_Stats
     (Fields    : in out SQL_Field_List;
      From      : out SQL_Table_List;
      Criteria  : in out Sql_Criteria;
      Base      : Natural;
      Aliases   : Alias_Array;
      Depth     : Natural;
      Follow_LJ : Boolean)
   is
      Table : T_Numbered_User_Torrent_Stats(Aliases(Base));
      C2    : Sql_Criteria;
      T     : SQL_Table_List;
   begin
      Fields := Fields & Table.By_User
      & Table.Of_Torrent
      & Table.Uploaded
      & Table.Downloaded;
      if Depth > 0 then

         declare
            FK1 : T_Numbered_Users(Aliases(Aliases(Base + 1)));
            FK2 : T_Numbered_Torrents(Aliases(Aliases(Base + 2)));
         begin Criteria := Criteria
         and Table.By_User = FK1.Id
         and Table.Of_Torrent = FK2.Id;
         From := +Table;
         C2 := No_Criteria;
         Do_Query_Users(Fields, T, C2,Aliases(Base + 1),
            Aliases, Depth - 1, Follow_LJ);
         if Depth > 1 then
            Criteria := Criteria and C2;
         end if;
         From := From & T;

         C2 := No_Criteria;
         Do_Query_Torrents(Fields, T, C2,Aliases(Base + 2),
            Aliases, Depth - 1, Follow_LJ);
         if Depth > 1 then
            Criteria := Criteria and C2;
         end if;
         From := From & T;
      end;
   end if;
   end Do_Query_User_Torrent_Stats;

   --------------------
   -- Do_Query_Users --
   --------------------

   procedure Do_Query_Users
     (Fields    : in out SQL_Field_List;
      From      : out SQL_Table_List;
      Criteria  : in out Sql_Criteria;
      Base      : Natural;
      Aliases   : Alias_Array;
      Depth     : Natural;
      Follow_LJ : Boolean;
      Pk_Only   : Boolean := False)
   is
      pragma Unreferenced (Criteria, Depth, Follow_LJ);
      Table : T_Numbered_Users(Aliases(Base));
   begin
      if PK_Only then
         Fields := Fields & Table.Id;
      else
         Fields := Fields & Table.Id
         & Table.Username
         & Table.Password
         & Table.Passkey
         & Table.Uploaded
         & Table.Downloaded;
      end if;
      From := Empty_Table_List;
   end Do_Query_Users;

   ------------
   -- Filter --
   ------------

   function Filter
     (Self       : Torrents_Managers'Class;
      Id         : Integer := -1;
      Info_Hash  : String := No_Update;
      Created_By : Integer := -1)
     return Torrents_Managers
   is
      C      : Sql_Criteria := No_Criteria;
      Result : Torrents_Managers;
   begin
      if Id /= -1 then
         C := C and DBA.Torrents.Id = Id;
      end if;
      if Info_Hash /= No_Update then
         C := C and DBA.Torrents.Info_Hash = Info_Hash;
      end if;
      if Created_By /= -1 then
         C := C and DBA.Torrents.Created_By = Created_By;
      end if;
      Copy(Self.Filter(C), Into => Result);
      return Result;
   end Filter;

   ------------
   -- Filter --
   ------------

   function Filter
     (Self       : Users_Managers'Class;
      Id         : Integer := -1;
      Username   : String := No_Update;
      Password   : String := No_Update;
      Passkey    : String := No_Update;
      Uploaded   : Long_Long_Integer := -1;
      Downloaded : Long_Long_Integer := -1)
     return Users_Managers
   is
      C      : Sql_Criteria := No_Criteria;
      Result : Users_Managers;
   begin
      if Id /= -1 then
         C := C and DBA.Users.Id = Id;
      end if;
      if Username /= No_Update then
         C := C and DBA.Users.Username = Username;
      end if;
      if Password /= No_Update then
         C := C and DBA.Users.Password = Password;
      end if;
      if Passkey /= No_Update then
         C := C and DBA.Users.Passkey = Passkey;
      end if;
      if Uploaded /= -1 then
         C := C and DBA.Users.Uploaded = Uploaded;
      end if;
      if Downloaded /= -1 then
         C := C and DBA.Users.Downloaded = Downloaded;
      end if;
      Copy(Self.Filter(C), Into => Result);
      return Result;
   end Filter;

   ------------
   -- Filter --
   ------------

   function Filter
     (Self    : Config_Managers'Class;
      Id      : Integer := -1;
      Version : Integer := -1)
     return Config_Managers
   is
      C      : Sql_Criteria := No_Criteria;
      Result : Config_Managers;
   begin
      if Id /= -1 then
         C := C and DBA.Config.Id = Id;
      end if;
      if Version /= -1 then
         C := C and DBA.Config.Version = Version;
      end if;
      Copy(Self.Filter(C), Into => Result);
      return Result;
   end Filter;

   ------------
   -- Filter --
   ------------

   function Filter
     (Self       : User_Torrent_Stats_Managers'Class;
      By_User    : Integer := -1;
      Of_Torrent : Integer := -1;
      Uploaded   : Long_Long_Integer := -1;
      Downloaded : Long_Long_Integer := -1)
     return User_Torrent_Stats_Managers
   is
      C      : Sql_Criteria := No_Criteria;
      Result : User_Torrent_Stats_Managers;
   begin
      if By_User /= -1 then
         C := C and DBA.User_Torrent_Stats.By_User = By_User;
      end if;
      if Of_Torrent /= -1 then
         C := C and DBA.User_Torrent_Stats.Of_Torrent = Of_Torrent;
      end if;
      if Uploaded /= -1 then
         C := C and DBA.User_Torrent_Stats.Uploaded = Uploaded;
      end if;
      if Downloaded /= -1 then
         C := C and DBA.User_Torrent_Stats.Downloaded = Downloaded;
      end if;
      Copy(Self.Filter(C), Into => Result);
      return Result;
   end Filter;

   ------------
   -- Filter --
   ------------

   function Filter
     (Self      : Invites_Managers'Class;
      Id        : Integer := -1;
      Value     : String := No_Update;
      Activated : Triboolean := Indeterminate;
      By_User   : Integer := -1;
      For_User  : Integer := -1)
     return Invites_Managers
   is
      C      : Sql_Criteria := No_Criteria;
      Result : Invites_Managers;
   begin
      if Id /= -1 then
         C := C and DBA.Invites.Id = Id;
      end if;
      if Value /= No_Update then
         C := C and DBA.Invites.Value = Value;
      end if;
      if Activated /= Indeterminate then
         C := C and DBA.Invites.Activated = To_Boolean(Activated);
      end if;
      if By_User /= -1 then
         C := C and DBA.Invites.By_User = By_User;
      end if;
      if For_User /= -1 then
         C := C and DBA.Invites.For_User = For_User;
      end if;
      Copy(Self.Filter(C), Into => Result);
      return Result;
   end Filter;

   ----------
   -- Free --
   ----------

   overriding procedure Free (Self : in out Config_Ddr) is
   begin
      Free (Detached_Data (Self));
   end Free;

   ----------
   -- Free --
   ----------

   overriding procedure Free (Self : in out Invite_Ddr) is
   begin
      Unchecked_Free (Self.ORM_FK_By_User);
      Unchecked_Free (Self.ORM_FK_For_User);

      Free (Detached_Data (Self));
   end Free;

   ----------
   -- Free --
   ----------

   overriding procedure Free (Self : in out Torrent_Ddr) is
   begin
      Unchecked_Free (Self.ORM_FK_Created_By);

      Free (Detached_Data (Self));
   end Free;

   ----------
   -- Free --
   ----------

   overriding procedure Free (Self : in out User_Torrent_Stat_Ddr) is
   begin
      Unchecked_Free (Self.ORM_FK_By_User);
      Unchecked_Free (Self.ORM_FK_Of_Torrent);

      Free (Detached_Data (Self));
   end Free;

   ----------
   -- Free --
   ----------

   overriding procedure Free (Self : in out User_Ddr) is
   begin
      Free (Detached_Data (Self));
   end Free;

   ----------------
   -- From_Cache --
   ----------------

   function From_Cache
     (Session : Session_Type;
      Id      : Integer)
     return Detached_Torrent'Class is
   begin
      return Detached_Torrent'Class (Session.From_Cache ((0, Id), No_Detached_Torrent));
   end From_Cache;

   ----------------
   -- From_Cache --
   ----------------

   function From_Cache
     (Session : Session_Type;
      Id      : Integer)
     return Detached_User'Class is
   begin
      return Detached_User'Class (Session.From_Cache ((1000000, Id), No_Detached_User));
   end From_Cache;

   ----------------
   -- From_Cache --
   ----------------

   function From_Cache
     (Session : Session_Type;
      Id      : Integer)
     return Detached_Config'Class is
   begin
      return Detached_Config'Class (Session.From_Cache ((2000000, Id), No_Detached_Config));
   end From_Cache;

   ----------------
   -- From_Cache --
   ----------------

   function From_Cache
     (Session : Session_Type;
      Id      : Integer)
     return Detached_Invite'Class is
   begin
      return Detached_Invite'Class (Session.From_Cache ((4000000, Id), No_Detached_Invite));
   end From_Cache;

   ----------------
   -- Get_Config --
   ----------------

   function Get_Config
     (Session          : Session_Type;
      Id               : Integer;
      Depth            : Related_Depth := 0;
      Follow_Left_Join : Boolean := False)
     return Detached_Config'Class
   is
      R : constant Detached_Config'Class := From_Cache (Session, Id);
   begin
      if not R.Is_Null then
         return R;
      else

         declare
            M : Config_Managers := Filter
              (All_Config,
               Id => Id);
            L : I_Config.List;
         begin
            M.Select_Related
              (Depth, Follow_Left_Join => Follow_Left_Join);
            M.Limit (1);
            L := M.Get(Session);
            if not L.Has_Row then
               return No_Detached_Config;
            else

               declare
                  E : constant Config := L.Element;
               begin
                  --  Workaround bug in gnat which is missing a call
                  --  to Finalize if we do not reset the list (K321-012)
                  L := I_Config.Empty_List;
                  return E.Detach_No_Lookup (Session);
               end;
            end if;
         end;
      end if;
   end Get_Config;

   ----------------
   -- Get_Invite --
   ----------------

   function Get_Invite
     (Session          : Session_Type;
      Id               : Integer;
      Depth            : Related_Depth := 0;
      Follow_Left_Join : Boolean := False)
     return Detached_Invite'Class
   is
      R : constant Detached_Invite'Class := From_Cache (Session, Id);
   begin
      if not R.Is_Null then
         return R;
      else

         declare
            M : Invites_Managers := Filter
              (All_Invites,
               Id => Id);
            L : I_Invites.List;
         begin
            M.Select_Related
              (Depth, Follow_Left_Join => Follow_Left_Join);
            M.Limit (1);
            L := M.Get(Session);
            if not L.Has_Row then
               return No_Detached_Invite;
            else

               declare
                  E : constant Invite := L.Element;
               begin
                  --  Workaround bug in gnat which is missing a call
                  --  to Finalize if we do not reset the list (K321-012)
                  L := I_Invites.Empty_List;
                  return E.Detach_No_Lookup (Session);
               end;
            end if;
         end;
      end if;
   end Get_Invite;

   -----------------
   -- Get_Torrent --
   -----------------

   function Get_Torrent
     (Session          : Session_Type;
      Id               : Integer;
      Depth            : Related_Depth := 0;
      Follow_Left_Join : Boolean := False)
     return Detached_Torrent'Class
   is
      R : constant Detached_Torrent'Class := From_Cache (Session, Id);
   begin
      if not R.Is_Null then
         return R;
      else

         declare
            M : Torrents_Managers := Filter
              (All_Torrents,
               Id => Id);
            L : I_Torrents.List;
         begin
            M.Select_Related
              (Depth, Follow_Left_Join => Follow_Left_Join);
            M.Limit (1);
            L := M.Get(Session);
            if not L.Has_Row then
               return No_Detached_Torrent;
            else

               declare
                  E : constant Torrent := L.Element;
               begin
                  --  Workaround bug in gnat which is missing a call
                  --  to Finalize if we do not reset the list (K321-012)
                  L := I_Torrents.Empty_List;
                  return E.Detach_No_Lookup (Session);
               end;
            end if;
         end;
      end if;
   end Get_Torrent;

   --------------
   -- Get_User --
   --------------

   function Get_User
     (Session          : Session_Type;
      Id               : Integer;
      Depth            : Related_Depth := 0;
      Follow_Left_Join : Boolean := False)
     return Detached_User'Class
   is
      R : constant Detached_User'Class := From_Cache (Session, Id);
   begin
      if not R.Is_Null then
         return R;
      else

         declare
            M : Users_Managers := Filter
              (All_Users,
               Id => Id);
            L : I_Users.List;
         begin
            M.Select_Related
              (Depth, Follow_Left_Join => Follow_Left_Join);
            M.Limit (1);
            L := M.Get(Session);
            if not L.Has_Row then
               return No_Detached_User;
            else

               declare
                  E : constant User := L.Element;
               begin
                  --  Workaround bug in gnat which is missing a call
                  --  to Finalize if we do not reset the list (K321-012)
                  L := I_Users.Empty_List;
                  return E.Detach_No_Lookup (Session);
               end;
            end if;
         end;
      end if;
   end Get_User;

   ----------------------
   -- Insert_Or_Update --
   ----------------------

   overriding procedure Insert_Or_Update
     (Self        : in out Detached_Config;
      Pk_Modified : in out Boolean;
      Mask        : Dirty_Mask)
   is
      D          : constant Config_Data := Config_Data (Self.Unchecked_Get);
      Q          : SQL_Query;
      A          : Sql_Assignment := No_Assignment;
      Missing_Pk : constant Boolean := D.ORM_Id = -1;
      R          : Forward_Cursor;
   begin
      if Mask (2) then
         A := A & (DBA.Config.Version = D.ORM_Version);
      end if;
      if Missing_PK then
         Q := SQL_Insert (A);
      else
         Q := SQL_Update (DBA.Config, A, DBA.Config.Id = D.ORM_Id);
      end if;
      R.Fetch (Self.Session.DB, Q);

      if Missing_PK and then Success (Self.Session.DB) then
         PK_Modified := True;
         D.ORM_Id := R.Last_Id (Self.Session.DB, DBA.Config.Id);
      end if;
   end Insert_Or_Update;

   ----------------------
   -- Insert_Or_Update --
   ----------------------

   overriding procedure Insert_Or_Update
     (Self        : in out Detached_Invite;
      Pk_Modified : in out Boolean;
      Mask        : Dirty_Mask)
   is
      D          : constant Invite_Data := Invite_Data (Self.Unchecked_Get);
      Q          : SQL_Query;
      A          : Sql_Assignment := No_Assignment;
      Missing_Pk : constant Boolean := D.ORM_Id = -1;
      R          : Forward_Cursor;
   begin
      if Mask (2) then
         A := A & (DBA.Invites.Value = To_String (D.ORM_Value));
      end if;
      if Mask (3) then
         A := A & (DBA.Invites.Activated = D.ORM_Activated);
      end if;
      if Mask (4) then
         if D.ORM_By_User /= -1 then
            A := A & (DBA.Invites.By_User = D.ORM_By_User);
         else

            declare
               D2 : constant User_Data :=
               User_data (D.ORM_FK_By_User.Unchecked_Get);
            begin
               if D2.ORM_Id = -1 then
                  Self.Session.Insert_Or_Update
                    (D.ORM_FK_By_User.all);
               end if;

               A := A & (DBA.Invites.By_User = D2.ORM_Id);
            end;
         end if;
      end if;
      if Mask (5) then
         if D.ORM_For_User /= -1 then
            A := A & (DBA.Invites.For_User = D.ORM_For_User);
         else

            declare
               D2 : constant User_Data :=
               User_data (D.ORM_FK_For_User.Unchecked_Get);
            begin
               if D2.ORM_Id = -1 then
                  Self.Session.Insert_Or_Update
                    (D.ORM_FK_For_User.all);
               end if;

               A := A & (DBA.Invites.For_User = D2.ORM_Id);
            end;
         end if;
      end if;
      if Missing_PK then
         Q := SQL_Insert (A);
      else
         Q := SQL_Update (DBA.Invites, A, DBA.Invites.Id = D.ORM_Id);
      end if;
      R.Fetch (Self.Session.DB, Q);

      if Missing_PK and then Success (Self.Session.DB) then
         PK_Modified := True;
         D.ORM_Id := R.Last_Id (Self.Session.DB, DBA.Invites.Id);
      end if;
   end Insert_Or_Update;

   ----------------------
   -- Insert_Or_Update --
   ----------------------

   overriding procedure Insert_Or_Update
     (Self        : in out Detached_Torrent;
      Pk_Modified : in out Boolean;
      Mask        : Dirty_Mask)
   is
      D          : constant Torrent_Data := Torrent_Data (Self.Unchecked_Get);
      Q          : SQL_Query;
      A          : Sql_Assignment := No_Assignment;
      Missing_Pk : constant Boolean := D.ORM_Id = -1;
      R          : Forward_Cursor;
   begin
      if Mask (2) then
         A := A & (DBA.Torrents.Info_Hash = To_String (D.ORM_Info_Hash));
      end if;
      if Mask (3) then
         if D.ORM_Created_By /= -1 then
            A := A & (DBA.Torrents.Created_By = D.ORM_Created_By);
         else

            declare
               D2 : constant User_Data :=
               User_data (D.ORM_FK_Created_By.Unchecked_Get);
            begin
               if D2.ORM_Id = -1 then
                  Self.Session.Insert_Or_Update
                    (D.ORM_FK_Created_By.all);
               end if;

               A := A & (DBA.Torrents.Created_By = D2.ORM_Id);
            end;
         end if;
      end if;
      if Missing_PK then
         Q := SQL_Insert (A);
      else
         Q := SQL_Update (DBA.Torrents, A, DBA.Torrents.Id = D.ORM_Id);
      end if;
      R.Fetch (Self.Session.DB, Q);

      if Missing_PK and then Success (Self.Session.DB) then
         PK_Modified := True;
         D.ORM_Id := R.Last_Id (Self.Session.DB, DBA.Torrents.Id);
      end if;
   end Insert_Or_Update;

   ----------------------
   -- Insert_Or_Update --
   ----------------------

   overriding procedure Insert_Or_Update
     (Self        : in out Detached_User_Torrent_Stat;
      Pk_Modified : in out Boolean;
      Mask        : Dirty_Mask)
   is
      pragma Unreferenced (Pk_Modified);
      D : constant User_Torrent_Stat_Data := User_Torrent_Stat_Data (Self.Unchecked_Get);
      Q : SQL_Query;
      A : Sql_Assignment := No_Assignment;
      R : Forward_Cursor;
   begin
      if Mask (1) then
         if D.ORM_By_User /= -1 then
            A := A & (DBA.User_Torrent_Stats.By_User = D.ORM_By_User);
         else

            declare
               D2 : constant User_Data :=
               User_data (D.ORM_FK_By_User.Unchecked_Get);
            begin
               if D2.ORM_Id = -1 then
                  Self.Session.Insert_Or_Update
                    (D.ORM_FK_By_User.all);
               end if;

               A := A & (DBA.User_Torrent_Stats.By_User = D2.ORM_Id);
            end;
         end if;
      end if;
      if Mask (2) then
         if D.ORM_Of_Torrent /= -1 then
            A := A & (DBA.User_Torrent_Stats.Of_Torrent = D.ORM_Of_Torrent);
         else

            declare
               D2 : constant Torrent_Data :=
               Torrent_data (D.ORM_FK_Of_Torrent.Unchecked_Get);
            begin
               if D2.ORM_Id = -1 then
                  Self.Session.Insert_Or_Update
                    (D.ORM_FK_Of_Torrent.all);
               end if;

               A := A & (DBA.User_Torrent_Stats.Of_Torrent = D2.ORM_Id);
            end;
         end if;
      end if;
      if Mask (3) then
         A := A & (DBA.User_Torrent_Stats.Uploaded = D.ORM_Uploaded);
      end if;
      if Mask (4) then
         A := A & (DBA.User_Torrent_Stats.Downloaded = D.ORM_Downloaded);
      end if;
      Q := SQL_Insert (A);
      R.Fetch (Self.Session.DB, Q);
   end Insert_Or_Update;

   ----------------------
   -- Insert_Or_Update --
   ----------------------

   overriding procedure Insert_Or_Update
     (Self        : in out Detached_User;
      Pk_Modified : in out Boolean;
      Mask        : Dirty_Mask)
   is
      D          : constant User_Data := User_Data (Self.Unchecked_Get);
      Q          : SQL_Query;
      A          : Sql_Assignment := No_Assignment;
      Missing_Pk : constant Boolean := D.ORM_Id = -1;
      R          : Forward_Cursor;
   begin
      if Mask (2) then
         A := A & (DBA.Users.Username = To_String (D.ORM_Username));
      end if;
      if Mask (3) then
         A := A & (DBA.Users.Password = To_String (D.ORM_Password));
      end if;
      if Mask (4) then
         A := A & (DBA.Users.Passkey = To_String (D.ORM_Passkey));
      end if;
      if Mask (5) then
         A := A & (DBA.Users.Uploaded = D.ORM_Uploaded);
      end if;
      if Mask (6) then
         A := A & (DBA.Users.Downloaded = D.ORM_Downloaded);
      end if;
      if Missing_PK then
         Q := SQL_Insert (A);
      else
         Q := SQL_Update (DBA.Users, A, DBA.Users.Id = D.ORM_Id);
      end if;
      R.Fetch (Self.Session.DB, Q);

      if Missing_PK and then Success (Self.Session.DB) then
         PK_Modified := True;
         D.ORM_Id := R.Last_Id (Self.Session.DB, DBA.Users.Id);
      end if;
   end Insert_Or_Update;

   ---------------------
   -- Internal_Delete --
   ---------------------

   overriding procedure Internal_Delete (Self : Detached_Config)
   is
      D : constant Config_Data := Config_Data (Self.Unchecked_Get);
   begin
      Execute (Self.Session.DB, SQL_Delete (DBA.Config, DBA.Config.Id = D.ORM_Id));
   end Internal_Delete;

   ---------------------
   -- Internal_Delete --
   ---------------------

   overriding procedure Internal_Delete (Self : Detached_Invite)
   is
      D : constant Invite_Data := Invite_Data (Self.Unchecked_Get);
   begin
      Execute (Self.Session.DB, SQL_Delete (DBA.Invites, DBA.Invites.Id = D.ORM_Id));
   end Internal_Delete;

   ---------------------
   -- Internal_Delete --
   ---------------------

   overriding procedure Internal_Delete (Self : Detached_Torrent)
   is
      D : constant Torrent_Data := Torrent_Data (Self.Unchecked_Get);
   begin
      Execute (Self.Session.DB, SQL_Delete (DBA.Torrents, DBA.Torrents.Id = D.ORM_Id));
   end Internal_Delete;

   ---------------------
   -- Internal_Delete --
   ---------------------

   overriding procedure Internal_Delete (Self : Detached_User_Torrent_Stat) is
      pragma Unreferenced (Self);
   begin
      raise Program_Error with "Table User_Torrent_Stats has no primary key";
   end Internal_Delete;

   ---------------------
   -- Internal_Delete --
   ---------------------

   overriding procedure Internal_Delete (Self : Detached_User)
   is
      D : constant User_Data := User_Data (Self.Unchecked_Get);
   begin
      Execute (Self.Session.DB, SQL_Delete (DBA.Users, DBA.Users.Id = D.ORM_Id));
   end Internal_Delete;

   ---------------------------
   -- Internal_Query_Config --
   ---------------------------

   procedure Internal_Query_Config
     (Fields    : in out SQL_Field_List;
      From      : out SQL_Table_List;
      Criteria  : in out Sql_Criteria;
      Depth     : Natural;
      Follow_LJ : Boolean;
      Pk_Only   : Boolean := False) is
   begin
      Do_Query_Config(Fields, From, Criteria,
         0, Alias_Config, Depth, Follow_LJ, PK_Only);
   end Internal_Query_Config;

   ----------------------------
   -- Internal_Query_Invites --
   ----------------------------

   procedure Internal_Query_Invites
     (Fields    : in out SQL_Field_List;
      From      : out SQL_Table_List;
      Criteria  : in out Sql_Criteria;
      Depth     : Natural;
      Follow_LJ : Boolean;
      Pk_Only   : Boolean := False) is
   begin
      Do_Query_Invites(Fields, From, Criteria,
         0, Alias_Invites, Depth, Follow_LJ, PK_Only);
   end Internal_Query_Invites;

   -----------------------------
   -- Internal_Query_Torrents --
   -----------------------------

   procedure Internal_Query_Torrents
     (Fields    : in out SQL_Field_List;
      From      : out SQL_Table_List;
      Criteria  : in out Sql_Criteria;
      Depth     : Natural;
      Follow_LJ : Boolean;
      Pk_Only   : Boolean := False) is
   begin
      Do_Query_Torrents(Fields, From, Criteria,
         0, Alias_Torrents, Depth, Follow_LJ, PK_Only);
   end Internal_Query_Torrents;

   ---------------------------------------
   -- Internal_Query_User_Torrent_Stats --
   ---------------------------------------

   procedure Internal_Query_User_Torrent_Stats
     (Fields    : in out SQL_Field_List;
      From      : out SQL_Table_List;
      Criteria  : in out Sql_Criteria;
      Depth     : Natural;
      Follow_LJ : Boolean;
      Pk_Only   : Boolean := False) is
   begin
      if PK_Only then
         raise Program_Error with "Table User_Torrent_Stats has no primary key";
      end if;
      Do_Query_User_Torrent_Stats(Fields, From, Criteria,
         0, Alias_User_Torrent_Stats, Depth, Follow_LJ);
   end Internal_Query_User_Torrent_Stats;

   --------------------------
   -- Internal_Query_Users --
   --------------------------

   procedure Internal_Query_Users
     (Fields    : in out SQL_Field_List;
      From      : out SQL_Table_List;
      Criteria  : in out Sql_Criteria;
      Depth     : Natural;
      Follow_LJ : Boolean;
      Pk_Only   : Boolean := False) is
   begin
      Do_Query_Users(Fields, From, Criteria,
         0, Alias_Users, Depth, Follow_LJ, PK_Only);
   end Internal_Query_Users;

   ----------------
   -- Invited_By --
   ----------------

   function Invited_By (Self : User'Class) return Invites_Managers is
   begin
      return Filter (All_Invites, For_User => Self.Id);
   end Invited_By;

   ----------------
   -- Invited_By --
   ----------------

   function Invited_By (Self : Detached_User'Class) return Invites_Managers is
   begin
      return Filter (All_Invites, For_User => Self.Id);
   end Invited_By;

   ----------------
   -- Invited_By --
   ----------------

   function Invited_By (Self : I_Users_Managers'Class) return Invites_Managers
   is
      Q : constant SQL_Query := I_Users.Build_Query(Self, +DBA.Users.Id);
   begin
      return All_Invites.Filter
        (SQL_In(DBA.Invites.For_User, Q));
   end Invited_By;

   -------------
   -- Invites --
   -------------

   function Invites (Self : User'Class) return Invites_Managers is
   begin
      return Filter (All_Invites, By_User => Self.Id);
   end Invites;

   -------------
   -- Invites --
   -------------

   function Invites (Self : Detached_User'Class) return Invites_Managers is
   begin
      return Filter (All_Invites, By_User => Self.Id);
   end Invites;

   -------------
   -- Invites --
   -------------

   function Invites (Self : I_Users_Managers'Class) return Invites_Managers
   is
      Q : constant SQL_Query := I_Users.Build_Query(Self, +DBA.Users.Id);
   begin
      return All_Invites.Filter
        (SQL_In(DBA.Invites.By_User, Q));
   end Invites;

   ---------
   -- Key --
   ---------

   overriding function Key (Self : Config_Ddr) return Element_Key is
   begin
      if Self.ORM_Id = -1 then
         return (2000000, No_Primary_Key);
      else
         return (2000000, Self.ORM_Id);
      end if;
   end Key;

   ---------
   -- Key --
   ---------

   overriding function Key (Self : Invite_Ddr) return Element_Key is
   begin
      if Self.ORM_Id = -1 then
         return (4000000, No_Primary_Key);
      else
         return (4000000, Self.ORM_Id);
      end if;
   end Key;

   ---------
   -- Key --
   ---------

   overriding function Key (Self : Torrent_Ddr) return Element_Key is
   begin
      if Self.ORM_Id = -1 then
         return (0, No_Primary_Key);
      else
         return (0, Self.ORM_Id);
      end if;
   end Key;

   ---------
   -- Key --
   ---------

   overriding function Key (Self : User_Torrent_Stat_Ddr) return Element_Key
   is
      pragma Unreferenced (Self);
   begin
      --  Not cachable, since the PK is not a single integer field
      return (3000000, No_Primary_Key);
   end Key;

   ---------
   -- Key --
   ---------

   overriding function Key (Self : User_Ddr) return Element_Key is
   begin
      if Self.ORM_Id = -1 then
         return (1000000, No_Primary_Key);
      else
         return (1000000, Self.ORM_Id);
      end if;
   end Key;

   ----------------
   -- New_Config --
   ----------------

   function New_Config return Detached_Config'Class
   is
      Result : Detached_Config;
      Data   : Config_Ddr;
   begin
      Result.Set (Data);
      return Result;
   end New_Config;

   ----------------
   -- New_Invite --
   ----------------

   function New_Invite return Detached_Invite'Class
   is
      Result : Detached_Invite;
      Data   : Invite_Ddr;
   begin
      Result.Set (Data);
      return Result;
   end New_Invite;

   -----------------
   -- New_Torrent --
   -----------------

   function New_Torrent return Detached_Torrent'Class
   is
      Result : Detached_Torrent;
      Data   : Torrent_Ddr;
   begin
      Result.Set (Data);
      return Result;
   end New_Torrent;

   --------------
   -- New_User --
   --------------

   function New_User return Detached_User'Class
   is
      Result : Detached_User;
      Data   : User_Ddr;
   begin
      Result.Set (Data);
      return Result;
   end New_User;

   ---------------------------
   -- New_User_Torrent_Stat --
   ---------------------------

   function New_User_Torrent_Stat return Detached_User_Torrent_Stat'Class
   is
      Result : Detached_User_Torrent_Stat;
      Data   : User_Torrent_Stat_Ddr;
   begin
      Result.Set (Data);
      return Result;
   end New_User_Torrent_Stat;

   ----------------
   -- On_Persist --
   ----------------

   overriding procedure On_Persist (Self : Detached_Invite)
   is
      D : constant Invite_Data := Invite_Data (Self.Unchecked_Get);
   begin
      if Persist_Cascade (Self.Session) then
         if D.ORM_FK_By_User /= null then
            Self.Session.Persist (D.ORM_FK_By_User.all);
         end if;
         if D.ORM_FK_For_User /= null then
            Self.Session.Persist (D.ORM_FK_For_User.all);
         end if;
      end if;
   end On_Persist;

   ----------------
   -- On_Persist --
   ----------------

   overriding procedure On_Persist (Self : Detached_Torrent)
   is
      D : constant Torrent_Data := Torrent_Data (Self.Unchecked_Get);
   begin
      if Persist_Cascade (Self.Session) then
         if D.ORM_FK_Created_By /= null then
            Self.Session.Persist (D.ORM_FK_Created_By.all);
         end if;
      end if;
   end On_Persist;

   ----------------
   -- On_Persist --
   ----------------

   overriding procedure On_Persist (Self : Detached_User_Torrent_Stat)
   is
      D : constant User_Torrent_Stat_Data := User_Torrent_Stat_Data (Self.Unchecked_Get);
   begin
      if Persist_Cascade (Self.Session) then
         if D.ORM_FK_By_User /= null then
            Self.Session.Persist (D.ORM_FK_By_User.all);
         end if;
         if D.ORM_FK_Of_Torrent /= null then
            Self.Session.Persist (D.ORM_FK_Of_Torrent.all);
         end if;
      end if;
   end On_Persist;

   -------------------
   -- Set_Activated --
   -------------------

   procedure Set_Activated (Self : Detached_Invite; Value : Boolean)
   is
      D : constant Invite_Data := Invite_Data (Self.Unchecked_Get);
   begin
      D.ORM_Activated := Value;
      Self.Set_Modified (3);
   end Set_Activated;

   -----------------
   -- Set_By_User --
   -----------------

   procedure Set_By_User (Self : Detached_User_Torrent_Stat; Value : Integer)
   is
      D : constant User_Torrent_Stat_Data := User_Torrent_Stat_Data (Self.Unchecked_Get);
   begin
      Unchecked_Free (D.ORM_FK_By_User);
      D.ORM_By_User := Value;
      Self.Set_Modified (1);
   end Set_By_User;

   -----------------
   -- Set_By_User --
   -----------------

   procedure Set_By_User
     (Self  : Detached_User_Torrent_Stat;
      Value : Detached_User'Class)
   is
      D : constant User_Torrent_Stat_Data := User_Torrent_Stat_Data (Self.Unchecked_Get);
   begin
      Unchecked_Free (D.ORM_FK_By_User);
      D.ORM_By_User := Value.Id;
      D.ORM_FK_By_User := new Detached_User'Class'(Value);

      Self.Set_Modified (1);
      if Persist_Cascade (Self.Session) then
         Self.Session.Persist (D.ORM_FK_By_User.all);
      end if;
   end Set_By_User;

   -----------------
   -- Set_By_User --
   -----------------

   procedure Set_By_User (Self : Detached_Invite; Value : Integer)
   is
      D : constant Invite_Data := Invite_Data (Self.Unchecked_Get);
   begin
      Unchecked_Free (D.ORM_FK_By_User);
      D.ORM_By_User := Value;
      Self.Set_Modified (4);
   end Set_By_User;

   -----------------
   -- Set_By_User --
   -----------------

   procedure Set_By_User (Self : Detached_Invite; Value : Detached_User'Class)
   is
      D : constant Invite_Data := Invite_Data (Self.Unchecked_Get);
   begin
      Unchecked_Free (D.ORM_FK_By_User);
      D.ORM_By_User := Value.Id;
      D.ORM_FK_By_User := new Detached_User'Class'(Value);

      Self.Set_Modified (4);
      if Persist_Cascade (Self.Session) then
         Self.Session.Persist (D.ORM_FK_By_User.all);
      end if;
   end Set_By_User;

   --------------------
   -- Set_Created_By --
   --------------------

   procedure Set_Created_By (Self : Detached_Torrent; Value : Integer)
   is
      D : constant Torrent_Data := Torrent_Data (Self.Unchecked_Get);
   begin
      Unchecked_Free (D.ORM_FK_Created_By);
      D.ORM_Created_By := Value;
      Self.Set_Modified (3);
   end Set_Created_By;

   --------------------
   -- Set_Created_By --
   --------------------

   procedure Set_Created_By
     (Self  : Detached_Torrent;
      Value : Detached_User'Class)
   is
      D : constant Torrent_Data := Torrent_Data (Self.Unchecked_Get);
   begin
      Unchecked_Free (D.ORM_FK_Created_By);
      D.ORM_Created_By := Value.Id;
      D.ORM_FK_Created_By := new Detached_User'Class'(Value);

      Self.Set_Modified (3);
      if Persist_Cascade (Self.Session) then
         Self.Session.Persist (D.ORM_FK_Created_By.all);
      end if;
   end Set_Created_By;

   --------------------
   -- Set_Downloaded --
   --------------------

   procedure Set_Downloaded (Self : Detached_User; Value : Long_Long_Integer)
   is
      D : constant User_Data := User_Data (Self.Unchecked_Get);
   begin
      D.ORM_Downloaded := Value;
      Self.Set_Modified (6);
   end Set_Downloaded;

   --------------------
   -- Set_Downloaded --
   --------------------

   procedure Set_Downloaded
     (Self  : Detached_User_Torrent_Stat;
      Value : Long_Long_Integer)
   is
      D : constant User_Torrent_Stat_Data := User_Torrent_Stat_Data (Self.Unchecked_Get);
   begin
      D.ORM_Downloaded := Value;
      Self.Set_Modified (4);
   end Set_Downloaded;

   ------------------
   -- Set_For_User --
   ------------------

   procedure Set_For_User (Self : Detached_Invite; Value : Integer)
   is
      D : constant Invite_Data := Invite_Data (Self.Unchecked_Get);
   begin
      Unchecked_Free (D.ORM_FK_For_User);
      D.ORM_For_User := Value;
      Self.Set_Modified (5);
   end Set_For_User;

   ------------------
   -- Set_For_User --
   ------------------

   procedure Set_For_User (Self : Detached_Invite; Value : Detached_User'Class)
   is
      D : constant Invite_Data := Invite_Data (Self.Unchecked_Get);
   begin
      Unchecked_Free (D.ORM_FK_For_User);
      D.ORM_For_User := Value.Id;
      D.ORM_FK_For_User := new Detached_User'Class'(Value);

      Self.Set_Modified (5);
      if Persist_Cascade (Self.Session) then
         Self.Session.Persist (D.ORM_FK_For_User.all);
      end if;
   end Set_For_User;

   -------------------
   -- Set_Info_Hash --
   -------------------

   procedure Set_Info_Hash (Self : Detached_Torrent; Value : String)
   is
      D : constant Torrent_Data := Torrent_Data (Self.Unchecked_Get);
   begin
      D.ORM_Info_Hash := To_Unbounded_String (Value);
      Self.Set_Modified (2);
   end Set_Info_Hash;

   --------------------
   -- Set_Of_Torrent --
   --------------------

   procedure Set_Of_Torrent (Self : Detached_User_Torrent_Stat; Value : Integer)
   is
      D : constant User_Torrent_Stat_Data := User_Torrent_Stat_Data (Self.Unchecked_Get);
   begin
      Unchecked_Free (D.ORM_FK_Of_Torrent);
      D.ORM_Of_Torrent := Value;
      Self.Set_Modified (2);
   end Set_Of_Torrent;

   --------------------
   -- Set_Of_Torrent --
   --------------------

   procedure Set_Of_Torrent
     (Self  : Detached_User_Torrent_Stat;
      Value : Detached_Torrent'Class)
   is
      D : constant User_Torrent_Stat_Data := User_Torrent_Stat_Data (Self.Unchecked_Get);
   begin
      Unchecked_Free (D.ORM_FK_Of_Torrent);
      D.ORM_Of_Torrent := Value.Id;
      D.ORM_FK_Of_Torrent := new Detached_Torrent'Class'(Value);

      Self.Set_Modified (2);
      if Persist_Cascade (Self.Session) then
         Self.Session.Persist (D.ORM_FK_Of_Torrent.all);
      end if;
   end Set_Of_Torrent;

   -----------------
   -- Set_Passkey --
   -----------------

   procedure Set_Passkey (Self : Detached_User; Value : String)
   is
      D : constant User_Data := User_Data (Self.Unchecked_Get);
   begin
      D.ORM_Passkey := To_Unbounded_String (Value);
      Self.Set_Modified (4);
   end Set_Passkey;

   ------------------
   -- Set_Password --
   ------------------

   procedure Set_Password (Self : Detached_User; Value : String)
   is
      D : constant User_Data := User_Data (Self.Unchecked_Get);
   begin
      D.ORM_Password := To_Unbounded_String (Value);
      Self.Set_Modified (3);
   end Set_Password;

   ------------------
   -- Set_Uploaded --
   ------------------

   procedure Set_Uploaded (Self : Detached_User; Value : Long_Long_Integer)
   is
      D : constant User_Data := User_Data (Self.Unchecked_Get);
   begin
      D.ORM_Uploaded := Value;
      Self.Set_Modified (5);
   end Set_Uploaded;

   ------------------
   -- Set_Uploaded --
   ------------------

   procedure Set_Uploaded
     (Self  : Detached_User_Torrent_Stat;
      Value : Long_Long_Integer)
   is
      D : constant User_Torrent_Stat_Data := User_Torrent_Stat_Data (Self.Unchecked_Get);
   begin
      D.ORM_Uploaded := Value;
      Self.Set_Modified (3);
   end Set_Uploaded;

   ------------------
   -- Set_Username --
   ------------------

   procedure Set_Username (Self : Detached_User; Value : String)
   is
      D : constant User_Data := User_Data (Self.Unchecked_Get);
   begin
      D.ORM_Username := To_Unbounded_String (Value);
      Self.Set_Modified (2);
   end Set_Username;

   ---------------
   -- Set_Value --
   ---------------

   procedure Set_Value (Self : Detached_Invite; Value : String)
   is
      D : constant Invite_Data := Invite_Data (Self.Unchecked_Get);
   begin
      D.ORM_Value := To_Unbounded_String (Value);
      Self.Set_Modified (2);
   end Set_Value;

   -----------------
   -- Set_Version --
   -----------------

   procedure Set_Version (Self : Detached_Config; Value : Integer)
   is
      D : constant Config_Data := Config_Data (Self.Unchecked_Get);
   begin
      D.ORM_Version := Value;
      Self.Set_Modified (2);
   end Set_Version;

   -------------------
   -- Torrent_Stats --
   -------------------

   function Torrent_Stats (Self : User'Class) return User_Torrent_Stats_Managers
   is
   begin
      return Filter (All_User_Torrent_Stats, By_User => Self.Id);
   end Torrent_Stats;

   -------------------
   -- Torrent_Stats --
   -------------------

   function Torrent_Stats
     (Self : Detached_User'Class)
     return User_Torrent_Stats_Managers is
   begin
      return Filter (All_User_Torrent_Stats, By_User => Self.Id);
   end Torrent_Stats;

   -------------------
   -- Torrent_Stats --
   -------------------

   function Torrent_Stats
     (Self : I_Users_Managers'Class)
     return User_Torrent_Stats_Managers
   is
      Q : constant SQL_Query := I_Users.Build_Query(Self, +DBA.Users.Id);
   begin
      return All_User_Torrent_Stats.Filter
        (SQL_In(DBA.User_Torrent_Stats.By_User, Q));
   end Torrent_Stats;
end Orm;

