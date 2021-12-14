
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
      ( Torrent_DDR, Torrent_Data);

   F_Config_Id      : constant := 0;
   F_Config_Version : constant := 1;
   Alias_Config : constant Alias_Array := (0 => -1);
   F_Torrents_Id       : constant := 0;
   F_Torrents_Bencoded : constant := 1;
   F_Torrents_Filename : constant := 2;
   Alias_Torrents : constant Alias_Array := (0 => -1);

   pragma Warnings (On);
   function Detach_No_Lookup
     (Self    : Config'Class;
      Session : Session_Type)
     return Detached_Config'Class;
   function Detach_No_Lookup
     (Self    : Torrent'Class;
      Session : Session_Type)
     return Detached_Torrent'Class;
   --  Same as Detach, but does not check the session cache Same as Detach,
   --  but does not check the session cache

   procedure Do_Query_Config
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

   --------------
   -- Bencoded --
   --------------

   function Bencoded (Self : Torrent) return String is
   begin
      return String_Value (Self, F_Torrents_Bencoded);
   end Bencoded;

   --------------
   -- Bencoded --
   --------------

   function Bencoded (Self : Detached_Torrent) return String is
   begin
      return To_String (Torrent_Data (Self.Unchecked_Get).ORM_Bencoded);
   end Bencoded;

   --------------
   -- Filename --
   --------------

   function Filename (Self : Torrent) return String is
   begin
      return String_Value (Self, F_Torrents_Filename);
   end Filename;

   --------------
   -- Filename --
   --------------

   function Filename (Self : Detached_Torrent) return String is
   begin
      return To_String (Torrent_Data (Self.Unchecked_Get).ORM_Filename);
   end Filename;

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
     (Self    : Torrent'Class;
      Session : Session_Type)
     return Detached_Torrent'Class
   is
      Default : Detached_Torrent;
      Result  : Detached_Torrent'Class := Detached_Torrent'Class (Session.Factory (Self, Default));
      Tmp     : Torrent_Data;
   begin
      if Result.Is_Null then
         Result.Set (Torrent_DDR'
              (Detached_Data with Field_Count => 3, others => <>));
      end if;

      Tmp := Torrent_Data (Result.Unchecked_Get);

      Tmp.ORM_Bencoded    := To_Unbounded_String (String_Value (Self, F_Torrents_Bencoded));
      Tmp.ORM_Filename    := To_Unbounded_String (String_Value (Self, F_Torrents_Filename));
      Tmp.ORM_Id          := Integer_Value (Self, F_Torrents_Id);
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
      pragma Unreferenced (Criteria, Depth, Follow_LJ);
      Table : T_Numbered_Torrents(Aliases(Base));
   begin
      if PK_Only then
         Fields := Fields & Table.Id;
      else
         Fields := Fields & Table.Id
         & Table.Bencoded
         & Table.Filename;
      end if;
      From := Empty_Table_List;
   end Do_Query_Torrents;

   ------------
   -- Filter --
   ------------

   function Filter
     (Self     : Torrents_Managers'Class;
      Id       : Integer := -1;
      Bencoded : String := No_Update;
      Filename : String := No_Update)
     return Torrents_Managers
   is
      C      : Sql_Criteria := No_Criteria;
      Result : Torrents_Managers;
   begin
      if Id /= -1 then
         C := C and DBA.Torrents.Id = Id;
      end if;
      if Bencoded /= No_Update then
         C := C and DBA.Torrents.Bencoded = Bencoded;
      end if;
      if Filename /= No_Update then
         C := C and DBA.Torrents.Filename = Filename;
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

   overriding procedure Free (Self : in out Torrent_Ddr) is
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
     return Detached_Config'Class is
   begin
      return Detached_Config'Class (Session.From_Cache ((1000000, Id), No_Detached_Config));
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
         A := A & (DBA.Torrents.Bencoded = To_String (D.ORM_Bencoded));
      end if;
      if Mask (3) then
         A := A & (DBA.Torrents.Filename = To_String (D.ORM_Filename));
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

   overriding procedure Internal_Delete (Self : Detached_Torrent)
   is
      D : constant Torrent_Data := Torrent_Data (Self.Unchecked_Get);
   begin
      Execute (Self.Session.DB, SQL_Delete (DBA.Torrents, DBA.Torrents.Id = D.ORM_Id));
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

   ---------
   -- Key --
   ---------

   overriding function Key (Self : Config_Ddr) return Element_Key is
   begin
      if Self.ORM_Id = -1 then
         return (1000000, No_Primary_Key);
      else
         return (1000000, Self.ORM_Id);
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

   ------------------
   -- Set_Bencoded --
   ------------------

   procedure Set_Bencoded (Self : Detached_Torrent; Value : String)
   is
      D : constant Torrent_Data := Torrent_Data (Self.Unchecked_Get);
   begin
      D.ORM_Bencoded := To_Unbounded_String (Value);
      Self.Set_Modified (2);
   end Set_Bencoded;

   ------------------
   -- Set_Filename --
   ------------------

   procedure Set_Filename (Self : Detached_Torrent; Value : String)
   is
      D : constant Torrent_Data := Torrent_Data (Self.Unchecked_Get);
   begin
      D.ORM_Filename := To_Unbounded_String (Value);
      Self.Set_Modified (3);
   end Set_Filename;

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
end Orm;

