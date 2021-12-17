with GNATCOLL.SQL; use GNATCOLL.SQL;
pragma Warnings (Off, "no entities of * are referenced");
pragma Warnings (Off, "use clause for package * has no effect");
with GNATCOLL.SQL_Fields; use GNATCOLL.SQL_Fields;
pragma Warnings (On, "no entities of * are referenced");
pragma Warnings (On, "use clause for package * has no effect");
with GNATCOLL.SQL.Exec;
with Hellish_Database_Names; use Hellish_Database_Names;
package Hellish_Database is
   pragma Style_Checks (Off);
   pragma Elaborate_Body;

   type T_Abstract_Config
      (Instance : Cst_String_Access;
       Index    : Integer)
   is abstract new SQL_Table (Ta_Config, Instance, Index) with
   record
      Id : SQL_Field_Integer (Ta_Config, Instance, N_Id, Index);
      Version : SQL_Field_Integer (Ta_Config, Instance, N_Version, Index);
   end record;

   type T_Config (Instance : Cst_String_Access)
      is new T_Abstract_Config (Instance, -1) with null record;
   --  To use named aliases of the table in a query
   --  Use Instance=>null to use the default name.

   type T_Numbered_Config (Index : Integer)
      is new T_Abstract_Config (null, Index) with null record;
   --  To use aliases in the form name1, name2,...

   type T_Abstract_Torrents
      (Instance : Cst_String_Access;
       Index    : Integer)
   is abstract new SQL_Table (Ta_Torrents, Instance, Index) with
   record
      Id : SQL_Field_Integer (Ta_Torrents, Instance, N_Id, Index);
      Torrent_File : SQL_Field_Text (Ta_Torrents, Instance, N_Torrent_File, Index);
      Filename : SQL_Field_Text (Ta_Torrents, Instance, N_Filename, Index);
      Created_By : SQL_Field_Integer (Ta_Torrents, Instance, N_Created_By, Index);
   end record;

   type T_Torrents (Instance : Cst_String_Access)
      is new T_Abstract_Torrents (Instance, -1) with null record;
   --  To use named aliases of the table in a query
   --  Use Instance=>null to use the default name.

   type T_Numbered_Torrents (Index : Integer)
      is new T_Abstract_Torrents (null, Index) with null record;
   --  To use aliases in the form name1, name2,...

   type T_Abstract_Users
      (Instance : Cst_String_Access;
       Index    : Integer)
   is abstract new SQL_Table (Ta_Users, Instance, Index) with
   record
      Id : SQL_Field_Integer (Ta_Users, Instance, N_Id, Index);
      Username : SQL_Field_Text (Ta_Users, Instance, N_Username, Index);
      Password : SQL_Field_Text (Ta_Users, Instance, N_Password, Index);
   end record;

   type T_Users (Instance : Cst_String_Access)
      is new T_Abstract_Users (Instance, -1) with null record;
   --  To use named aliases of the table in a query
   --  Use Instance=>null to use the default name.

   type T_Numbered_Users (Index : Integer)
      is new T_Abstract_Users (null, Index) with null record;
   --  To use aliases in the form name1, name2,...

   function FK (Self : T_Torrents'Class; Foreign : T_Users'Class) return SQL_Criteria;
   Config : T_Config (null);
   Torrents : T_Torrents (null);
   Users : T_Users (null);

   procedure Create_Database
      (DB : not null access GNATCOLL.SQL.Exec.Database_Connection_Record'Class);
   --  Create the database and its initial contents
   --  The SQL is not automatically committed
end Hellish_Database;
