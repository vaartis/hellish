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

   type T_Abstract_Invites
      (Instance : Cst_String_Access;
       Index    : Integer)
   is abstract new SQL_Table (Ta_Invites, Instance, Index) with
   record
      Id : SQL_Field_Integer (Ta_Invites, Instance, N_Id, Index);
      Value : SQL_Field_Text (Ta_Invites, Instance, N_Value, Index);
      Activated : SQL_Field_Boolean (Ta_Invites, Instance, N_Activated, Index);
      By_User : SQL_Field_Integer (Ta_Invites, Instance, N_By_User, Index);
      For_User : SQL_Field_Integer (Ta_Invites, Instance, N_For_User, Index);
   end record;

   type T_Invites (Instance : Cst_String_Access)
      is new T_Abstract_Invites (Instance, -1) with null record;
   --  To use named aliases of the table in a query
   --  Use Instance=>null to use the default name.

   type T_Numbered_Invites (Index : Integer)
      is new T_Abstract_Invites (null, Index) with null record;
   --  To use aliases in the form name1, name2,...

   type T_Abstract_Posts
      (Instance : Cst_String_Access;
       Index    : Integer)
   is abstract new SQL_Table (Ta_Posts, Instance, Index) with
   record
      Id : SQL_Field_Integer (Ta_Posts, Instance, N_Id, Index);
      Title : SQL_Field_Text (Ta_Posts, Instance, N_Title, Index);
      --  NULL for thread replies

      Content : SQL_Field_Text (Ta_Posts, Instance, N_Content, Index);
      By_User : SQL_Field_Integer (Ta_Posts, Instance, N_By_User, Index);
      Parent_Post : SQL_Field_Integer (Ta_Posts, Instance, N_Parent_Post, Index);
      --  The ID of the post that started the thread

      Flag : SQL_Field_Integer (Ta_Posts, Instance, N_Flag, Index);
      --  0 = nothing, 1 = news

      Parent_Torrent : SQL_Field_Integer (Ta_Posts, Instance, N_Parent_Torrent, Index);
      --  The ID of the torrent that started the thread

   end record;

   type T_Posts (Instance : Cst_String_Access)
      is new T_Abstract_Posts (Instance, -1) with null record;
   --  To use named aliases of the table in a query
   --  Use Instance=>null to use the default name.

   type T_Numbered_Posts (Index : Integer)
      is new T_Abstract_Posts (null, Index) with null record;
   --  To use aliases in the form name1, name2,...

   type T_Abstract_Torrents
      (Instance : Cst_String_Access;
       Index    : Integer)
   is abstract new SQL_Table (Ta_Torrents, Instance, Index) with
   record
      Id : SQL_Field_Integer (Ta_Torrents, Instance, N_Id, Index);
      Info_Hash : SQL_Field_Text (Ta_Torrents, Instance, N_Info_Hash, Index);
      --  The SHA1 hash of the torrent

      Created_By : SQL_Field_Integer (Ta_Torrents, Instance, N_Created_By, Index);
      Display_Name : SQL_Field_Text (Ta_Torrents, Instance, N_Display_Name, Index);
      Description : SQL_Field_Text (Ta_Torrents, Instance, N_Description, Index);
      Snatches : SQL_Field_Integer (Ta_Torrents, Instance, N_Snatches, Index);
      --  Total number of downloads

   end record;

   type T_Torrents (Instance : Cst_String_Access)
      is new T_Abstract_Torrents (Instance, -1) with null record;
   --  To use named aliases of the table in a query
   --  Use Instance=>null to use the default name.

   type T_Numbered_Torrents (Index : Integer)
      is new T_Abstract_Torrents (null, Index) with null record;
   --  To use aliases in the form name1, name2,...

   type T_Abstract_User_Torrent_Stats
      (Instance : Cst_String_Access;
       Index    : Integer)
   is abstract new SQL_Table (Ta_User_Torrent_Stats, Instance, Index) with
   record
      By_User : SQL_Field_Integer (Ta_User_Torrent_Stats, Instance, N_By_User, Index);
      Of_Torrent : SQL_Field_Integer (Ta_User_Torrent_Stats, Instance, N_Of_Torrent, Index);
      Uploaded : SQL_Field_Bigint (Ta_User_Torrent_Stats, Instance, N_Uploaded, Index);
      Downloaded : SQL_Field_Bigint (Ta_User_Torrent_Stats, Instance, N_Downloaded, Index);
   end record;

   type T_User_Torrent_Stats (Instance : Cst_String_Access)
      is new T_Abstract_User_Torrent_Stats (Instance, -1) with null record;
   --  To use named aliases of the table in a query
   --  Use Instance=>null to use the default name.

   type T_Numbered_User_Torrent_Stats (Index : Integer)
      is new T_Abstract_User_Torrent_Stats (null, Index) with null record;
   --  To use aliases in the form name1, name2,...

   type T_Abstract_Users
      (Instance : Cst_String_Access;
       Index    : Integer)
   is abstract new SQL_Table (Ta_Users, Instance, Index) with
   record
      Id : SQL_Field_Integer (Ta_Users, Instance, N_Id, Index);
      Username : SQL_Field_Text (Ta_Users, Instance, N_Username, Index);
      Password : SQL_Field_Text (Ta_Users, Instance, N_Password, Index);
      Passkey : SQL_Field_Text (Ta_Users, Instance, N_Passkey, Index);
      Uploaded : SQL_Field_Bigint (Ta_Users, Instance, N_Uploaded, Index);
      Downloaded : SQL_Field_Bigint (Ta_Users, Instance, N_Downloaded, Index);
      Role : SQL_Field_Integer (Ta_Users, Instance, N_Role, Index);
      --  0 = user, 1 = admin

   end record;

   type T_Users (Instance : Cst_String_Access)
      is new T_Abstract_Users (Instance, -1) with null record;
   --  To use named aliases of the table in a query
   --  Use Instance=>null to use the default name.

   type T_Numbered_Users (Index : Integer)
      is new T_Abstract_Users (null, Index) with null record;
   --  To use aliases in the form name1, name2,...

   function FK (Self : T_Posts'Class; Foreign : T_Users'Class) return SQL_Criteria;
   function FK (Self : T_Posts'Class; Foreign : T_Posts'Class) return SQL_Criteria;
   function FK (Self : T_Posts'Class; Foreign : T_Torrents'Class) return SQL_Criteria;
   function FK (Self : T_Torrents'Class; Foreign : T_Users'Class) return SQL_Criteria;
   function FK (Self : T_User_Torrent_Stats'Class; Foreign : T_Users'Class) return SQL_Criteria;
   function FK (Self : T_User_Torrent_Stats'Class; Foreign : T_Torrents'Class) return SQL_Criteria;
   Config : T_Config (null);
   Invites : T_Invites (null);
   Posts : T_Posts (null);
   Torrents : T_Torrents (null);
   User_Torrent_Stats : T_User_Torrent_Stats (null);
   Users : T_Users (null);

   procedure Create_Database
      (DB : not null access GNATCOLL.SQL.Exec.Database_Connection_Record'Class);
   --  Create the database and its initial contents
   --  The SQL is not automatically committed
end Hellish_Database;
