with GNATCOLL.SQL; use GNATCOLL.SQL;
package Hellish_Database_Names is
   pragma Style_Checks (Off);
   TC_Config : aliased constant String := "config";
   Ta_Config : constant Cst_String_Access := TC_Config'Access;
   TC_Torrents : aliased constant String := "torrents";
   Ta_Torrents : constant Cst_String_Access := TC_Torrents'Access;
   TC_Users : aliased constant String := "users";
   Ta_Users : constant Cst_String_Access := TC_Users'Access;

   NC_Created_By : aliased constant String := "created_by";
   N_Created_By : constant Cst_String_Access := NC_created_by'Access;
   NC_Filename : aliased constant String := "filename";
   N_Filename : constant Cst_String_Access := NC_filename'Access;
   NC_Id : aliased constant String := "id";
   N_Id : constant Cst_String_Access := NC_id'Access;
   NC_Password : aliased constant String := """password""";
   N_Password : constant Cst_String_Access := NC_password'Access;
   NC_Torrent_File : aliased constant String := "torrent_file";
   N_Torrent_File : constant Cst_String_Access := NC_torrent_file'Access;
   NC_Username : aliased constant String := "username";
   N_Username : constant Cst_String_Access := NC_username'Access;
   NC_Version : aliased constant String := """version""";
   N_Version : constant Cst_String_Access := NC_version'Access;
end Hellish_Database_Names;
