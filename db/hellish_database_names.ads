with GNATCOLL.SQL; use GNATCOLL.SQL;
package Hellish_Database_Names is
   pragma Style_Checks (Off);
   TC_Config : aliased constant String := "config";
   Ta_Config : constant Cst_String_Access := TC_Config'Access;
   TC_Invites : aliased constant String := "invites";
   Ta_Invites : constant Cst_String_Access := TC_Invites'Access;
   TC_Torrents : aliased constant String := "torrents";
   Ta_Torrents : constant Cst_String_Access := TC_Torrents'Access;
   TC_User_Torrent_Stats : aliased constant String := "user_torrent_stats";
   Ta_User_Torrent_Stats : constant Cst_String_Access := TC_User_Torrent_Stats'Access;
   TC_Users : aliased constant String := "users";
   Ta_Users : constant Cst_String_Access := TC_Users'Access;

   NC_Activated : aliased constant String := "activated";
   N_Activated : constant Cst_String_Access := NC_activated'Access;
   NC_By_User : aliased constant String := "by_user";
   N_By_User : constant Cst_String_Access := NC_by_user'Access;
   NC_Created_By : aliased constant String := "created_by";
   N_Created_By : constant Cst_String_Access := NC_created_by'Access;
   NC_Downloaded : aliased constant String := "downloaded";
   N_Downloaded : constant Cst_String_Access := NC_downloaded'Access;
   NC_For_User : aliased constant String := "for_user";
   N_For_User : constant Cst_String_Access := NC_for_user'Access;
   NC_Id : aliased constant String := "id";
   N_Id : constant Cst_String_Access := NC_id'Access;
   NC_Info_Hash : aliased constant String := "info_hash";
   N_Info_Hash : constant Cst_String_Access := NC_info_hash'Access;
   NC_Of_Torrent : aliased constant String := "of_torrent";
   N_Of_Torrent : constant Cst_String_Access := NC_of_torrent'Access;
   NC_Passkey : aliased constant String := "passkey";
   N_Passkey : constant Cst_String_Access := NC_passkey'Access;
   NC_Password : aliased constant String := """password""";
   N_Password : constant Cst_String_Access := NC_password'Access;
   NC_Uploaded : aliased constant String := "uploaded";
   N_Uploaded : constant Cst_String_Access := NC_uploaded'Access;
   NC_Username : aliased constant String := "username";
   N_Username : constant Cst_String_Access := NC_username'Access;
   NC_Value : aliased constant String := """value""";
   N_Value : constant Cst_String_Access := NC_value'Access;
   NC_Version : aliased constant String := """version""";
   N_Version : constant Cst_String_Access := NC_version'Access;
end Hellish_Database_Names;
