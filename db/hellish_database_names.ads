with GNATCOLL.SQL; use GNATCOLL.SQL;
package Hellish_Database_Names is
   pragma Style_Checks (Off);
   TC_Config : aliased constant String := "config";
   Ta_Config : constant Cst_String_Access := TC_Config'Access;
   TC_Torrents : aliased constant String := "torrents";
   Ta_Torrents : constant Cst_String_Access := TC_Torrents'Access;

   NC_Bencoded : aliased constant String := "bencoded";
   N_Bencoded : constant Cst_String_Access := NC_bencoded'Access;
   NC_Filename : aliased constant String := "filename";
   N_Filename : constant Cst_String_Access := NC_filename'Access;
   NC_Id : aliased constant String := "id";
   N_Id : constant Cst_String_Access := NC_id'Access;
   NC_Version : aliased constant String := """version""";
   N_Version : constant Cst_String_Access := NC_version'Access;
end Hellish_Database_Names;
