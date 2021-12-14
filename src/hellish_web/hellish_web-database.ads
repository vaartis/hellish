with Gnatcoll.Sql.Exec; use Gnatcoll.Sql.Exec;
with Gnatcoll.Sql.Postgres; use Gnatcoll.Sql.Postgres;

package Hellish_Web.Database is
   Db_Desc : Database_Description := Setup("hellish", User => "postgres");

   procedure Init;
end;
