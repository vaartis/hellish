with Ada.Text_IO; use Ada.Text_IO;
with Ada.Task_Termination; use Ada.Task_Termination;

with Hellish_Termination_Handler; use Hellish_Termination_Handler;
with Hellish_Web.Peers; use Hellish_Web.Peers;
with Hellish_Web.Database;

with Orm; use Orm;

package body Hellish_Web.Jobs is
   task body Prune_Expired_Invites is
   begin
      delay 5.0;

      loop
         Database.Invite_Prune;
         Put_Line("Pruning expired invites");
         -- Every two hours or so
         delay 60.0 * 60.0 * 2.0;
      end loop;
   end Prune_Expired_Invites;

   task body Prune_Stale_Peers is
      Torrent_List : Direct_Torrent_List;
   begin
      -- All peers are assumed to join on start, so wait for half an hour before checking
      delay 60.0 * 30.0;

      loop
         Put_Line("Pruning stale peers");
         Torrent_List := Database.Get_Active_Torrents;
         while Torrent_List.Has_Row loop
            Peers.Protected_Map.Remove_Stale_Peers(Torrent_List.Element.Info_Hash);
            Torrent_List.Next;
         end loop;

         -- Every hour
         delay 60.0 * 60.0;
      end loop;
   end Prune_Stale_Peers;

   procedure Start is
   begin
      Set_Specific_Handler(Prune_Expired_Invites'Identity, Termination_Handlers.Termination_Handler'Access);
      Set_Specific_Handler(Prune_Stale_Peers'Identity, Termination_Handlers.Termination_Handler'Access);
   end;
end Hellish_Web.Jobs;
