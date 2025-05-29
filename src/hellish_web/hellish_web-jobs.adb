with Ada.Text_IO; use Ada.Text_IO;
with Ada.Task_Termination; use Ada.Task_Termination;

with Hellish_Termination_Handler; use Hellish_Termination_Handler;
with Hellish_Web.Database;

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

   procedure Start is
   begin
      Set_Specific_Handler(Prune_Expired_Invites'Identity, Termination_Handlers.Termination_Handler'Access);
   end;
end Hellish_Web.Jobs;
