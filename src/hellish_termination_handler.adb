with Ada.Text_IO; use Ada.Text_IO;

package body Hellish_Termination_Handler is
   protected body Termination_Handlers is
      procedure Termination_Handler(Unused_Cause : Cause_Of_Termination;
                                    Id : Task_Id;
                                    E : Exception_Occurrence) is
      begin
         Put_Line("!! TASK CRASHED " & Image(Id));
         Put_Line(Exception_Information(E));
      end Termination_Handler;
   end Termination_Handlers;
end Hellish_Termination_Handler;
