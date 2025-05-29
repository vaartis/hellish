with Ada.Task_Termination; use Ada.Task_Termination;
with Ada.Task_Identification; use Ada.Task_Identification;
with Ada.Exceptions; use Ada.Exceptions;

package Hellish_Termination_Handler is
   protected Termination_Handlers is
      procedure Termination_Handler(Unused_Cause : Cause_Of_Termination;
                                    Id : Task_Id;
                                    E : Exception_Occurrence);
   end Termination_Handlers;
end Hellish_Termination_Handler;
