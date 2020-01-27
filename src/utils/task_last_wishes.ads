--  This package defines what happens when a task terminates, which is unexpected in Ravenscar-based profiles. In this case the last chance handler is invoked via Program_Error with a string indicating which task terminated.

with Ada.Task_Identification;

package Task_Last_Wishes is

   protected Failure is
      procedure Response (Departed : Ada.Task_Identification.Task_Id);
   end Failure;

end Task_Last_Wishes;
