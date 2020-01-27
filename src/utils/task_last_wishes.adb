with Ada.Task_Termination;

package body Task_Last_Wishes is

   use Ada.Task_Termination;
   use Ada.Task_Identification;

   protected body Failure is

      procedure Response (Departed : Ada.Task_Identification.Task_Id) is
      begin
         raise Program_Error with Image (Departed) & " terminated abnormally!";
      end Response;

   end Failure;

begin
   Set_Dependents_Fallback_Handler (Failure.Response'Access);
   --  called by the environment task during library unit elaboration, so
   --  covers all application tasks, but not the environment task itself
   --  since no task is dependent upon itself
end Task_Last_Wishes;
