------------------------------------------------------------------------------
--                                                                          --
--                      Copyright (C) 2017, AdaCore                         --
--                                                                          --
--  Redistribution and use in source and binary forms, with or without      --
--  modification, are permitted provided that the following conditions are  --
--  met:                                                                    --
--     1. Redistributions of source code must retain the above copyright    --
--        notice, this list of conditions and the following disclaimer.     --
--     2. Redistributions in binary form must reproduce the above copyright --
--        notice, this list of conditions and the following disclaimer in   --
--        the documentation and/or other materials provided with the        --
--        distribution.                                                     --
--     3. Neither the name of STMicroelectronics nor the names of its       --
--        contributors may be used to endorse or promote products derived   --
--        from this software without specific prior written permission.     --
--                                                                          --
--   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS    --
--   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT      --
--   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR  --
--   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT   --
--   HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, --
--   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT       --
--   LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,  --
--   DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY  --
--   THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT    --
--   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE  --
--   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.   --
--                                                                          --
------------------------------------------------------------------------------

with STM32.Device;  use STM32.Device;

with Poll_For_Continuous_State;

package body Discrete_Inputs is

   ---------------------
   -- Poll_For_Active --
   ---------------------

   procedure Poll_For_Active is new Poll_For_Continuous_State
     (Input           => Discrete_Input,
      In_Target_State => Active_Indicated)
     with Inline;

   ------------------
   -- Await_Active --
   ------------------

   procedure Await_Active
     (This          : Discrete_Input;
      Debounce_Time : Time_Span := Default_Debounce_Time)
   renames Poll_For_Active;

   -----------------------
   -- Poll_For_Inactive --
   -----------------------

   procedure Poll_For_Inactive is new Poll_For_Continuous_State
     (Input           => Discrete_Input,
      In_Target_State => Inactive_Indicated)
     with Inline;

   --------------------
   -- Await_Inactive --
   --------------------

   procedure Await_Inactive
     (This          : Discrete_Input;
      Debounce_Time : Time_Span := Default_Debounce_Time)
   renames Poll_For_Inactive;

   -------------------------
   -- Initialize_Hardware --
   -------------------------

   procedure Initialize_Hardware (This : in out Discrete_Input) is
      Config : GPIO_Port_Configuration;
   begin
      Enable_Clock (This.Pin.all);

      Config.Mode := Mode_In;
      Config.Resistors := (if This.Active = High then Pull_Down else Pull_Up);
      Config.Speed := Speed_50MHz;
      This.Pin.Configure_IO (Config);
   end Initialize_Hardware;

end Discrete_Inputs;
