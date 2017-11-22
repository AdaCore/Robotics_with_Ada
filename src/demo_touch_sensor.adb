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

with Last_Chance_Handler;  pragma Unreferenced (Last_Chance_Handler);

with STM32.Device;      use STM32.Device;
with LCD_Std_Out;       use LCD_Std_Out;
with NXT.Touch_Sensors; use NXT.Touch_Sensors;
with Discrete_Inputs;   use Discrete_Inputs;
with Ada.Real_Time;     use Ada.Real_Time;

procedure Demo_Touch_Sensor is

   Button : Touch_Sensor (Pin => PB4'Access, Active => Low);
   --  The logic level choice is directly dependent upon the electronic circuit
   --  we are using to connect the sensor to the MCU. The proper choice is
   --  critical to correct behavior. The GPIO pin choice is arbitrary.

   Toggle_Count : Natural := 0;
   --  The touch sensor is a momentary switch. Once depressed, it will be
   --  released automatically when no longer held down. This variable keeps
   --  the count of these toggle events.

begin
   Clear_Screen;
   Button.Initialize_Hardware;

   Discrete_Inputs.Default_Debounce_Time := Milliseconds (50);
   --  Be a little more responsive to users toggling the switch

   Put_Line ("Toggle button");
   loop
      Button.Await_Toggle;
      --  Should not return until the button is released. If it returns as
      --  soon as the button is pressed, ie before released, the Button.Active
      --  discriminant is set incorrectly for the circuit involved.

      Toggle_Count := Toggle_Count + 1;
      Put_Line ("Toggled" & Toggle_Count'Img & "   ");
   end loop;
end Demo_Touch_Sensor;
