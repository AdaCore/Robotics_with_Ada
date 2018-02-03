------------------------------------------------------------------------------
--                                                                          --
--                   Copyright (C) 2017-2018, AdaCore                       --
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

with STM32.GPIO;       use STM32.GPIO;
with Discrete_Inputs;  use Discrete_Inputs;

package NXT.Touch_Sensors is

   type Touch_Sensor (Pin : access GPIO_Point; Active : Logic_Levels) is
     tagged private;
   --  A sensor consisting of a momentary button switch on the front.

   function Currently_Pressed (This : Touch_Sensor) return Boolean;

   procedure Await_Pressed (This : in out Touch_Sensor);
   --  Wait for the sensor button to be pressed.
   --  Uses polling.

   procedure Await_Released (This : in out Touch_Sensor);
   --  Wait for the sensor button to be released.
   --  Uses polling.

   procedure Await_Toggle (This : in out Touch_Sensor);
   --  Wait for the sensor button to be pressed and then subsequently released,
   --  in that order.
   --  Uses polling.

   procedure Initialize_Hardware (This : in out Touch_Sensor);

private

   type Touch_Sensor (Pin : access GPIO_Point; Active : Logic_Levels) is
     new Discrete_Input (Pin, Active) with null record;

end NXT.Touch_Sensors;
