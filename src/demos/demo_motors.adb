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

--  This program demonstrates driving a LEGO NXT motor using an STM32F4
--  Discovery board and an NXT_Shield motor interface baord. The progam
--  iteratively increases power by 25% and displays the relative speed using
--  the four LEDs. Motor #1 is used, arbitrarily chosen from the two made
--  available by the NXT_Shield board. The user presses (and releases) an
--  attached NXT touch sensor button to control when the power changes are
--  applied.

with Last_Chance_Handler;  pragma Unreferenced (Last_Chance_Handler);

with STM32.Device;      use STM32.Device;
with STM32.Board;       use STM32.Board;

with NXT.Touch_Sensors; use NXT.Touch_Sensors;
with NXT.Motors;        use NXT.Motors;
with NXT_Shield;        use NXT_Shield;
with Initialize_NXT_Shield;
with Discrete_Inputs;

with Ada.Real_Time;     use Ada.Real_Time;

procedure Demo_Motors is

   NXT_Button : Touch_Sensor (Pin => PC0'Access, Active => Discrete_Inputs.Low);
   --  The logic level choice is directly dependent upon the electronic circuit
   --  connecting the sensor to the MCU. The GPIO pin choice is arbitrary.

   Throttle_Setting : NXT.Motors.Power_Level := 0;
   --  Power setting for controlling motor speed

   Driven_Motor : NXT.Motors.Basic_Motor renames NXT_Shield.Motor1;  -- arbitrary

   Encoder_Sampling_Interval : constant Time_Span := Seconds (1);
   --  Sampling interval for computing encoder counts per second. You can
   --  change this but will then need to compute the rate per second.

   --  These subtypes represent categories of rotation rates. The ranges are
   --  dependent on both the battery level and the motor.
   subtype Stopped  is Motor_Encoder_Counts range 0 .. 0;
   subtype Slow     is Motor_Encoder_Counts range Stopped'Last + 1 .. 600;
   subtype Cruising is Motor_Encoder_Counts range Slow'Last + 1 .. 1400;
   subtype Fast     is Motor_Encoder_Counts range Cruising'Last + 1 .. 1600;
   subtype Redline  is Motor_Encoder_Counts range Fast'Last + 1 .. 10_000;

   function Encoder_Delta (This : Basic_Motor;  Sample_Interval : Time_Span)
      return Motor_Encoder_Counts;
   --  Returns the encoder count delta for This motor over the Sample_Interval
   --  time. Delays the caller for the Interval since it waits that amount of
   --  time between taking the two samples used to calculate the delta.

   procedure Panic with No_Return;
   --  Flash the LEDs to indicate disaster, forever.

   procedure All_Stop (This : in out Basic_Motor);
   --  Powers down This motor and waits for rotations to cease by polling the
   --  motor's encoder.

   -----------
   -- Panic --
   -----------

   procedure Panic is
   begin
      loop
         --  When in danger, or in doubt, run in circles, scream and shout.
         All_LEDs_Off;
         delay until Clock + Milliseconds (250); -- arbitrary
         All_LEDs_On;
         delay until Clock + Milliseconds (250); -- arbitrary
      end loop;
   end Panic;

   -------------------
   -- Encoder_Delta --
   -------------------

   function Encoder_Delta (This : Basic_Motor; Sample_Interval : Time_Span)
      return Motor_Encoder_Counts
   is
      Start_Sample, End_Sample : Motor_Encoder_Counts;
   begin
      Start_Sample := This.Encoder_Count;
      delay until Clock + Sample_Interval;
      End_Sample := This.Encoder_Count;
      return abs (End_Sample - Start_Sample);  -- they can rotate backwards...
   end Encoder_Delta;

   --------------
   -- All_Stop --
   --------------

   procedure All_Stop (This : in out Basic_Motor) is
      Stopping_Time : constant Time_Span := Milliseconds (50);  -- WAG
   begin
      This.Stop;
      loop
         exit when Encoder_Delta (This, Sample_Interval => Stopping_Time) = 0;
      end loop;
   end All_Stop;

begin
   NXT_Button.Initialize_Hardware;
   Initialize_NXT_Shield;
   STM32.Board.Initialize_LEDs;
   STM32.Board.All_LEDs_Off;

   loop
      NXT_Button.Await_Toggle;

      Throttle_Setting := (if Throttle_Setting = 100 then 0 else Throttle_Setting + 25);

      if Throttle_Setting = 0 then
         All_Stop (Motor1);
      else
         Driven_Motor.Engage (Forward, Power => Throttle_Setting);
      end if;

      --  note that the following function call delays for the Sample_Interval
      case Encoder_Delta (Driven_Motor, Sample_Interval => Encoder_Sampling_Interval) is
         when Stopped  => All_LEDs_Off;
         when Slow     => Blue_LED.Set;
         when Cruising => Green_LED.Set;
         when Fast     => Orange_LED.Set;
         when Redline  => Red_LED.Set;
         when others   => Panic;
      end case;
   end loop;
end Demo_Motors;
