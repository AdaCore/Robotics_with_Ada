------------------------------------------------------------------------------
--                                                                          --
--                    Copyright (C) 2017, AdaCore                           --
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
--     3. Neither the name of the copyright holder nor the names of its     --
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

--  This package provides a basic interface for the Lego NXT motors.

with STM32.GPIO;   use STM32.GPIO;
with STM32.Timers; use STM32.Timers;
with STM32;        use STM32;
with STM32.PWM;    use STM32.PWM;

with Quadrature_Encoders; use Quadrature_Encoders;

with HAL; use HAL;

package NXT.Motors is
   pragma Elaborate_Body;

   type Basic_Motor is tagged limited private;

   subtype Power_Level is Integer range 0 .. 100;

   function Throttle (This : Basic_Motor) return Power_Level;

   type Directions is (Forward, Backward);

   function Rotation_Direction (This : Basic_Motor) return Directions;

   type Motor_Encoder_Counts is range -(2 ** 31) .. +(2 ** 31 - 1);

   Encoder_Counts_Per_Revolution : constant := 720;
   --  Thus 1/2 degree resolution

   procedure Engage
     (This      : in out Basic_Motor;
      Direction : Directions;
      Power     : Power_Level)
   with Post => Throttle (This) = Power;

   procedure Rotate_To
     (This       : in out Basic_Motor;
      Target     : Motor_Encoder_Counts;
      Power      : Power_Level;
      Stop_After : Boolean := True)
   with Post =>
       (if Stop_After
        then Throttle (This) = 100
        else Throttle (This) = Power);

   procedure Stop (This : in out Basic_Motor) with
     Post => Throttle (This) = 100;
   --  Full stop immediately and actively lock motor position.

   procedure Coast (This : in out Basic_Motor) with
     Post => Throttle (This) = 0;
   --  Gradual stop without locking motor position.

   procedure Reset_Encoder_Count (This : in out Basic_Motor) with
     Post => Encoder_Count (This) = 0;

   function Encoder_Count (This : Basic_Motor) return Motor_Encoder_Counts;

   procedure Initialize
     (This                 : in out Basic_Motor;
      --  motor encoder
      Encoder_Input1       : GPIO_Point;
      Encoder_Input2       : GPIO_Point;
      Encoder_Timer        : not null access Timer;
      Encoder_AF           : GPIO_Alternate_Function;
      --  motor power control
      PWM_Timer            : not null access Timer;
      PWM_Output_Frequency : UInt32; -- in Hertz
      PWM_AF               : GPIO_Alternate_Function;
      PWM_Output           : GPIO_Point;
      PWM_Output_Channel   : Timer_Channel;
      --  discrete outputs to H-Bridge that control direction and stopping
      Polarity1            : GPIO_Point;
      Polarity2            : GPIO_Point)
     with
       Pre  => Has_32bit_Counter (Encoder_Timer.all) and
               Bidirectional (Encoder_Timer.all),
       Post => Encoder_Count (This) = 0 and
               Throttle (This) = 0;

private

   type Basic_Motor is tagged limited record
      Encoder     : Rotary_Encoder;
      Power_Plant : PWM_Modulator;
      PWM_Channel : Timer_Channel;  -- for PWM modulator
      Polarity1   : GPIO_Point;
      Polarity2   : GPIO_Point;
   end record;

end NXT.Motors;
