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

with Ada.Unchecked_Conversion;
with STM32.Device; use STM32.Device;

package body NXT.Motors is

   procedure Configure_Polarity_Control (This : GPIO_Point);
   --  Configures the GPIO port/pin pair as a discrete output to the NXT
   --  Shield. These outputs control the polarity of the power provided to the
   --  motors, thereby controlling their direction, as well as whether they
   --  coast to a stop or come to an immediate halt.

   --------------
   -- Throttle --
   --------------

   function Throttle (This : Basic_Motor) return Power_Level is
   begin
      return This.Power_Plant.Current_Duty_Cycle;
   end Throttle;

   ------------
   -- Engage --
   ------------

   procedure Engage
     (This      : in out Basic_Motor;
      Direction : Directions;
      Power     : Power_Level)
   is
   begin
      case Direction is
         when Forward  =>
            Set (This.Polarity1);
            Clear (This.Polarity2);
         when Backward =>
            Clear (This.Polarity1);
            Set (This.Polarity2);
      end case;
      This.Power_Plant.Set_Duty_Cycle (Power);
   end Engage;

   ---------------
   -- Rotate_To --
   ---------------

   procedure Rotate_To
     (This       : in out Basic_Motor;
      Target     : Motor_Encoder_Counts;
      Power      : Power_Level;
      Stop_After : Boolean := True)
   is
      Current_Count : constant Motor_Encoder_Counts := Encoder_Count (This);
   begin
      if Target > Current_Count then
         Engage (This, Forward, Power);
         loop
            exit when Encoder_Count (This) >= Target;
         end loop;
      elsif Target < Current_Count then
         Engage (This, Backward, Power);
         loop
            exit when Encoder_Count (This) <= Target;
         end loop;
      end if;
      if Stop_After then
         Stop (This);
      end if;
   end Rotate_To;

   ----------
   -- Stop --
   ----------

   procedure Stop (This : in out Basic_Motor) is
   begin
      Set (This.Polarity1);
      Set (This.Polarity2);
      This.Power_Plant.Set_Duty_Cycle (100);  -- full power to lock position
   end Stop;

   -----------
   -- Coast --
   -----------

   procedure Coast (This : in out Basic_Motor) is
   begin
      This.Power_Plant.Set_Duty_Cycle (0);  -- do not lock position
   end Coast;

   ------------------------
   -- Rotation_Direction --
   ------------------------

   function Rotation_Direction (This : Basic_Motor) return Directions is
   begin
      case Current_Direction (This.Encoder) is
         when Up   => return Forward;
         when Down => return Backward;
      end case;
   end Rotation_Direction;

   -------------------------
   -- Reset_Encoder_Count --
   -------------------------

   procedure Reset_Encoder_Count (This : in out Basic_Motor) is
   begin
      Reset_Count (This.Encoder);
   end Reset_Encoder_Count;

   -------------------
   -- Encoder_Count --
   -------------------

   function Encoder_Count (This : Basic_Motor) return Motor_Encoder_Counts is
      function As_Motor_Encoder_Counts is new Ada.Unchecked_Conversion
        (Source => UInt32, Target => Motor_Encoder_Counts);
   begin
      return As_Motor_Encoder_Counts (Current_Count (This.Encoder));
   end Encoder_Count;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (This                 : in out Basic_Motor;
      --  motor encoder I/O
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
      --  discrete outputs to NXT Shield that control motor direction
      Polarity1            : GPIO_Point;
      Polarity2            : GPIO_Point)
   is
   begin
      --  First set up the PWM for the motors' power control.
      --  We do this configuration here because we are not sharing the timer
      --  across other PWM generation clients
      Configure_PWM_Timer (PWM_Timer, PWM_Output_Frequency);

      This.Power_Plant.Attach_PWM_Channel
        (PWM_Timer,
         PWM_Output_Channel,
         PWM_Output,
         PWM_AF);

      This.Power_Plant.Enable_Output;

      This.PWM_Channel := PWM_Output_Channel;

      --  Now set up the motor encoders

      Initialize_Encoder
        (This.Encoder,
         Encoder_Input1,
         Encoder_Input2,
         Encoder_Timer,
         Encoder_AF);

      Reset_Count (This.Encoder);

      This.Polarity1 := Polarity1;
      This.Polarity2 := Polarity2;

      Enable_Clock (Polarity1);
      Enable_Clock (Polarity2);

      Configure_Polarity_Control (Polarity1);
      Configure_Polarity_Control (Polarity2);
   end Initialize;

   --------------------------------
   -- Configure_Polarity_Control --
   --------------------------------

   procedure Configure_Polarity_Control (This : GPIO_Point) is
      Configuration : GPIO_Port_Configuration;
   begin
      Configuration := (Mode        => Mode_Out,
                        Output_Type => Push_Pull,
                        Resistors   => Pull_Down,
                        Speed       => Speed_100MHz);

      This.Configure_IO (Configuration);

      This.Lock;
   end Configure_Polarity_Control;

end NXT.Motors;
