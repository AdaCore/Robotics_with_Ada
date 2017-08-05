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

   ----------------------
   -- Current_Throttle --
   ----------------------

   function Current_Throttle (This : Basic_Motor) return Percentage is
   begin
      return This.Power.Current_Duty_Cycle;
   end Current_Throttle;

   ------------
   -- Engage --
   ------------

   procedure Engage
     (This        : in out Basic_Motor;
      Direction   : Directions;
      Power_Level : Percentage)
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
      This.Power.Set_Duty_Cycle (Power_Level);
   end Engage;

   ------------
   -- Engage --
   ------------

   procedure Engage
     (This        : in out Basic_Motor;
      Direction   : Directions;
      Power_Level : Percentage;
      Counts      : Positive;
      Brake       : Boolean := True)
   is
      Target : Motor_Encoder_Counts;
   begin
      case Direction is
         when Forward =>
            Target := Encoder_Count (This) + Motor_Encoder_Counts (Counts);
            Engage (This, Forward, Power_Level);
            loop
               exit when Encoder_Count (This) >= Target;
            end loop;
         when Backward =>
            Target := Encoder_Count (This) - Motor_Encoder_Counts (Counts);
            Engage (This, Backward, Power_Level);
            loop
               exit when Encoder_Count (This) <= Target;
            end loop;
      end case;
      if Brake then
         Stop (This);
      end if;
   end Engage;

   ----------
   -- Stop --
   ----------

   procedure Stop (This : in out Basic_Motor) is
   begin
      Set (This.Polarity1);
      Set (This.Polarity2);
      This.Power.Set_Duty_Cycle (100);  -- full power
   end Stop;

   -----------
   -- Coast --
   -----------

   procedure Coast (This : in out Basic_Motor) is
   begin
      This.Power.Set_Duty_Cycle (0);  -- cut power
   end Coast;

   -----------------------
   -- Current_Direction --
   -----------------------

   function Current_Direction (This : Basic_Motor) return Directions is
   begin
      case Current_Direction (This.Encoder) is
         when Up   => return Forward;
         when Down => return Backward;
      end case;
   end Current_Direction;

   --------------------
   -- Reset_Position --
   --------------------

   procedure Reset_Encoder_Count (This : in out Basic_Motor) is
   begin
      Reset_Count (This.Encoder);
   end Reset_Encoder_Count;

   ----------------------
   -- Current_Position --
   ----------------------

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

      This.Power.Attach_PWM_Channel
        (PWM_Timer,
         PWM_Output_Channel,
         PWM_Output,
         PWM_AF);

      This.Power.Enable_Output;

      This.Channel := PWM_Output_Channel;

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
      Configuration.Mode := Mode_Out;
      Configuration.Output_Type := Push_Pull;
      Configuration.Resistors := Pull_Down;
      Configuration.Speed := Speed_100MHz;

      This.Configure_IO (Configuration);

      This.Lock;
   end Configure_Polarity_Control;

end NXT.Motors;
