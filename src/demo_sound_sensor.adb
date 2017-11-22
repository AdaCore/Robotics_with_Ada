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

--  This program demonstrates driving a LEGO NXT Sound Sensor. We use a PWM
--  modulator component to vary the intensity of an on-board LED, based on the
--  sampled value returned from the sensor. The sensor is periodically sampled
--  at an arbitrary rate, but a rate intended to be responsive.

--  NOTE: this program is set up to run on an STM32F4 Discovery board. You can
--  change that, but note that not all boards can drive an LED via PWM (e.g.,
--  the F429 Disco board).

--  This program requires one LED. The code is currently written to use one
--  of the four LEDs on the F4 Discovery board, but a different board could
--  certainly be used instead, as long as the LED can be driven by a timer
--  channel (not all can do so).

--  NOTE: you must have an external pull-up resistor tied to +5V on the analog
--  input pin. A 10K resistor works well. You will get odd readings otherwise.

with Last_Chance_Handler;  pragma Unreferenced (Last_Chance_Handler);

with STM32.Board;  use STM32.Board;
with STM32.Device; use STM32.Device;

with HAL;          use HAL;
with STM32.PWM;    use STM32.PWM;
with STM32.ADC;
with STM32.Timers;
use STM32;

with NXT.Analog_Sensors;     use NXT.Analog_Sensors;
with NXT.Sound_Sensors;      use NXT.Sound_Sensors;
with Sound_Demo_Peripherals; use Sound_Demo_Peripherals;
with Ada.Real_Time;          use Ada.Real_Time;

procedure Demo_Sound_Sensor is

   procedure Set_Up_ADC_General_Settings;
   --  Does ADC general setup for all ADC units.

   procedure Set_Up_PWM;
   --  Enables the clock for the LEDs and configures the PWM modulator
   --  connected via the timer to those LEDs. (Only one LED is actually
   --  driven.)

   procedure Panic with No_Return;
   --  Flash the LEDs to indicate disaster, forever.

   ---------------------------------
   -- Set_Up_ADC_General_Settings --
   ---------------------------------

   procedure Set_Up_ADC_General_Settings is
      use STM32.ADC;
   begin
      Reset_All_ADC_Units;
      Configure_Common_Properties
        (Mode           => Independent,
         Prescalar      => PCLK2_Div_2,
         DMA_Mode       => Disabled,  -- this is multi-dma mode
         Sampling_Delay => Sampling_Delay_5_Cycles);
   end Set_Up_ADC_General_Settings;

   ----------------
   -- Set_Up_PWM --
   ----------------

   procedure Set_Up_PWM is
      use STM32.Timers;

      Selected_Timer : Timer renames Timer_4;
      --  NOT arbitrary! We drive the on-board LEDs that are tied to the
      --  channels of Timer_4 on some boards. Not all boards have this
      --  association. If you use a difference board, select a GPIO point
      --  connected to your selected timer and drive that instead.

      Timer_AF : constant STM32.GPIO_Alternate_Function := GPIO_AF_TIM4_2;
      --  Note that this value MUST match the corresponding timer selected!

      Requested_Frequency : constant Hertz := 30_000;  -- arbitrary

      Output_Channel : constant Timer_Channel := Channel_2; -- arbitrary
      --  The LED driven by this example is determined by the channel selected.
      --  That is so because each channel of Timer_4 is connected to a specific
      --  LED in the alternate function configuration on this board. We will
      --  initialize all of the LEDs to be in the AF mode. The
      --  particular channel selected is completely arbitrary, as long as the
      --  selected GPIO port/pin for the LED matches the selected channel.
      --
      --  Channel_1 is connected to the green LED.
      --  Channel_2 is connected to the orange LED.
      --  Channel_3 is connected to the red LED.
      --  Channel_4 is connected to the blue LED.
      LED_For : constant array (Timer_Channel) of User_LED :=
                  (Channel_1 => Green_LED,
                   Channel_2 => Orange_LED,
                   Channel_3 => Red_LED,
                   Channel_4 => Blue_LED);

   begin
      Configure_PWM_Timer (Selected_Timer'Access, Requested_Frequency);
      LED_Power_Control.Attach_PWM_Channel
        (Selected_Timer'Access,
         Output_Channel,
         LED_For (Output_Channel),
         Timer_AF);
      LED_Power_Control.Enable_Output;
   end Set_Up_PWM;

   -----------
   -- Panic --
   -----------

   procedure Panic is
   begin
      --  "When in danger, or in doubt, run in circles, scream and shout."
      loop
         All_LEDs_Off;
         delay until Clock + Milliseconds (250); -- arbitrary
         All_LEDs_On;
         delay until Clock + Milliseconds (250); -- arbitrary
      end loop;
   end Panic;

   --  The values controlling the periodic execution. The period is arbitrary
   --  but should be quick enough for the LED brightness to be responsive to
   --  incoming sounds.

   Next_Release : Time := Clock;
   Period       : constant Time_Span := Milliseconds (100);  -- arbitrary

   Power_Level : PWM.Percentage;
   Reading     : Intensity;
   Status      : Reading_Status;

begin
   Set_Up_PWM;
   Set_Up_ADC_General_Settings;
   Sensor.Initialize;
   --  See the note in the header about the required resistor.

   --  Manual calibration. These values were determined empirically and worked
   --  well with the spoken voice coming out of a desktop speaker, but of
   --  course you may need to change them for your environment.
   Sensor.Set_Calibrated_Minimum (0);
   Sensor.Set_Calibrated_Maximum (900);

   loop
      Sensor.Get_Scaled_Reading (Reading, Status);
      if Status /= Valid_Reading then
         Panic;
      else
         Power_Level := PWM.Percentage (Reading);
      end if;
      LED_Power_Control.Set_Duty_Cycle (Power_Level);

      Next_Release := Next_Release + Period;
      delay until Next_Release;
   end loop;
end Demo_Sound_Sensor;
