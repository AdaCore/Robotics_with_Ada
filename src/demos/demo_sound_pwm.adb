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
--  the F429I Disco board cannot).

--  This program requires one LED. The code is currently written to use one
--  of the four LEDs on the F4 Discovery board, but a different board could
--  certainly be used instead, as long as the LED can be driven by a timer
--  channel.

--  NOTE: you must have an external pull-up resistor tied to +5V on the analog
--  input pin. A 10K resistor works well. You will get odd readings otherwise.

--  To use this program, connect an NXT sound sensor to a supported board. The
--  project file is configured for an STM32F4 Discovery board. Place the sound
--  sensor next to a sound source, such as a loud-speaker, or just speak into
--  it. In either case place the sensor reasonably close to the sound source,
--  e.g., no more than twice the length of the sensor itself. As the input
--  sound volume increases and decreases, the orange LED will brighten and dim,
--  respectively (assuming a board with an orange LED, such as the F4 Disco
--  board).

--  The wiring connections for connecting the senor to the board are as
--  follows, for the standard Lego NXT connectors:
--
--  Pin 1 (white wire)  - Analog output from the device, required
--  Pin 2 (black wire)  - Ground (either one, or both)
--  Pin 3 (red wire)    - Ground (either one, or both)
--  Pin 4 (green wire)  - Vcc (+5V), required to power the sensor
--  Pin 5 (yellow wire) - Used in some sensors ("Digital0" in literature)
--  Pin 6 (blue wire)   - Used in some sensors ("Digital1" in literature)

with Last_Chance_Handler;  pragma Unreferenced (Last_Chance_Handler);

with Panic;

with STM32.Board;  use STM32.Board;
with STM32.Device; use STM32.Device;

with HAL;       use HAL;
with STM32.PWM; use STM32.PWM;
with STM32.ADC;
with STM32.Timers;

with NXT.Analog;            use NXT.Analog;
with Analog_Sensor_Factory; use Analog_Sensor_Factory;

with Ada.Real_Time; use Ada.Real_Time;

procedure Demo_Sound_PWM is

   Sensor : NXT_Analog_Sensor'Class := Analog_Sensor_Factory.New_Sensor (Kind => Sound);
   --  This demo always uses a sound sensor. Don't change that.

   LED_Power_Control : PWM_Modulator;
   --  This is the power generator driving the LED. The LED brightness will
   --  vary with the sound intensity detected by the sound sensor.

   Next_Release : Time := Clock;
   Period       : constant Time_Span := Milliseconds (100);  -- arbitrary
   Power_Level  : STM32.PWM.Percentage;
   Reading      : Intensity;
   Successful   : Boolean;

   procedure Set_Up_ADC_General_Settings;
   --  Does ADC general setup for all ADC units.

   procedure Set_Up_PWM;
   --  Enables the clock for the LEDs and configures the PWM modulator
   --  connected via the timer to those LEDs. (Only one LED is actually
   --  driven.)

   ---------------------------------
   -- Set_Up_ADC_General_Settings --
   ---------------------------------

   procedure Set_Up_ADC_General_Settings is
      use STM32.ADC;
   begin
      STM32.Device.Reset_All_ADC_Units;
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
      --  channels of Timer_4 on the F4 Discovery boards. Not all boards have
      --  this association. If you use a difference board, select a GPIO point
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

begin
   Set_Up_PWM;
   Set_Up_ADC_General_Settings;
   Sensor.Initialize;

   --  Manual calibration. These values were determined empirically and worked
   --  well with the spoken voice coming out of a desktop speaker, but of
   --  course you may need to change them for your environment.
   Sensor.Set_Calibration (Least => 0, Greatest => 900);

   loop
      Sensor.Get_Intensity (Reading, Successful);
      if not Successful then
         Panic;
      else
         Power_Level := STM32.PWM.Percentage (Reading);
      end if;
      LED_Power_Control.Set_Duty_Cycle (Power_Level);

      Next_Release := Next_Release + Period;
      delay until Next_Release;
   end loop;
end Demo_Sound_PWM;
