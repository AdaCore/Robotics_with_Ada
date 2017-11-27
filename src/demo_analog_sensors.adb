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

--  This program demonstrates setup, calibration, and interaction with the
--  NXT Lego analog sensors. It can handle any kind of analog sensor. All it
--  does is iteratively display the currently received sensed values.

--  Note that you must have an external pull-up resistor tied to +5V on the
--  analog input pin. A 10K resistor works well.

--  The main procedure's sequence of statements sets up the hardware and
--  then iterates forever, sampling the sensor and displaying values on
--  the LCD screen.

with LCD_Std_Out;  use LCD_Std_Out;

with Panic;

with Last_Chance_Handler;  pragma Unreferenced (Last_Chance_Handler);

with STM32.Device;
with STM32.Board;  use STM32.Board;
with STM32.ADC;

with NXT.Analog;                        use NXT.Analog;
with Analog_Sensor_Factory;             use Analog_Sensor_Factory;
with NXT.Analog_Sensor_Calibration_LCD; use NXT.Analog_Sensor_Calibration_LCD;

with Ada.Real_Time;  use Ada.Real_Time;

procedure Demo_Analog_Sensors is

   Sensor : NXT_Analog_Sensor'Class := Analog_Sensor_Factory.New_Sensor (Kind => Light);
   --  You are intended to try the out different analog sensors by changing the
   --  kind of sensor produced by the factory function

   Next_Release : Time := Clock;
   Period       : constant Time_Span := Milliseconds (100);  -- arbitrary
   Reading      : Intensity;
   Raw_Reading  : Natural;
   Successful   : Boolean;

   procedure Set_Up_ADC_General_Settings;
   --  Does ADC general setup for all ADC units.

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

begin
   Initialize_LEDs;
   Set_Up_ADC_General_Settings;
   Sensor.Initialize;

   Calibrate_Analog_Sensor
     (Sensor,
      Sampling_Interval => Seconds (2), --  arbitrary
      Successful        => Successful);
   if not Successful then
      Panic;
   end if;

   LCD_Std_Out.Clear_Screen;

   loop
      Green_LED.Toggle;  -- visually indicate execution rate

      Sensor.Get_Raw_Reading (Raw_Reading, Successful);
      if not Successful then
         Put_Line ("raw read fail");
         Panic;
      end if;

      Sensor.Get_Intensity (Reading, Successful);
      if not Successful then
         Put_Line ("read fail");
         Panic;
      end if;

      Put_Line (As_Varying_Directly (Raw_Reading)'Img &
                Reading'Img & "%      ");

      Next_Release := Next_Release + Period;
      delay until Next_Release;
   end loop;
end Demo_Analog_Sensors;
