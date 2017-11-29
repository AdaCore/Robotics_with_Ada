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
--  NXT Lego analog sensors.

--  Note that you must have an external pull-up resistor tied to +5V on the
--  analog input pin. A 10K resistor works well.

--  The wiring connections are as follows, for the standard Lego NXT
--  connectors:
--
--  Pin 1 (white wire)  - Analog output from the device, required
--  Pin 2 (black wire)  - Ground (either one, or both)
--  Pin 3 (red wire)    - Ground (either one, or both)
--  Pin 4 (green wire)  - Vcc (+5V), required to power the sensor
--  Pin 5 (yellow wire) - Used in some sensors ("Digital0" in literature)
--  Pin 6 (blue wire)   - Used in some sensors ("Digital1" in literature)

with LCD_Std_Out;  use LCD_Std_Out;

with Last_Chance_Handler;  pragma Unreferenced (Last_Chance_Handler);

with STM32.Board;  use STM32.Board;
with STM32.Device; use STM32.Device;
with STM32.ADC;    use STM32.ADC;
with STM32.GPIO;   use STM32.GPIO;

with HiTechnic.Gyroscopic_Sensor; use HiTechnic.Gyroscopic_Sensor;

with Ada.Real_Time;  use Ada.Real_Time;

procedure Demo_HiTechnic_Gyro is

   Gyro : Gyro_Sensor;

   Next_Release : Time := Clock;
   Period       : constant Time_Span := Milliseconds (100);  -- arbitrary

   Gyro_ADC_Unit      : Analog_To_Digital_Converter renames ADC_1;
   Gyro_Input_Channel : constant Analog_Input_Channel := 5;
   Gyro_Input_Pin     : GPIO_Point renames PA5;  -- must match the channel!


   procedure Set_Up_ADC_General_Settings;
   --  Does ADC general setup for all ADC units.

   ---------------------------------
   -- Set_Up_ADC_General_Settings --
   ---------------------------------

   procedure Set_Up_ADC_General_Settings is
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

   Gyro.Configure (Gyro_ADC_Unit'Access, Gyro_Input_Channel, Gyro_Input_Pin);

   Gyro.Calibrate (Sampling_Interval => Seconds (2));

   LCD_Std_Out.Clear_Screen;

   loop
      Green_LED.Toggle;  -- visually indicate execution rate

      Put_Line (Gyro.Reading'Img & "      ");

      Next_Release := Next_Release + Period;
      delay until Next_Release;
   end loop;
end Demo_HiTechnic_Gyro;
