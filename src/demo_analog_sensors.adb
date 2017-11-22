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

with Last_Chance_Handler;  pragma Unreferenced (Last_Chance_Handler);

with STM32.Device; use STM32.Device;
with STM32.Board;  use STM32.Board;

with STM32.ADC; use STM32.ADC;
with STM32.GPIO; use STM32.GPIO;
with STM32.DMA; use STM32.DMA;

--  with NXT.Light_Sensors;     use NXT.Light_Sensors;
with NXT.Sound_Sensors; use NXT.Sound_Sensors;

with NXT.Analog_Sensors;                use NXT.Analog_Sensors;
with NXT.Analog_Sensor_Utils;           use NXT.Analog_Sensor_Utils;
with NXT.Analog_Sensor_Calibration_LCD; use NXT.Analog_Sensor_Calibration_LCD;

with Ada.Real_Time;  use Ada.Real_Time;

procedure Demo_Analog_Sensors is

   Selected_ADC_Unit      : Analog_To_Digital_Converter renames ADC_1;
   Selected_Input_Channel : constant Analog_Input_Channel := 5;
   Matching_Input_Pin     : GPIO_Point renames PA5;  -- must match the channel!

   Required_DMA_Unit      : DMA_Controller renames DMA_2;
   --  On the STM32F4 devices, only DMA_2 can attach to an ADC

   Matching_Stream        : constant DMA_Stream_Selector := Stream_0;
   --  maps to ADC_1 on DMA_2 (Stream_4 is the only alternative)

   --  To demonstrate the sound sensor, uncomment this declaration for the
   --  Sensor object:
   Sensor : NXT_Sound_Sensor
     (Converter      => Selected_ADC_Unit'Access,
      Input_Channel  => Selected_Input_Channel,
      Input_Pin      => Matching_Input_Pin'Access,
      Controller     => Required_DMA_Unit'Access,
      Stream         => Matching_Stream,
      Digital_0      => PC11'Access,    -- arbitrary
      Digital_1      => PC12'Access);   -- arbitrary

   --  To demonstrate the light sensor, uncomment this declaration for the
   --  Sensor object:
--     Sensor : NXT_Light_Sensor
--       (Converter      => Selected_ADC_Unit'Access,
--        Input_Channel  => Selected_Input_Channel,
--        Input_Pin      => Matching_Input_Pin'Access,
--        Controller     => Required_DMA_Unit'Access,
--        Stream         => Matching_Stream,
--        Floodlight_Pin => PC11'Access);   -- arbitrary

   Next_Release : Time := Clock;
   Period       : constant Time_Span := Milliseconds (100);  -- arbitrary

   Count       : Natural := 0;
   Reading     : Intensity;
   Raw_Reading : Natural;
   Successful  : Boolean;
   Status      : Reading_Status;
   Progress    : Character;  -- used to indicate progress on the LCD

   procedure Set_Up_ADC_General_Settings;
   --  Does ADC general setup for all ADC units.

   procedure Panic with No_Return;
   --  Flash the LEDs to indicate disaster, forever.

   ---------------------------------
   -- Set_Up_ADC_General_Settings --
   ---------------------------------

   procedure Set_Up_ADC_General_Settings is
   begin
      Reset_All_ADC_Units;
      Configure_Common_Properties
        (Mode           => Independent,
         Prescalar      => PCLK2_Div_2,
         DMA_Mode       => Disabled,  -- this is multi-dma mode
         Sampling_Delay => Sampling_Delay_5_Cycles);
   end Set_Up_ADC_General_Settings;

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

begin
   Initialize_LEDs;
   Set_Up_ADC_General_Settings;
   Sensor.Initialize;
   --  See the note in the header about the required resistor.

   Calibrate_Analog_Sensor
     (Sensor,
      Sampling_Interval => Seconds (2), --  arbitrary
      Successful        => Successful);
   if not Successful then
      Panic;
   end if;

   LCD_Std_Out.Clear_Screen;

   loop
      Green_LED.Toggle;  -- visual feedback of execution and rate

      Sensor.Get_Raw_Reading (Raw_Reading, Successful);
      if not Successful then
         Put_Line ("raw read fail");
         Panic;
      end if;

      Sensor.Get_Scaled_Reading (Reading, Status);
      if Status /= Valid_Reading then
         Put_Line (Image (Status));
         Panic;
      end if;

      Count := (if Count = Integer'Last then 1 else Count + 1);

      Progress := (if Count mod 2 = 0 then ':' else '|');

      Put_Line (Progress &
                As_Varying_Directly (Raw_Reading)'Img &
                Reading'Img & "%      ");

      Next_Release := Next_Release + Period;
      delay until Next_Release;
   end loop;
end Demo_Analog_Sensors;
