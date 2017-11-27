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

with STM32.Device; use STM32.Device;
with STM32.ADC;    use STM32.ADC;
with STM32.GPIO;   use STM32.GPIO;
with STM32.DMA;    use STM32.DMA;

with NXT.Analog.DMA.Light.Constructors; use NXT.Analog.DMA.Light.Constructors;
with NXT.Analog.DMA.Sound.Constructors; use NXT.Analog.DMA.Sound.Constructors;

use  NXT.Analog.DMA.Light;
use  NXT.Analog.DMA.Sound;

package body Analog_Sensor_Factory is

   Selected_ADC_Unit      : Analog_To_Digital_Converter renames ADC_1;
   Selected_Input_Channel : constant Analog_Input_Channel := 5;
   Matching_Input_Pin     : GPIO_Point renames PA5;  -- must match the channel!

   Required_DMA_Unit      : DMA_Controller renames DMA_2;
   --  On the STM32F4 devices, only DMA_2 can attach to an ADC
   Matching_Stream        : constant DMA_Stream_Selector := Stream_0;
   --  maps to ADC_1 on DMA_2 (Stream_4 is the only alternative)

   Digital_Line_0         : constant GPIO_Point := PC11;   -- arbitrary
   Digital_Line_1         : constant GPIO_Point := PC12;   -- arbitrary

   ----------------
   -- New_Sensor --
   ----------------

   function New_Sensor (Kind : Known_Analog_Sensors) return NXT_Analog_Sensor'Class is
   begin
      case Kind is
         when Light =>
            return Result : NXT_Analog_Sensor'Class := New_Light_Sensor
              (Converter      => Selected_ADC_Unit'Access,
               Input_Channel  => Selected_Input_Channel,
               Input_Pin      => Matching_Input_Pin,
               Controller     => Required_DMA_Unit'Access,
               Stream         => Matching_Stream,
               Floodlight_Pin => Digital_Line_0);
         when Sound =>
            return Result : NXT_Analog_Sensor'Class := New_Sound_Sensor
              (Converter     => Selected_ADC_Unit'Access,
               Input_Channel => Selected_Input_Channel,
               Input_Pin     => Matching_Input_Pin,
               Controller    => Required_DMA_Unit'Access,
               Stream        => Matching_Stream,
               Mode_Pin_0    => Digital_Line_0,
               Mode_Pin_1    => Digital_Line_1);
      end case;
   end New_Sensor;

end Analog_Sensor_Factory;
