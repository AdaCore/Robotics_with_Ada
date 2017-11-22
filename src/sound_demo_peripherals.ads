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

--  This package provides the declarations for the peripherals for the NXT
--  sound sensor demo in the "demo_sound_sensor.adb" file.

with STM32.Device; use STM32.Device;

with HAL;          use HAL;
with STM32.PWM;    use STM32.PWM;
with STM32.DMA;    use STM32.DMA;

with NXT.Sound_Sensors; use NXT.Sound_Sensors;

package Sound_Demo_Peripherals is

   Sensor : NXT_Sound_Sensor
     (Converter      => ADC_1'Access,
      Input_Channel  => 5,
      Input_Pin      => PA5'Access,          -- must match input channel
      Controller     => DMA_2'Access,        -- only DMA2 can map to an ADC
      Stream         => STM32.DMA.Stream_0,  -- maps to ADC1 on DMA2
      Digital_0      => PC11'Access,         -- arbitrary
      Digital_1      => PC12'Access);        -- arbitrary
   --  See the mapping of channels to GPIO pins at the top of the ADC driver
   --  package. Also see your board's User Manual for determining which GPIO
   --  pins are available. For example, on the F429 and F4 Discovery boards,
   --  GPIO pin PA5 maps to channel 5 of both ADC #1 and #2. The ADC unit
   --  selection itself is arbitrary, as long as the input channel, the
   --  GPIO pin, and the DMA stream map to it.

   LED_Power_Control : PWM_Modulator;
   --  This is the power generator driving the LED. The LED brightness will
   --  vary with the sound intensity detected by the sound sensor.

end Sound_Demo_Peripherals;
