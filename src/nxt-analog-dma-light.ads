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

--  This package provides a concrete subclass interface to the NXT light
--  sensor. This subclass uses DMA to acquire the raw sensor readings.

--  Note that you must have an external pull-up resistor tied to +5V on the
--  analog input pin. A 10K resistor works well.

--  In addition to the sensor-specific call to procedure Initialize, clients
--  are responsible for making the following calls to the ADC routines. They
--  are not called internally by Initialize because the functionality and
--  settings are independent of the specific ADC unit used for the sensor,
--  i.e., we don't want to hard-code them globally for one usage. The arguments
--  to procedure Configure_Common_Properties indicated below work with the NXT
--  sound and light sensors
--
--    STM32.ADC.Reset_All_ADC_Units;
--
--    STM32.ADC.Configure_Common_Properties
--      (Mode           => Independent,
--       Prescalar      => PCLK2_Div_2,
--       DMA_Mode       => Disabled,
--       Sampling_Delay => Sampling_Delay_5_Cycles);

package NXT.Analog.DMA.Light is

   type NXT_Light_Sensor is new NXT_Analog_Sensor_DMA with private;

   overriding
   procedure Initialize (This : in out NXT_Light_Sensor) with
     Post => not Floodlight_Enabled (This);

   procedure Enable_Floodlight (This : in out NXT_Light_Sensor) with
     Post => Floodlight_Enabled (This);

   procedure Disable_Floodlight (This : in out NXT_Light_Sensor) with
     Post => not Floodlight_Enabled (This);

   function Floodlight_Enabled (This : NXT_Light_Sensor) return Boolean;

private

   type NXT_Light_Sensor is new NXT_Analog_Sensor_DMA with record
      Floodlight_Pin     : GPIO_Point;
      Floodlight_Enabled : Boolean := False;
   end record;

end NXT.Analog.DMA.Light;
