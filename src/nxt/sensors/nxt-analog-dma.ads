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

--  This package provides an abstract subclass for the NXT analog sensors.
--  This subclass uses DMA to acquire the raw sensor readings.

--  Note that you must have an external pull-up resistor tied to +5V on the
--  analog input pin. A 10K resistor works well.

--  Note that, on the STM32F4xxx series, only DMA2 can attach to an ADC, per
--  Table 43 of the RM for that series.

with STM32.DMA; use STM32.DMA;

package NXT.Analog.DMA is

   type NXT_Analog_Sensor_DMA is abstract new NXT_Analog_Sensor with private;

   procedure Assign_DMA
     (This       : in out NXT_Analog_Sensor_DMA;
      Controller : access DMA_Controller;
      Stream     : DMA_Stream_Selector);

   overriding
   procedure Initialize (This : in out NXT_Analog_Sensor_DMA) with
     Post => Enabled (This);

   overriding
   procedure Get_Raw_Reading
     (This       : in out NXT_Analog_Sensor_DMA;
      Reading    : out Natural;
      Successful : out Boolean);

private

   type NXT_Analog_Sensor_DMA is new NXT_Analog_Sensor with record
      Controller : access DMA_Controller;
      Stream     : DMA_Stream_Selector;
      Raw_Value  : UInt16 := 0 with Atomic;
   end record;

end NXT.Analog.DMA;
