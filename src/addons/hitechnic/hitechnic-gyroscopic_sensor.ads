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

--  This package provides an interface to the HiTechnic Gyroscopic sensor

with STM32.ADC;     use STM32.ADC;
with STM32.GPIO;    use STM32.GPIO;
with Ada.Real_Time; use Ada.Real_Time;

private with NXT.Analog.Polling;

package HiTechnic.Gyroscopic_Sensor is

   type Gyro_Sensor is tagged limited private;

   procedure Configure
     (This          : in out Gyro_Sensor;
      Converter     : not null access Analog_To_Digital_Converter;
      Input_Channel : Analog_Input_Channel;
      Input_Pin     : GPIO_Point);

   procedure Calibrate
     (This              : in out Gyro_Sensor;
      Sampling_Interval : Time_Span);
   --  Takes samples over the sampling interval and assigns the average to the
   --  offset for this sensor. The offset is the sensor reading when the sensor
   --  is at rest.

   function Reading (This : in out Gyro_Sensor) return Integer;
   --  Returns the current raw reading minus the calibrated offset. If the
   --  sensor is not calibrated, the offset is zero and so the effect is to
   --  get the raw reading.

   procedure Reset_Calibration (This : in out Gyro_Sensor);
   --  The gyro's offset is set to zero (the initial value)

private

   use NXT.Analog.Polling;

   type Gyro_Sensor is new NXT_Analog_Sensor_Polled with record
      Offset : Integer := 0;  --  the value when the sensor is at rest
   end record;

end HiTechnic.Gyroscopic_Sensor;
