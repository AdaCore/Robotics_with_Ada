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

package body HiTechnic.Gyroscopic_Sensor is

   ---------------
   -- Configure --
   ---------------

   procedure Configure
     (This          : in out Gyro_Sensor;
      Converter     : not null access Analog_To_Digital_Converter;
      Input_Channel : Analog_Input_Channel;
      Input_Pin     : GPIO_Point)
   is
   begin
      This.Assign_ADC (Converter, Input_Channel, Input_Pin);
      Initialize (NXT_Analog_Sensor_Polled (This));
   end Configure;

   ---------------
   -- Calibrate --
   ---------------

   procedure Calibrate
     (This              : in out Gyro_Sensor;
      Sampling_Interval : Time_Span)
   is
      Total        : Integer := 0;
      Sample_Count : Integer := 0;
      Deadline     : constant Time := Clock + Sampling_Interval;
      Reading      : Integer;
      Unused       : Boolean;
   begin
      while Clock <= Deadline loop
         This.Get_Raw_Reading (Reading, Successful => Unused);
         Total := Total + Reading;
         Sample_Count := Sample_Count + 1;
      end loop;
      This.Offset := Total / Sample_Count;
   end Calibrate;

   -----------------------
   -- Reset_Calibration --
   -----------------------

   procedure Reset_Calibration (This : in out Gyro_Sensor) is
   begin
      This.Offset := 0;
   end Reset_Calibration;

   -------------
   -- Reading --
   -------------

   function Reading (This : in out Gyro_Sensor) return Integer is
      Sample : Integer;
      Unused : Boolean;
   begin
      This.Get_Raw_Reading (Sample, Successful => Unused);
      return Sample - This.Offset;
   end Reading;

end HiTechnic.Gyroscopic_Sensor;
