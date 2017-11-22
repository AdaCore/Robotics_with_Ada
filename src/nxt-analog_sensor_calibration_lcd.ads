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

--  This package provides an interactive analog sensor calibration routine that
--  works with any NXT analog sensor (hence the use of NXT_Analog_Sensor'Class
--  in the formal parameter).

--  NB: this package implementation uses the LCD_Std_Out package and the blue
--  user button. Not all STM32F4xxx Discovery boards have an LCD. Moreover, the
--  STM32F4 Disco board used for some demos has the blue user button disabled
--  by removing a solder bridge.

with NXT.Analog_Sensors; use NXT.Analog_Sensors;
with Ada.Real_Time;      use Ada.Real_Time;

package NXT.Analog_Sensor_Calibration_LCD is

   procedure Calibrate_Analog_Sensor
     (Sensor            : in out NXT_Analog_Sensor'Class;
      Sampling_Interval : Time_Span;
      Successful        : out Boolean)
   with Pre => Enabled (Sensor);
   --  This routine interactively calibrates the specified sensor for both low
   --  and high inputs. The user is prompted for each state, and then presses
   --  the blue User button when ready to take the samples in that state. In
   --  particular, the physical sensor is placed in an environment for taking
   --  minimum (maximum, respectively) input levels and then the button is to
   --  be pushed. The average sensed value is used in each case. The final min
   --  and max values are displayed on the LCD screen. The raw low and high
   --  bounds are displayed as values that vary directly with the sensed
   --  input. The raw values are then set in the sensor.
   --
   --  First, the "minimum" (e.g., darkest) sensed input level is calibrated,
   --  then the "maximum" (e.g., brightest) input levels. For example, a full
   --  successful calibration would appear as follows on the LCD:
   --
   --    Min levels:             (indicates about to sample low input levels)
   --    Blue button...          (indicates waiting for user button press)
   --    Sampling...             (indicates calibration for Sampling_Interval)
   --    Blue button...          (indicates waiting for user button press)
   --    Max levels:             (indicates about to sample high input levels)
   --    Blue button...          (indicates waiting for user button press)
   --    Sampling...             (indicates calibration for Sampling_Interval)
   --    Low: xxxx               (the raw low input calibration value)
   --    High: yyyy              (the raw high input calibration value)
   --    Blue button...          (indicates waiting for user button press)
   --
   --  The last "Button press..." doesn't appear if an error regardng the two
   --  values is detected by the calibration routine, in which case an error
   --  message appears and Successful is set to False.
   --
   --  If any attempt to acquire a raw sensor reading fails, that is
   --  indicated on the LCD, Successful is set to False, and the call
   --  returns immediately.

end NXT.Analog_Sensor_Calibration_LCD;
