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

with LCD_Std_Out;   use LCD_Std_Out;
with STM32.Board;
with NXT.Analog_Sensor_Utils; use NXT.Analog_Sensor_Utils;

package body NXT.Analog_Sensor_Calibration_LCD is

   procedure Await_Button_Toggle;
   --  wait for the Blue user button to be pressed and then released, by
   --  polling

   -----------------------------
   -- Calibrate_Analog_Sensor --
   -----------------------------

   procedure Calibrate_Analog_Sensor
     (Sensor            : in out NXT_Analog_Sensor'Class;
      Sampling_Interval : Time_Span;
      Successful        : out Boolean)
   is
      Low_Bound : Integer;
      High_Bound : Integer;
   begin
      Clear_Screen;
      STM32.Board.Configure_User_Button_GPIO; -- for blue user button

      Put_Line ("--Min levels--");
      Put_Line ("Blue button...");
      Await_Button_Toggle;
      Put_Line ("Sampling...");
      Get_Average_Reading (Sensor, Sampling_Interval, Low_Bound, Successful);
      if not Successful then
         Put_Line ("Read failed");
         return;
      end if;

      Put_Line ("--Max levels--");
      Put_Line ("Blue button...");
      Await_Button_Toggle;
      Put_Line ("Sampling...");
      Get_Average_Reading (Sensor, Sampling_Interval, High_Bound, Successful);
      if not Successful then
         Put_Line ("Read failed");
         return;
      end if;

      Low_Bound := As_Varying_Directly (Low_Bound);
      High_Bound := As_Varying_Directly (High_Bound);

      Put_Line ("--Results--");
      Put_Line ("Min:" & Low_Bound'Img);
      Put_Line ("Max:" & High_Bound'Img);

      if Low_Bound = High_Bound then
         Put_Line ("Min = Max!");
         Successful := False;
         return;
      end if;

      if Low_Bound > High_Bound then
         Put_Line ("Min > Max!");
         Successful := False;
         return;
      end if;

      --  The Set_* routines expect inputs to be varying directly
      Set_Calibrated_Minimum (Sensor, Low_Bound);
      Set_Calibrated_Maximum (Sensor, High_Bound);

      Put_Line ("Blue button...");
      Await_Button_Toggle;
   end Calibrate_Analog_Sensor;

   -------------------------
   -- Await_Button_Toggle --
   -------------------------

   procedure Await_Button_Toggle is
      use STM32.Board;
   begin
      while not User_Button_Point.Set loop
         null;
      end loop;
      while User_Button_Point.Set loop
         null;
      end loop;
   end Await_Button_Toggle;


end NXT.Analog_Sensor_Calibration_LCD;
