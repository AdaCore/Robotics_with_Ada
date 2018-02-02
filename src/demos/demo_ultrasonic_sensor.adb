------------------------------------------------------------------------------
--                                                                          --
--                      Copyright (C) 2018, AdaCore                         --
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

--  The program demonstrates the Lego NXT Ultrasonic Sonar sensor, used with
--  an STM32F4 board instead of the Lego NXT Brick. We also use a third-party
--  bespoke interface board that provides the external circuitry required
--  for driving both the sensor and two motors. This board is the NXT Motor
--  Shield, sold here: http://shop.tkjelectronics.dk

--  Note: This program calls Ada.Text_IO.Put_Line to display the sonar detected
--  values. Therefore, it is intended to be run with a means of seeing the
--  results of those calls, for example within the debugger in the st-util
--  window.

--  Ordinarily, pull-up resistors would be required for both lines, but the
--  Lego Ultrasonic Sensor doesn't have strong internal resistors so it cannot
--  pull the lines low (e.g., to ack I2C messages) if the ordinary scheme is
--  used. Therefore we are using only the internal GPIO pull-up resistors.
--  There are no external pull-up resistors used. Works well.

with Last_Chance_Handler;  pragma Unreferenced (Last_Chance_Handler);

with Panic;

with STM32.Device;  use STM32.Device;
with STM32.Board;   use STM32.Board;
with STM32.GPIO;    use STM32.GPIO;
with HAL;           use HAL;

with Ada.Real_Time; use Ada.Real_Time;
with Ada.Text_IO;   use Ada.Text_IO;

with NXT.Ultrasonic_Sensors; use NXT.Ultrasonic_Sensors;

procedure Demo_Ultrasonic_Sensor is

   Sonar_Clock_Pin : GPIO_Point renames PB13;  -- SCL
   Sonar_Data_Pin  : GPIO_Point renames PB11;  -- SDA
   --  The choice of pins is largely arbitrary because we are bit-banging the
   --  I/O instead of using an ob-board I2C device. Nonetheless, the internal
   --  pull-up resistor values are not the same across all pins. Specifically,
   --  PB10 and PB12 have approximately 11K pull-up resistors, whereas the
   --  other pins have approximately 40K pull-up resistors. See table 47
   --  "I/O Static Characteristics" in the STM32F405xx STM32F407xx Datasheet.

   --  The TKJ NXT Shield circuit connects that board's digital pin 4 to the
   --  I2C sensor's clock line so that we can wiggle it via that digital pin.
   --  We don't use it because we are bit-banging the I/O (because we could
   --  not get the STM32 I2C unit to let the extra clock cycle occur via that
   --  backdoor line). We should connect it via jumper to ground, but didn't
   --  bother.

   Sonar_I2C_Address : constant := 1;  -- the unshifted address

   Sonar : Ultrasonic_Sonar_Sensor (Sonar_I2C_Address);

   Next_Release : Time := Clock;
   Period       : constant Time_Span := Milliseconds (250);  -- arbitrary

   Successful : Boolean;

   Max_Attempts : constant := 3; -- arbitrary

begin
   Initialize_LEDs;

   Sonar.Configure
     (Data_Line       => Sonar_Data_Pin,
      Clock_Line      => Sonar_Clock_Pin,
      Clock_Frequency => 9600,
      Success         => Successful);
   if not Successful then
      Panic;
   end if;

--     Sonar.Enable (Mode => On_Demand);  -- default scan mode is Continuous
   Sonar.Enable (Mode => Continuous);

   for Attempt in 1 .. Max_Attempts loop
      Sonar.Warm_Restart (Successful);
      exit when Successful;
      if Attempt = Max_Attempts then
         Panic;
      end if;
   end loop;

   declare
      Value : String (1 .. 8);
   begin
      Sonar.Get_Product_Id (Value, Successful);
      if Successful then
         Put_Line (Value);
      else
         Panic;
      end if;
   end;

   declare
      Value : String (1 .. 8);
   begin
      Sonar.Get_Version (Value, Successful);
      if Successful then
         Put_Line (Value);
      else
         Panic;
      end if;
   end;

   declare
      Value : String (1 .. 8);
   begin
      Sonar.Get_Sensor_Type (Value, Successful);
      if Successful then
         Put_Line (Value);
      else
         Panic;
      end if;
   end;

   declare
      Value : String (1 .. 8);
   begin
      Sonar.Get_Units (Value, Successful);
      if Successful then
         Put_Line (Value);
      else
         Panic;
      end if;
   end;

   loop
      STM32.Board.Toggle (Green_LED);  --  to indicate we are running

--        declare
--           Readings     : Distances (1 .. 8) := (others => Unavailable);
--           Num_Detected : Natural;
--        begin
--           Sonar.Ping;
--           Sonar.Get_Distances (Readings, Num_Detected);
--           --  We put all the values, instead of the slice 1 .. Num_Detected,
--           --  because the semihosting version of Put seems to print the output
--           --  with a newline anyway, therefore we use one call to Put_Line.
--           Put_Line ('[' & Num_Detected'Img & ']' &
--                     Readings (1)'Img & ", " &
--                     Readings (2)'Img & ", " &
--                     Readings (3)'Img & ", " &
--                     Readings (4)'Img & ", " &
--                     Readings (5)'Img & ", " &
--                     Readings (6)'Img & ", " &
--                     Readings (7)'Img & ", " &
--                     Readings (8)'Img);
--        end;

      declare
         Distance : Centimeters;
      begin
         --  assuming we are in Continuous mode...
         Sonar.Get_Distance (Distance);
         Put_Line (Distance'Img);
      end;

      Next_Release := Next_Release + Period;
      delay until Next_Release;
   end loop;
end Demo_Ultrasonic_Sensor;
