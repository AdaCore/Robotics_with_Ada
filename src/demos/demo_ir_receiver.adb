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

--  The program demonstrates the HiTechNic IR Receiver sensor, used in
--  combination with a LEGO® Power Functions IR Remote Control (see below). It
--  uses that controller to turn on and off the four LEDs on the board (in this
--  case, an F4 Disco) based on the user's manipulation of the controller's two
--  momentary switches.
--
--  * https://shop.lego.com/en-US/LEGO-Power-Functions-IR-Remote-Control-8885

--  Note that pull-up resistors are required for the two I2C lines. 10K works.

--  If the left ("A") momentary switch is pressed forward, the orange LED
--  will come on, otherwise the orange LED will be off.
--
--  If the left ("A") momentary switch is pressed backward, the blue LED
--  will come on, otherwise the blue LED will be off.
--
--  If the right ("B") momentary switch is pressed forward, the green LED
--  will come on, otherwise the green LED will be off.
--
--  If the right ("B") momentary switch is pressed backward, the red LED
--  will come on, otherwise the red LED will be off.

--  When used with that Power Functions controller these are the only possible
--  values for the data. Other controllers may provide other values.
--
--     Momentary_Pressed_Forward  : constant := 100;
--     Momentary_Pressed_Backward : constant := 156;
--     Momentary_Not_Pressed      : constant := 0;

with Last_Chance_Handler;  pragma Unreferenced (Last_Chance_Handler);

with STM32.Device;  use STM32.Device;
with STM32.Board;   use STM32.Board;
with STM32.GPIO;    use STM32.GPIO;
with STM32.I2C;     use STM32.I2C;
with HAL;           use HAL;

with Ada.Real_Time; use Ada.Real_Time;

with NXT.Digital;            use NXT.Digital;
with HiTechnic.IR_Receivers; use HiTechnic.IR_Receivers;

procedure Demo_IR_Receiver is

   --  SCL is the yellow wire from Lego connector
   --  SDA is the blue wire from Lego connector
   --  Black and Red wires are Ground
   --  Green wire is +5V (required)
   --  White wire is unused

   Selected_I2C_Port      : constant access I2C_Port := I2C_1'Access;
   Selected_I2C_Port_AF   : constant STM32.GPIO_Alternate_Function := GPIO_AF_I2C1_4;
   Selected_I2C_Clock_Pin : GPIO_Point renames PB8;  -- SCL
   Selected_I2C_Data_Pin  : GPIO_Point renames PB9;  -- SDA

   Lego_NXT_I2C_Frequency : constant := 9600; -- per the Lego HDK

   Next_Release : Time := Clock;
   Period       : constant Time_Span := Milliseconds (100);  -- arbitrary

   IR_Sensor_Address : constant I2C_Device_Address := 1;  --  unshifted!

   Receiver : IR_Receiver (IR_Sensor_Address);

   Momentary_Pressed_Forward  : constant := 100;
   Momentary_Pressed_Backward : constant := -100;

   procedure Reflect_User_Input (Sensor : in out IR_Receiver);
   --  Sets or clears the four LEDs on the F4 Disco board based on
   --  the controller momentary switch settings, reflecting the user's
   --  manipulations. It uses controller channel 1 exclusively, but that is
   --  an entirely arbitrary choice.

   procedure Conditionally_Set (Pin : in out GPIO_Point; Condition : Boolean);
   --  Conditionally set or clear Pin

   -----------------------
   -- Conditionally_Set --
   -----------------------

   procedure Conditionally_Set (Pin : in out GPIO_Point; Condition : Boolean) is
   begin
      if Condition then
         Pin.Set;
      else
         Pin.Clear;
      end if;
   end Conditionally_Set;

   ------------------------
   -- Reflect_User_Input --
   ------------------------

   procedure Reflect_User_Input (Sensor : in out IR_Receiver) is
      Switches      : Raw_Sensor_Data;
      IO_Successful : Boolean;
      Channel       : constant Channel_Id := 1;  -- depends on transmitter settings
   begin
      Sensor.Get_Raw_Data (Switches, IO_Successful);
      if IO_Successful then
         Conditionally_Set (Orange_LED, Switches.A (Channel) = Momentary_Pressed_Forward);
         Conditionally_Set (Blue_LED,   Switches.A (Channel) = Momentary_Pressed_Backward);
         Conditionally_Set (Green_LED,  Switches.B (Channel) = Momentary_Pressed_Forward);
         Conditionally_Set (Red_LED,    Switches.B (Channel) = Momentary_Pressed_Backward);
      end if;
   end Reflect_User_Input;

begin
   Initialize_LEDs;

   Receiver.Configure
     (Port        => Selected_I2C_Port,
      SCL         => Selected_I2C_Clock_Pin,
      SDA         => Selected_I2C_Data_Pin,
      AF_Code     => Selected_I2C_Port_AF,
      Clock_Speed => Lego_NXT_I2C_Frequency);

   loop
      Reflect_User_Input (Receiver);

      Next_Release := Next_Release + Period;
      delay until Next_Release;
   end loop;
end Demo_IR_Receiver;
