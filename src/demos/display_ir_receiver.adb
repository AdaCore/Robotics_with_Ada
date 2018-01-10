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

--  The program demonstrates the HiTechNic IR Receiver sensor. It is used
--  with the LEGO® Power Functions IR Remote Control* and displays the values
--  received from that controller. Output appears in the st-util window when
--  in GPS when run via the debugger. (You need not build with debugging
--  enabled to run a program within the debugger.)
--
--  * https://shop.lego.com/en-US/LEGO-Power-Functions-IR-Remote-Control-8885

--  Note that pull-up resistors are required for the two I2C lines. 10K works.

--  When used with that controller these are the only possible values for the
--  data. Other controllers may provide other values.
--
--     Momentary_Pressed_Forward  : constant := 100;
--     Momentary_Pressed_Backward : constant := 156;
--     Momentary_Not_Pressed      : constant := 0;

with Last_Chance_Handler;  pragma Unreferenced (Last_Chance_Handler);

with Panic;

with STM32.Device;  use STM32.Device;
with STM32.Board;   use STM32.Board;
with STM32.GPIO;    use STM32.GPIO;
with STM32.I2C;     use STM32.I2C;
with HAL;           use HAL;

with Ada.Real_Time; use Ada.Real_Time;
with Ada.Text_IO;   use Ada.Text_IO;

with NXT.Digital;            use NXT.Digital;
with HiTechnic.IR_Receivers; use HiTechnic.IR_Receivers;

procedure Display_IR_Receiver is

   Selected_I2C_Port      : constant access I2C_Port := I2C_1'Access;
   Selected_I2C_Port_AF   : constant STM32.GPIO_Alternate_Function := GPIO_AF_I2C1_4;
   Selected_I2C_Clock_Pin : GPIO_Point renames PB8;  -- SCL
   Selected_I2C_Data_Pin  : GPIO_Point renames PB9;  -- SDA

   Lego_NXT_I2C_Frequency : constant := 9600; -- per the Lego HDK

   Next_Release : Time := Clock;
   Period       : constant Time_Span := Milliseconds (250);  -- arbitrary

   IR_Sensor_Address : constant I2C_Device_Address := 1;  --  unshifted!!!

   Sensor : IR_Receiver (IR_Sensor_Address);

   procedure Show_Device_Info;
   --  Calls Put_Line to print the product id, sensor type, and version info.
   --  Expected output:
   --      "HiTechnc"
   --      "IRRecv  "
   --      "ýV1.1   "

   procedure Show_Device_Readings;
   --  Calls Put_Line to print all the readings received from the controller.
   --  The following are using the LEGO® Power Functions IR Remote Control,
   --  with channel 1 selected. Selecting the other channels will make the
   --  output change for those other values indicated below.
   --
   --  Expected output when no controller actuations:
   --      A1: 0, A2: 0, A3: 0, A4: 0, B1: 0, B2: 0, B3: 0, B4: 0
   --  Expected output when both controller momentary switches are forward:
   --      A1: 100, A2: 0, A3: 0, A4: 0, B1: 100, B2: 0, B3: 0, B4: 0
   --  Expected output when both controller momentary switches are forward:
   --      A1: 156, A2: 0, A3: 0, A4: 0, B1: 156, B2: 0, B3: 0, B4: 0

   ----------------------
   -- Show_Device_Info --
   ----------------------

   procedure Show_Device_Info is
      Successful : Boolean;
      Attribute  : Device_Attribute;
   begin
      Sensor.Get_Product_Id (Attribute, Successful);
      if Successful then
         Put_Line (Attribute);
      else
         Panic;
      end if;

      Sensor.Get_Sensor_Type (Attribute, Successful);
      if Successful then
         Put_Line (Attribute);
      else
         Panic;
      end if;

      Sensor.Get_Version (Attribute, Successful);
      if Successful then
         Put_Line (Attribute);
      else
         Panic;
      end if;
   end Show_Device_Info;

   --------------------------
   -- Show_Device_Readings --
   --------------------------

   procedure Show_Device_Readings is
      Switches      : Raw_Sensor_Data;
      IO_Successful : Boolean;
   begin
      Sensor.Get_Raw_Data (Switches, IO_Successful);
      if IO_Successful then
         STM32.Board.Turn_Off (Orange_LED);

         Put_Line ("A1:" & Switches.A (1)'Img & ", " &
                   "A2:" & Switches.A (2)'Img & ", " &
                   "A3:" & Switches.A (3)'Img & ", " &
                   "A4:" & Switches.A (4)'Img & ", " &
                   "B1:" & Switches.B (1)'Img & ", " &
                   "B2:" & Switches.B (2)'Img & ", " &
                   "B3:" & Switches.B (3)'Img & ", " &
                   "B4:" & Switches.B (4)'Img);
      else
         STM32.Board.Turn_On (Orange_LED);
      end if;
   end Show_Device_Readings;

begin
   Initialize_LEDs;

   Sensor.Configure
     (Port        => Selected_I2C_Port,
      SCL         => Selected_I2C_Clock_Pin,
      SDA         => Selected_I2C_Data_Pin,
      AF_Code     => Selected_I2C_Port_AF,
      Clock_Speed => Lego_NXT_I2C_Frequency);

   Show_Device_Info;

   loop
      STM32.Board.Toggle (Green_LED);  --  to indicate we are running

      Show_Device_Readings;

      Next_Release := Next_Release + Period;
      delay until Next_Release;
   end loop;
end Display_IR_Receiver;
