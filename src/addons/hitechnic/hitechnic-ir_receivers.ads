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

--  This package provides a device driver for the HiTechNic IR Receiver sensor
--  used with Lego NXT robotics kits:
--
--  http://www.hitechnic.com/cgi-bin/commerce.cgi?preadd=action&key=nir1032

--  When the sensor is driven by the LEGO® Power Functions IR Remote Control*
--  and everything is working as expected, these are the only possible values
--  for the data. Other controllers may provide other values.
--
--     Momentary_Pressed_Forward  : constant := 16#64#;
--     Momentary_Pressed_Backward : constant := 16#80#;
--     Momentary_Not_Pressed      : constant := 0;
--
--  * https://shop.lego.com/en-US/LEGO-Power-Functions-IR-Remote-Control-8885

with NXT.Digital; use NXT.Digital;
with HAL;         use HAL;

package HiTechnic.IR_Receivers is

   type IR_Receiver is new NXT_Digital_Sensor with private;

   type Channel_Id is range 1 .. 4;

   type Raw_Sensor_Values is array (Channel_Id) of UInt8;

   type Raw_Sensor_Data is record
     A : Raw_Sensor_Values;
     B : Raw_Sensor_Values;
   end record;

   procedure Get_Raw_Data
     (This          : in out IR_Receiver;
      Data          :    out Raw_Sensor_Data;
      IO_Successful :    out Boolean)
   with
       Post => (if not IO_Successful then Data = (others => (others => 0)));
   --  Note that when True, IO_Successful does not necessarily imply valid
   --  data, but when False the data are definitely not current.

private

   type IR_Receiver is new NXT_Digital_Sensor with null record;

   Data_Registers : constant Register_Address := 16#42#;

end HiTechnic.IR_Receivers;
