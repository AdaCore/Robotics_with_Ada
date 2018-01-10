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

--  This package provides a base class for device drivers for I2C-based Lego
--  sensors. See the Lego NXT Hardware Developer's Kit document for details.

with HAL;        use HAL;
with STM32.I2C;  use STM32.I2C;
with STM32.GPIO; use STM32.GPIO;
use STM32; -- for GPIO_Alternate_Function

package NXT.Digital is

   type I2C_Device_Address is new UInt8 range 1 .. 127;
   --  These are the device hardware addresses, without the shift into the
   --  upper 7 bits required for the I2C protocol. Shifting is done for
   --  clients by this software.

   type NXT_Digital_Sensor (Device_Hardware_Address : I2C_Device_Address) is
      abstract tagged limited private;

   procedure Configure
     (This        : in out NXT_Digital_Sensor;
      Port        : access I2C_Port;
      SCL         : in out GPIO_Point;  -- clock
      SDA         : in out GPIO_Point;  -- data
      AF_Code     :        GPIO_Alternate_Function;
      Clock_Speed :        UInt32)
   with Post => Configured (This) and
                Enabled (This);
   --  Configures both SDA and SCL pins with open-drain and internal pull-up
   --  resistors enabled. Note that both SDA and SCL require external pull-up
   --  resistors, 10K works well. The one AF_Code is applied to both pins.

   function Configured (This : NXT_Digital_Sensor) return Boolean;

   procedure Enable (This : in out NXT_Digital_Sensor) with
     Pre  => Configured (This),
     Post => Enabled (This);

   procedure Disable (This : in out NXT_Digital_Sensor) with
     Pre  => Configured (This),
     Post => not Enabled (This);

   function Enabled (This : NXT_Digital_Sensor) return Boolean;

   subtype Device_Attribute is String (1 .. 8);

   procedure Get_Sensor_Type
     (This    : in out NXT_Digital_Sensor;
      Value   :    out Device_Attribute;
      Success :    out Boolean)
   with Pre => Configured (This) and
               Enabled (This);
   --  Returns the sensor type, e.g., "Sonar" or "IRRecv"

   procedure Get_Product_Id
     (This    : in out NXT_Digital_Sensor;
      Value   :    out Device_Attribute;
      Success :    out Boolean)
   with Pre => Configured (This) and
               Enabled (This);
   --  Returns the sensor product identifier, e.g., "LEGO" or "HiTechnc"

   procedure Get_Version
     (This    : in out NXT_Digital_Sensor;
      Value   :    out Device_Attribute;
      Success :    out Boolean)
   with Pre => Configured (This) and
               Enabled (This);
   --  Returns the sensor version number, e.g., "V1.1"

   type Sequence is array (Positive range <>) of UInt8;

   type Register_Address is new UInt8;

   procedure Write_Register
     (This     : in out NXT_Digital_Sensor;
      Register :        Register_Address;
      Value    :        UInt8;
      Success  :    out Boolean);

   procedure Write_Register
     (This     : in out NXT_Digital_Sensor;
      Register :        Register_Address;
      Value    :        Sequence;
      Success  :    out Boolean);

   procedure Read_Register
     (This     : in out NXT_Digital_Sensor;
      Register :        Register_Address;
      Value    :    out UInt8;
      Success  :    out Boolean);

   procedure Read_Register
     (This     : in out NXT_Digital_Sensor;
      Register :        Register_Address;
      Value    :    out Sequence;
      Success  :    out Boolean);

   procedure Read_Register
     (This     : in out NXT_Digital_Sensor;
      Register :        Register_Address;
      Value    :    out Device_Attribute;
      Success  :    out Boolean);
   --  A convenience routine to get a string from the specified register

private

   type NXT_Digital_Sensor (Device_Hardware_Address : I2C_Device_Address) is
   abstract tagged limited record
      Port                    : access I2C_Port;
      Device_Address          : UInt8;
      Configuration_Completed : Boolean := False;
   end record;

   --  See the LEGO MINDSTORMS NXT Hardware Development Kit, figure 4, in the
   --  Device Memory Arrangement section, for these register addresses.
   Version_Register     : constant Register_Address := 16#00#;
   Product_Id_Register  : constant Register_Address := 16#08#;
   Sensor_Type_Register : constant Register_Address := 16#10#;

end NXT.Digital;
