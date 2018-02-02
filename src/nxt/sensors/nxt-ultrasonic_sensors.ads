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

--  This package provides a device driver for the Lego NXT Ultrasonic Sensors.
--  It uses a bit-banged implementation for the low-level I2C I/O.

with BitBanged_I2C.SoftWire; use BitBanged_I2C.SoftWire;
with STM32.GPIO;             use STM32.GPIO;
with HAL;                    use HAL;
with Ada.Real_Time;          use Ada.Real_Time;

package NXT.Ultrasonic_Sensors is

   type I2C_Device_Address is new UInt8 range 1 .. 127;
   --  These are the hardware device addresses, without the shift into the
   --  upper 7 bits. The shifting is done for clients by this software, not
   --  by clients.

   type Ultrasonic_Sonar_Sensor (Hardware_Device_Address : I2C_Device_Address) is
     tagged limited private;

   procedure Configure
     (This            : in out Ultrasonic_Sonar_Sensor;
      Data_Line       :        GPIO_Point;
      Clock_Line      :        GPIO_Point;
      Clock_Frequency :        UInt32;
      Success         :    out Boolean)
   with Post => Configured (This) and
                not Enabled (This);
   --  Note that a pull-up resistor is required for the clock line, a 2K will
   --  do. Do not put a resistor on the data line, the sensor would not be able
   --  to pull the line low in that case.

   type Scan_Modes is (Continuous, On_Demand);
   --  The sensor can scan continuously or can scan only on demand. The latter
   --  would be useful especially when more than one sonar scanner is in use,
   --  to avoid operational conflicts. By default the sensor continuously
   --  scans for objects. The sensor can scan for eight objects at a time in
   --  On_Demand mode but in Continuous mode at most one object is detected.
   --  There is another mode named "Event Capture Mode" but beyond a vague and
   --  brief description there is no documentation for it and nobody knows what
   --  to do with it. We ignore it.

   procedure Enable
     (This : in out Ultrasonic_Sonar_Sensor;
      Mode :        Scan_Modes := Continuous)
   with
     Pre  => Configured (This),
     Post => Enabled (This) and
             Current_Scan_Mode (This) = Mode;

   type Centimeters is range 0 .. 255;

   Nothing_Detected : constant Centimeters := 255;
   --  Everthing is working as expected but nothing is currently detected

   procedure Get_Distance
     (This    : in out Ultrasonic_Sonar_Sensor;
      Reading :    out Centimeters)
   with Pre => Configured (This) and
               Enabled (This)  and
               Current_Scan_Mode (This) = Continuous;
   --  Get the latest single distance reading from the sensor, if any object
   --  is detected within range. Will return 255 if no object is detected.

   procedure Ping (This : in out Ultrasonic_Sonar_Sensor) with
     Pre => Configured (This) and
            Enabled (This) and
            Current_Scan_Mode (This) = On_Demand;

   subtype Distances_Index is Integer range 1 .. 8;

   type Distances is array (Distances_Index range <>) of Centimeters;

   procedure Get_Distances
     (This      : in out Ultrasonic_Sonar_Sensor;
      Readings  : out Distances;
      Actual    : out Natural)
   with Pre => Configured (This) and
               Enabled (This) and
               Current_Scan_Mode (This) = On_Demand;

   procedure Get_Distances
     (This      : in out Ultrasonic_Sonar_Sensor;
      Requested : Distances_Index;
      Offset    : Natural;
      Readings  : out Distances;
      Actual    : out Natural)
   with Pre => Configured (This) and
               Enabled (This) and
               Current_Scan_Mode (This) = On_Demand;
   --  Returns an array of distances, depending on the number of objects
   --  detected within the range of the sensor. In continuous mode, at most one
   --  distance is returned. In ping mode, up to 8 distances are returned, but
   --  not more than Requested. If the distance data is not yet available the
   --  method will wait for it.
   --  Requested: the number of distance readings to return.
   --  Offset: the offset within Readings at which new distance values should
   --  start being placed.
   --  Readings: the object containing the new distances returned.
   --  Actual: the number of objects detected and thus the number of distances
   --  assigned in Readings. Will be zero when no object is detected within
   --  range.

   function Configured (This : Ultrasonic_Sonar_Sensor) return Boolean;

   procedure Disable (This : in out Ultrasonic_Sonar_Sensor) with
     Pre  => Configured (This),
     Post => not Enabled (This);

   function Enabled (This : Ultrasonic_Sonar_Sensor) return Boolean;

   function Current_Scan_Mode (This : Ultrasonic_Sonar_Sensor) return Scan_Modes
     with Pre => Configured (This);

   procedure Warm_Restart
     (This    : in out Ultrasonic_Sonar_Sensor;
      Success :    out Boolean)
   with Pre => Configured (This) and Enabled (This);
   --  Note that success indicates that the IO with the device was successful.
   --  It does not query the device in any way. The assumption is that the
   --  device actually responded internally by resetting.

   procedure Get_Product_Id
     (This    : in out Ultrasonic_Sonar_Sensor;
      Value   :    out String;
      Success :    out Boolean)
   with Pre => Configured (This) and Enabled (This);
   --  On return, Value will hold "LEGO" and a null byte. If Value is too short
   --  to hold all 5 characters only those for which room is available are
   --  returned, the rest are truncated. That is not an error.

   procedure Get_Version
     (This    : in out Ultrasonic_Sonar_Sensor;
      Value   :    out String;
      Success :    out Boolean)
   with Pre => Configured (This) and Enabled (This);

   procedure Get_Sensor_Type
     (This    : in out Ultrasonic_Sonar_Sensor;
      Value   :    out String;
      Success :    out Boolean)
   with Pre => Configured (This) and Enabled (This);

   subtype Units_String is String (1 .. 8);

   procedure Get_Units
     (This    : in out Ultrasonic_Sonar_Sensor;
      Units   :    out Units_String;
      Success :    out Boolean)
   with Pre => Configured (This) and Enabled (This);
   --  Gets a string indicating the type of units in use by the sensor. The
   --  default response is 10E-2m indicating use of centimeters.

   procedure Get_Factory_Calibration_Data
     (This            : in out Ultrasonic_Sonar_Sensor;
      Calibrated_Zero :    out UInt8;
      Scale_Factor    :    out UInt8;
      Scale_Divisor   :    out UInt8;
      Success         :    out Boolean)
   with Pre => Configured (This) and Enabled (This);

   procedure Get_Calibration_Data
     (This            : in out Ultrasonic_Sonar_Sensor;
      Calibrated_Zero :    out UInt8;
      Scale_Factor    :    out UInt8;
      Scale_Divisor   :    out UInt8;
      Success         :    out Boolean)
   with Pre => Configured (This) and Enabled (This);
   --  Gets current calibration data

   procedure Set_Calibration_Data
     (This            : in out Ultrasonic_Sonar_Sensor;
      Calibrated_Zero :        UInt8;
      Scale_Factor    :        UInt8;
      Scale_Divisor   :        UInt8;
      Success         :    out Boolean)
   with Pre => Configured (This) and Enabled (This);
   --  Sets calibration data. NB: This routines does not seem to work, and it
   --  didn't work in the leJOS software either.

   procedure Get_Continuous_Interval
     (This     : in out Ultrasonic_Sonar_Sensor;
      Interval :    out UInt8;
      Success  :    out Boolean)
   with Pre => Configured (This) and Enabled (This);
   --  Gets the scan interval used in continuous mode

   procedure Set_Continuous_Interval
     (This     : in out Ultrasonic_Sonar_Sensor;
      Interval :        UInt8;
      Success  :    out Boolean)
   with Pre => Configured (This) and Enabled (This);
   --  Sets the scan interval to be used in continuous mode

private

   --  The Lego ultrasonic sensor seems to require a minimum delay between
   --  commands, otherwise the commands fail.
   Command_Delay : constant Time_Span := Milliseconds (5);

   --  The delay values corresponding to when the data become available in the
   --  given mode.
   Delay_Data_Ping  : constant Time_Span := Milliseconds (50);
   Delay_Data_Other : constant Time_Span := Milliseconds (30);

   type Ultrasonic_Sonar_Sensor (Hardware_Device_Address : I2C_Device_Address) is
      new I2C_Master with record
         Device_Address      : UInt8;  -- the 7-bit, shifted address
         Enabled             : Boolean := False;
         Configured          : Boolean := False;
         Mode                : Scan_Modes := Continuous;
         Next_Command_Time   : Time := Clock + Command_Delay;
      end record;

   type Register_Address is new UInt8;

   procedure Read_Register
     (This       : in out Ultrasonic_Sonar_Sensor;
      Register   :        Register_Address;
      Data       :    out UInt8;
      Successful :    out Boolean);
   --  Reads a byte from the register at the address in Register

   procedure Read_Register
     (This       : in out Ultrasonic_Sonar_Sensor;
      Register   :        Register_Address;
      Data       :    out Sequence;
      Successful :    out Boolean);
   --  Reads a sequence of bytes from the single register at the address
   --  in Register. Note that this is not always possible, depending on
   --  the register! In those cases, use Read_Sequential_Registers instead.

   procedure Read_Sequential_Registers
     (This           : in out Ultrasonic_Sonar_Sensor;
      First_Register :        Register_Address;
      Data           :    out Sequence;
      Successful     :    out Boolean);
   --  Reads data from First_Register and subsequent registers. A single byte
   --  is read from each register. Each "subsequent" register has an address
   --  one greater than the current register. The number of registers to be
   --  read is determined by Data'Length.

   procedure Read_Register
     (This     : in out Ultrasonic_Sonar_Sensor;
      Register :        Register_Address;
      Value    :    out String;
      Success  :    out Boolean)
   with Pre => Value'Length <= 8;
   --  A convenience routine for getting a Sequence from a single register and
   --  converting to type String, since some values returned from the registers
   --  are in fact sequences of characters (for example, the product id).

   procedure Write_Register
     (This       : in out Ultrasonic_Sonar_Sensor;
      Register   :        Register_Address;
      Data       :        UInt8;
      Successful :    out Boolean);
   --  Writes the Data byte to the register at the address in Register

   procedure Write_Sequential_Registers
     (This           : in out Ultrasonic_Sonar_Sensor;
      First_Register :        Register_Address;
      Data           :        Sequence;
      Successful     :    out Boolean);
   --  Writes data to a series of registers, starting with First_Register. A
   --  single byte is written each register, taken from Dara. Each "subsequent"
   --  register has an address one greater than the current register. The
   --  number of registers to be written is determined by Data'Length.

   --  See the LEGO MINDSTORMS NXT Hardware Development Kit document.
   --  See the LEGO MINDSTORMS NXT Ultrasonic Sensor I2C Communication Protocol
   --  document.
   Version_Register             : constant Register_Address := 16#0#;
   Product_ID_Register          : constant Register_Address := 16#8#;
   Sensor_Type_Register         : constant Register_Address := 16#10#;
   Factory_Data_Register        : constant Register_Address := 16#11#;
   Units_Register               : constant Register_Address := 16#14#;
   Continuous_Interval_Register : constant Register_Address := 16#40#;
   Mode_Register                : constant Register_Address := 16#41#;
   Distance_Register            : constant Register_Address := 16#42#;
   Calibration_Register         : constant Register_Address := 16#4A#;
   --  NB: the value for Calibration_Register is correct, but differs from
   --  the value specified in the Ultrasonic Sensor I2C Communication Protocol
   --  document.

   --  The values sent to the mode register.
   Off_Command                       : constant UInt8 := 16#0#;
   Ping_Command                      : constant UInt8 := 16#1#;
   Enter_Continuous_Measurement_Mode : constant UInt8 := 16#2#;
   Event_Capture_Command             : constant UInt8 := 16#3#;
   Request_Warm_Reset                : constant UInt8 := 16#4#;

end NXT.Ultrasonic_Sensors;
