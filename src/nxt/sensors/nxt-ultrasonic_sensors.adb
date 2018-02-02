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

with STM32.Device;  use STM32.Device;

with STM32.Board; use STM32.Board; ---  for testing only...

package body NXT.Ultrasonic_Sensors is

   ---------------
   -- Configure --
   ---------------

   procedure Configure
     (This            : in out Ultrasonic_Sonar_Sensor;
      Data_Line       :        GPIO_Point;
      Clock_Line      :        GPIO_Point;
      Clock_Frequency :        UInt32;
      Success         :    out Boolean)
   is
   begin
      This.Initialize
        (Data_Line       => Data_Line,
         Clock_Line      => Clock_Line,
         Clock_Frequency => Clock_Frequency,
         Success         => This.Configured);

      Success := This.Configured;
      This.Device_Address := UInt8 (This.Hardware_Device_Address) * 2;
      Disable (This);
   end Configure;

   ----------
   -- Ping --
   ----------

   procedure Ping (This : in out Ultrasonic_Sonar_Sensor) is
      Success  : Boolean;
   begin
      delay until This.Next_Command_Time;
      Write_Register (This, Mode_Register, Ping_Command, Success);
      This.Next_Command_Time := Clock + Delay_Data_Ping;
   end Ping;

   ------------------
   -- Get_Distance --
   ------------------

   procedure Get_Distance
     (This    : in out Ultrasonic_Sonar_Sensor;
      Reading :    out Centimeters)
   is
      Success : Boolean;
   begin
      delay until This.Next_Command_Time;
      Read_Register (This, Distance_Register, UInt8 (Reading), Success);
      This.Next_Command_Time := Clock + Delay_Data_Other;
   end Get_Distance;

   -------------------
   -- Get_Distances --
   -------------------

   procedure Get_Distances
     (This      : in out Ultrasonic_Sonar_Sensor;
      Readings  : out Distances;
      Actual    : out Natural)
   is
      Use_First_Index : constant := 0;
   begin
      Get_Distances (This, Readings'Length, Use_First_Index, Readings, Actual);
   end Get_Distances;

   -------------------
   -- Get_Distances --
   -------------------

   procedure Get_Distances
     (This      : in out Ultrasonic_Sonar_Sensor;
      Requested : Distances_Index;
      Offset    : Natural;
      Readings  : out Distances;
      Actual    : out Natural)
   is
      Incoming    : aliased Sequence (Distances_Index);
      Success     :  Boolean;

      Readings_Offset : constant Natural := Readings'First - Incoming'First;
      --  we don't know that the actual for Readings uses a 1-based index
   begin
      delay until This.Next_Command_Time;
      Read_Sequential_Registers
        (This,
         First_Register => Distance_Register,
         Data           => Incoming,
         Successful     => Success);
      Actual := 0;
      if Success then
         for K in 1 .. Requested loop
            exit when Incoming (K) = UInt8 (Nothing_Detected);
            Readings (K + Offset + Readings_Offset) := Centimeters (Incoming (K));
            Actual := Actual + 1;
         end loop;
      end if;
      This.Next_Command_Time := Clock + Delay_Data_Other;
   end Get_Distances;

   ----------------
   -- Configured --
   ----------------

   function Configured (This : Ultrasonic_Sonar_Sensor) return Boolean is
      (This.Configured);

   ------------
   -- Enable --
   ------------

   procedure Enable
     (This : in out Ultrasonic_Sonar_Sensor;
      Mode : Scan_Modes := Continuous)
   is
      Successful : Boolean;
   begin
      if Mode = Continuous then
         --  go into continuous can mode
         delay until This.Next_Command_Time;
         Write_Register (This, Mode_Register, Enter_Continuous_Measurement_Mode, Successful);
         This.Next_Command_Time := Clock + Command_Delay;
      else
         --  we don't issue a command in this mode, until we want to get a
         --  reading, so we will issue a "ping" then
         Successful := True;
      end if;
      if Successful then
         This.Mode := Mode;
      end if;
      This.Enabled := Successful;
   end Enable;

   -----------------------
   -- Current_Scan_Mode --
   -----------------------

   function Current_Scan_Mode (This : Ultrasonic_Sonar_Sensor) return Scan_Modes is
     (This.Mode);

   -------------
   -- Disable --
   -------------

   procedure Disable (This : in out Ultrasonic_Sonar_Sensor) is
      Successful : Boolean;
   begin
      delay until This.Next_Command_Time;
      Write_Register (This, Mode_Register, Off_Command, Successful);
      if Successful then
         This.Enabled := False;
      end if;
      This.Next_Command_Time := Clock + Command_Delay;
   end Disable;

   -------------
   -- Enabled --
   -------------

   function Enabled (This : Ultrasonic_Sonar_Sensor) return Boolean is
     (This.Enabled);

   --------------------
   -- Get_Product_Id --
   --------------------

   procedure Get_Product_Id
     (This    : in out Ultrasonic_Sonar_Sensor;
      Value   : out String;
      Success : out Boolean)
   is
   begin
      delay until This.Next_Command_Time;
      Read_Register (This, Product_ID_Register, Value, Success);
      This.Next_Command_Time := Clock + Command_Delay;
   end Get_Product_Id;

   -----------------
   -- Get_Version --
   -----------------

   procedure Get_Version
     (This    : in out Ultrasonic_Sonar_Sensor;
      Value   :    out String;
      Success :    out Boolean)
   is
   begin
      delay until This.Next_Command_Time;
      Read_Register (This, Version_Register, Value, Success);
      This.Next_Command_Time := Clock + Command_Delay;
   end Get_Version;

   ---------------------
   -- Get_Sensor_Type --
   ---------------------

   procedure Get_Sensor_Type
     (This    : in out Ultrasonic_Sonar_Sensor;
      Value   :    out String;
      Success :    out Boolean)
   is
   begin
      delay until This.Next_Command_Time;
      Read_Register (This, Sensor_Type_Register, Value, Success);
      This.Next_Command_Time := Clock + Command_Delay;
   end Get_Sensor_Type;

   ------------------
   -- Warm_Restart --
   ------------------

   procedure Warm_Restart
     (This    : in out Ultrasonic_Sonar_Sensor;
      Success : out Boolean)
   is
   begin
      delay until This.Next_Command_Time;
      Write_Register (This, Mode_Register, Request_Warm_Reset, Success);
      This.Next_Command_Time := Clock + Command_Delay;
   end Warm_Restart;

   ---------------
   -- Get_Units --
   ---------------

   procedure Get_Units
     (This    : in out Ultrasonic_Sonar_Sensor;
      Units   :    out Units_String;
      Success :    out Boolean)
   is
   begin
      delay until This.Next_Command_Time;
      Read_Register (This, Units_Register, Units, Success);
      This.Next_Command_Time := Clock + Command_Delay;
   end Get_Units;

   ----------------------
   -- Get_Factory_Data --
   ----------------------

   procedure Get_Factory_Calibration_Data
     (This            : in out Ultrasonic_Sonar_Sensor;
      Calibrated_Zero :    out UInt8;
      Scale_Factor    :    out UInt8;
      Scale_Divisor   :    out UInt8;
      Success         :    out Boolean)
   is
      Info : Sequence (1 .. 3);
   begin
      delay until This.Next_Command_Time;
      Read_Sequential_Registers (This, Factory_Data_Register, Info, Success);
      if Success then
         Calibrated_Zero := Info (1);
         Scale_Factor := Info (2);
         Scale_Divisor := Info (3);
      end if;
      This.Next_Command_Time := Clock + Command_Delay;
   end Get_Factory_Calibration_Data;

   --------------------------
   -- Get_Calibration_Data --
   --------------------------

   procedure Get_Calibration_Data
     (This            : in out Ultrasonic_Sonar_Sensor;
      Calibrated_Zero :    out UInt8;
      Scale_Factor    :    out UInt8;
      Scale_Divisor   :    out UInt8;
      Success         :    out Boolean)
   is
      Info : Sequence (1 .. 3);
   begin
      delay until This.Next_Command_Time;
      Read_Sequential_Registers (This, Calibration_Register, Info, Success);
      if Success then
         Calibrated_Zero := Info (1);
         Scale_Factor := Info (2);
         Scale_Divisor := Info (3);
      end if;
      This.Next_Command_Time := Clock + Command_Delay;
   end Get_Calibration_Data;

   --------------------------
   -- Set_Calibration_Data --
   --------------------------

   procedure Set_Calibration_Data
     (This            : in out Ultrasonic_Sonar_Sensor;
      Calibrated_Zero :        UInt8;
      Scale_Factor    :        UInt8;
      Scale_Divisor   :        UInt8;
      Success         :    out Boolean)
   is
      Settings : constant Sequence := (Calibrated_Zero, Scale_Factor, Scale_Divisor);
   begin
      delay until This.Next_Command_Time;
      Write_Sequential_Registers (This, Calibration_Register, Settings, Success);
      This.Next_Command_Time := Clock + Command_Delay;
   end Set_Calibration_Data;

   -----------------------------
   -- Get_Continuous_Interval --
   -----------------------------

   procedure Get_Continuous_Interval
     (This     : in out Ultrasonic_Sonar_Sensor;
      Interval :    out UInt8;
      Success  :    out Boolean)
   is
   begin
      delay until This.Next_Command_Time;
      Read_Register (This, Continuous_Interval_Register, Interval, Success);
      This.Next_Command_Time := Clock + Command_Delay;
   end Get_Continuous_Interval;

   -----------------------------
   -- Set_Continuous_Interval --
   -----------------------------

   procedure Set_Continuous_Interval
     (This     : in out Ultrasonic_Sonar_Sensor;
      Interval :        UInt8;
      Success  :    out Boolean)
   is
   begin
      delay until This.Next_Command_Time;
      Write_Register (This, Continuous_Interval_Register, Interval, Success);
      This.Next_Command_Time := Clock + Command_Delay;
   end Set_Continuous_Interval;

   -------------------
   -- Read_Register --
   -------------------

   procedure Read_Register
     (This       : in out Ultrasonic_Sonar_Sensor;
      Register   :        Register_Address;
      Data       :    out UInt8;
      Successful :    out Boolean)
   is
      Buffer : Sequence (1 .. 1);
   begin
      Read_Register (This, Register, Buffer, Successful);
      if Successful then
         Data := Buffer (1);
      end if;
   end Read_Register;

   -------------------
   -- Read_Register --
   -------------------

   procedure Read_Register
     (This     : in out Ultrasonic_Sonar_Sensor;
      Register :        Register_Address;
      Value    :    out String;
      Success  :    out Boolean)
   is
      Response : Sequence (1 .. 8);
      Index    : Positive := Value'First;
   begin
      Read_Register (This, Register, Response, Success);
      if Success then
         for R of Response loop
            Value (Index) := Character'Val (R);
            exit when R = 0;
            Index := Index + 1;
            exit when Index > Value'Last;
         end loop;
         if Index <= Value'Last then
            Value (Index .. Value'Last) := (others => ' ');
         end if;
      end if;
   end Read_Register;

   -------------------
   -- Read_Register --
   -------------------

   procedure Read_Register
     (This       : in out Ultrasonic_Sonar_Sensor;
      Register   :        Register_Address;
      Data       :    out Sequence;
      Successful :    out Boolean)
   is

      procedure Clock_Pulse;
      --  This routine adds an extra clock pulse required by a bug in the Lego
      --  NXT ultrasonic sensor firmware. As a result of the bug, the sensor
      --  does not follow I2C standard protocol. That is the reason for this
      --  bit-banging implementation, rather than simply using the STM32F4 I2C
      --  device.

      procedure Clock_Pulse is
         Interval : constant Time_Span := This.Half_Clock_Cycle_Time;
      begin
         This.Clock_Line_Low;
         delay until Clock + Interval;
         This.Clock_Line_High;
         delay until Clock + Interval;
      end Clock_Pulse;

   begin
      This.Begin_Transmission (This.Device_Address, Successful);
      if not Successful then
         STM32.Board.Turn_On (Red_LED);  -- for testing only
         return;
      end if;

      This.Write (UInt8 (Register), Successful);
      if not Successful then
         STM32.Board.Turn_On (Orange_LED);  -- for testing only
         return;
      end if;

      This.End_Transmission (Send_Stop => False);

      Clock_Pulse; --  required for NXT ultrasonic sensor's firmware bug

      This.Request_From
        (Device_Address => This.Device_Address,
         Buffer         => Data,
         Quantity       => Data'Length,
         Acknowledged   => Successful,
         Send_Stop      => True);

      if not Successful then
         STM32.Board.Turn_On (Blue_LED);  -- for testing only
      end if;
   end Read_Register;

   -------------------------------
   -- Read_Sequential_Registers --
   -------------------------------

   procedure Read_Sequential_Registers
     (This           : in out Ultrasonic_Sonar_Sensor;
      First_Register :        Register_Address;
      Data           :    out Sequence;
      Successful     :    out Boolean)
   is
      Response : UInt8;
      Register : Register_Address := First_Register;
   begin
      for K in Data'Range loop
         Read_Register (This, Register, Response, Successful);
         exit when not Successful;
         delay until Clock + Command_Delay;
         Data (K) := Response;
         Register := Register + 1;
      end loop;
   end Read_Sequential_Registers;

   --------------------
   -- Write_Register --
   --------------------

   procedure Write_Register
     (This       : in out Ultrasonic_Sonar_Sensor;
      Register   :        Register_Address;
      Data       :        UInt8;
      Successful :    out Boolean)
   is
   begin
      This.Begin_Transmission (This.Device_Address, Successful);
      if not Successful then
         STM32.Board.Turn_On (Red_LED);  -- for testing only
         return;
      end if;

      This.Write (UInt8 (Register) & Data, Successful);
      if not Successful then
         STM32.Board.Turn_On (Orange_LED);  -- for testing only
         return;
      end if;

      This.End_Transmission;
   end Write_Register;
   --------------------------------
   -- Write_Sequential_Registers --
   --------------------------------

   procedure Write_Sequential_Registers
     (This           : in out Ultrasonic_Sonar_Sensor;
      First_Register :        Register_Address;
      Data           :        Sequence;
      Successful     :    out Boolean)
   is
      Register : Register_Address := First_Register;
   begin
      for K in Data'Range loop
         Write_Register (This, Register, Data (K), Successful);
         exit when not Successful;
         Register := Register + 1;
         delay until Clock + Command_Delay;
      end loop;
   end Write_Sequential_Registers;

end NXT.Ultrasonic_Sensors;
