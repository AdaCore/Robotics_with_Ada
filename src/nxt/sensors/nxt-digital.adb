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
with HAL.I2C;

package body NXT.Digital is

   use type HAL.I2C.I2C_Status;

   ---------------
   -- Configure --
   ---------------

   procedure Configure
     (This        : in out NXT_Digital_Sensor;
      Port        : access I2C_Port;
      SCL         : in out GPIO_Point;
      SDA         : in out GPIO_Point;
      AF_Code     :        GPIO_Alternate_Function;
      Clock_Speed :        UInt32)
   is
   begin
      This.Port := Port;
      This.Device_Address := UInt8 (This.Device_Hardware_Address) * 2;

      Enable_Clock (SCL);
      Enable_Clock (SDA);
      Enable_Clock (Port.all);

      STM32.Device.Reset (Port.all);

      Configure_IO
        (SCL & SDA,   -- TODO: this order is required for correct functionality!
         (Mode           => Mode_AF,
          AF             => AF_Code,
          AF_Speed       => Speed_100MHz,
          AF_Output_Type => Open_Drain,  -- Open_Drain is essential for I2C
          Resistors      => Pull_Up));   -- Pull_Up is also essential

      STM32.I2C.Configure
        (Port.all,
         (Clock_Speed              => Clock_Speed,
          Addressing_Mode          => Addressing_Mode_7bit,
          General_Call_Enabled     => False,
          Clock_Stretching_Enabled => True,
          Own_Address              => 16#00#,
          others                   => <>));

      This.Port.Set_State (Enabled => True);

      This.Configuration_Completed := True;
   end Configure;

   ----------------
   -- Configured --
   ----------------

   function Configured (This : NXT_Digital_Sensor) return Boolean is
      (This.Configuration_Completed);

   ------------
   -- Enable --
   ------------

   procedure Enable (This : in out NXT_Digital_Sensor) is
   begin
      This.Port.Set_State (Enabled => True);
   end Enable;

   -------------
   -- Disable --
   -------------

   procedure Disable (This : in out NXT_Digital_Sensor) is
   begin
      This.Port.Set_State (Enabled => False);
   end Disable;

   -------------
   -- Enabled --
   -------------

   function Enabled (This : NXT_Digital_Sensor) return Boolean is
     (This.Port.Port_Enabled);

   ---------------------
   -- Get_Sensor_Type --
   ---------------------

   procedure Get_Sensor_Type
     (This    : in out NXT_Digital_Sensor;
      Value   :    out Device_Attribute;
      Success :    out Boolean)
   is
   begin
      Read_Register
        (NXT_Digital_Sensor'Class (This),  -- redispatch if necessary
         Register => Sensor_Type_Register,
         Value    => Value,
         Success  => Success);
   end Get_Sensor_Type;

   --------------------
   -- Get_Product_Id --
   --------------------

   procedure Get_Product_Id
     (This    : in out NXT_Digital_Sensor;
      Value   :    out Device_Attribute;
      Success :    out Boolean)
   is
   begin
      Read_Register
        (NXT_Digital_Sensor'Class (This),  -- redispatch if necessary
         Register => Product_Id_Register,
         Value    => Value,
         Success  => Success);
   end Get_Product_Id;

   -----------------
   -- Get_Version --
   -----------------

   procedure Get_Version
     (This    : in out NXT_Digital_Sensor;
      Value   :    out Device_Attribute;
      Success :    out Boolean)
   is
   begin
      Read_Register
        (NXT_Digital_Sensor'Class (This),  -- redispatch if necessary
         Register => Version_Register,
         Value    => Value,
         Success  => Success);
   end Get_Version;

   -------------------
   -- Read_Register --
   -------------------

   procedure Read_Register
     (This     : in out NXT_Digital_Sensor;
      Register :        Register_Address;
      Value    :    out UInt8;
      Success  :    out Boolean)
   is
      Buffer : Sequence (1 .. 1);
   begin
      Read_Register
        (NXT_Digital_Sensor'Class (This),  -- redispatch if necessary
         Register,
         Buffer,
         Success);
      if Success then
         Value := Buffer (1);
      end if;
   end Read_Register;

   -------------------
   -- Read_Register --
   -------------------

   procedure Read_Register
     (This     : in out NXT_Digital_Sensor;
      Register :        Register_Address;
      Value    :    out Device_Attribute;
      Success  :    out Boolean)
   is
      Buffer : Sequence (Value'Range);
   begin
      Read_Register
        (NXT_Digital_Sensor'Class (This),  -- redispatch if necessary
         Register,
         Buffer,
         Success);
      if not Success then
         return;
      end if;
      for K in Value'Range loop
         if Buffer (K) = 0 then
            Value (K) := ' ';
         else
            Value (K) := Character'Val (Buffer (K));
         end if;
      end loop;
      Success := True;
   end Read_Register;

   -------------------
   -- Read_Register --
   -------------------

   procedure Read_Register
     (This     : in out NXT_Digital_Sensor;
      Register :        Register_Address;
      Value    :    out Sequence;
      Success  :    out Boolean)
   is
      Status : HAL.I2C.I2C_Status;
   begin
      Master_Transmit
        (This.Port.all,
         UInt10 (This.Device_Address),
         Data    => HAL.I2C.I2C_Data'(1 => UInt8 (Register)),
         Status  => Status,
         Timeout => 1_000);
      if Status /= HAL.I2C.Ok then
         Success := False;
         return;
      end if;

      Master_Receive
        (This.Port.all,
         UInt10 (This.Device_Address),
         Data    => HAL.I2C.I2C_Data (Value),
         Status  => Status,
         Timeout => 1_000);
      Success := Status = HAL.I2C.Ok;
   end Read_Register;

   --------------------
   -- Write_Register --
   --------------------

   procedure Write_Register
     (This     : in out NXT_Digital_Sensor;
      Register :        Register_Address;
      Value    :        UInt8;
      Success  :    out Boolean)
   is
      Status   : HAL.I2C.I2C_Status;
      Outgoing : constant HAL.I2C.I2C_Data := (UInt8 (Register), Value);
   begin
      Master_Transmit
        (This.Port.all,
         UInt10 (This.Device_Address),
         Data    => Outgoing,
         Status  => Status,
         Timeout => 1_000);
      Success := Status = HAL.I2C.Ok;
   end Write_Register;

   --------------------
   -- Write_Register --
   --------------------

   procedure Write_Register
     (This     : in out NXT_Digital_Sensor;
      Register :        Register_Address;
      Value    :        Sequence;
      Success  :    out Boolean)
   is
      Status : HAL.I2C.I2C_Status;
   begin
      Master_Transmit
        (This.Port.all,
         UInt10 (This.Device_Address),
         Data    => HAL.I2C.I2C_Data (UInt8 (Register) & Value),
         Status  => Status,
         Timeout => 1_000);
      Success := Status = HAL.I2C.Ok;
   end Write_Register;

end NXT.Digital;
