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

--  This package provides an interface to bit-banged I2C based on the Arduino
--  Wire API and the Softwire implementation found here:
--  https://github.com/felias-fogg/SoftI2CMaster/blob/master/SoftWire.h

package BitBanged_I2C.SoftWire is

   type I2C_Master is new BitBanged_I2C.Port with private;

   procedure Initialize
     (This            : in out I2C_Master;
      Data_Line       :        GPIO_Point;
      Clock_Line      :        GPIO_Point;
      Clock_Frequency :        UInt32;
      Success         :    out Boolean)
   with Post => Initialized (This);
   --  Success will be true iff Clock_line and Data_Line are both high as a
   --  result of the initialization steps.

   function Initialized (This : I2C_Master) return Boolean;

   procedure Begin_Transmission
     (This           : in out I2C_Master;
      Device_Address :        UInt8;
      Acknowledged   :    out Boolean)
   with Pre => Initialized (This);

   procedure End_Transmission
     (This      : in out I2C_Master;
      Send_Stop :        Boolean := True)
   with Pre => Initialized (This);

   procedure Write
     (This         : in out I2C_Master;
      Output       :        UInt8;
      Acknowledged :    out Boolean)
   with Pre => Initialized (This);

   type Sequence is array (Positive range <>) of UInt8;

   procedure Write
     (This         : in out I2C_Master;
      Output       :        Sequence;
      Acknowledged :    out Boolean)
   with Pre => Initialized (This);

   procedure Request_From
     (This             : in out I2C_Master;
      Device_Address   :        UInt8;
      Buffer           :    out Sequence;
      Quantity         :        Positive;
      Acknowledged     :    out Boolean;
      Send_Stop        :        Boolean := True)
   with Pre => Initialized (This);

   procedure Request_From
     (This             : in out I2C_Master;
      Device_Address   :        UInt8;
      Register_Address :        UInt8;
      Buffer           :    out Sequence;
      Quantity         :        Positive;
      Acknowledged     :    out Boolean;
      Send_Stop        :        Boolean := True)
   with Pre => Initialized (This);

private

   Ack  : constant := 0;
   NAck : constant := 1;

   type I2C_Master is new BitBanged_I2C.Port with record
      Initialized  : Boolean := False;
      Transmitting : Boolean := False;
   end record;

   type Master_IO_Roles is (Transmitter, Receiver);

   procedure Write_7Bit_Address
     (This           : in out I2C_Master;
      Device_Address :        UInt8;
      Role           :        Master_IO_Roles);
   --  NB: we don't shift the address into the upper 7 bits, the user must
   --  do so beforehand.

   procedure Start
     (This           : in out I2C_Master;
      Device_Address :        UInt8;
      Role           :        Master_IO_Roles;
      Acknowledged   :    out Boolean);

   procedure Stop (This : in out I2C_Master);

   procedure Restart
     (This           : in out I2C_Master;
      Device_Address :        UInt8;
      Role           :        Master_IO_Roles;
      Acknowledged   :    out Boolean);

   procedure Read (This : in out I2C_Master;  Value : out UInt8)
   with Pre => Initialized (This);
   --  sends an Ack after reading byte

   procedure Read_Last (This : in out I2C_Master; Value : out UInt8)
   with Pre => Initialized (This);
   --  sends an NAck after reading byte

   function Initialized (This : I2C_Master) return Boolean is
     (This.Initialized);

end BitBanged_I2C.SoftWire;
