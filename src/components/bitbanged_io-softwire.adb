------------------------------------------------------------------------------
--                                                                          --
--                   Copyright (C) 2018-2022, AdaCore                       --
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

with STM32.Device; use STM32.Device;

package body BitBanged_IO.SoftWire is

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (This            : in out I2C_Master;
      Data_Line       :        GPIO_Point;
      Clock_Line      :        GPIO_Point;
      Clock_Frequency :        UInt32;
      Success         :    out Boolean)
   is
   begin
      This.Configure
        (Data_Pin        => Data_Line,
         Clock_Pin       => Clock_Line,
         Clock_Frequency => Clock_Frequency);
      This.Initialized := This.Data_Line.Set and This.Clock_Line.Set;
      Success := This.Initialized;
   end Initialize;

   ------------------------
   -- Begin_Transmission --
   ------------------------

   procedure Begin_Transmission
     (This           : in out I2C_Master;
      Device_Address :        UInt8;
      Acknowledged   :    out Boolean)
   is
   begin
      if This.Transmitting then
         This.Restart (Device_Address, Transmitter, Acknowledged);
         Acknowledged := True; --  ??
      else
         This.Start (Device_Address, Transmitter, Acknowledged);
         if Acknowledged then
            This.Transmitting := True;
         else
            This.Transmitting := False;
            This.Stop;
         end if;
      end if;
   end Begin_Transmission;

   ----------------------
   -- End_Transmission --
   ----------------------

   procedure End_Transmission
     (This      : in out I2C_Master;
      Send_Stop :        Boolean := True)
   is
   begin
      if Send_Stop then
         This.Stop;
         This.Transmitting := False;
      end if;
   end End_Transmission;

   -----------
   -- Write --
   -----------

   procedure Write
     (This         : in out I2C_Master;
      Output       :        UInt8;
      Acknowledged :    out Boolean)
   is
      Reply : UInt8;
   begin
      This.Write_Byte (Output);
      This.Read_Bit (Reply);
      Acknowledged := Reply = Ack;
   end Write;

   -----------
   -- Write --
   -----------

   procedure Write
     (This         : in out I2C_Master;
      Output       :        Sequence;
      Acknowledged :    out Boolean)
   is
   begin
      for K in Output'Range loop
         This.Write (Output (K), Acknowledged);
         exit when not Acknowledged;
      end loop;
   end Write;

   ------------------
   -- Request_From --
   ------------------

   procedure Request_From
     (This             : in out I2C_Master;
      Device_Address   :        UInt8;
      Register_Address :        UInt8;
      Buffer           :    out Sequence;
      Quantity         :        Positive;
      Acknowledged     :    out Boolean;
      Send_Stop        :        Boolean := True)
   is
      K : Integer range Buffer'Range;
   begin
      if Register_Address /= 0 then
         This.Begin_Transmission (Device_Address, Acknowledged);
         if not Acknowledged then
            This.Stop;
            return;
         end if;
         This.Write (Register_Address, Acknowledged);
         if not Acknowledged then
            This.Stop;
            return;
         end if;
         This.End_Transmission (Send_Stop => False);
      end if;

      This.Restart (Device_Address, Receiver, Acknowledged);
      if not Acknowledged then
         return;
      end if;
      K := Buffer'First;
      for Count in 1 .. Integer'Min (Quantity, Buffer'Length) - 1 loop
         This.Read (Buffer (K)); -- sends an Ack
         K := K + 1;
      end loop;
      This.Read_Last (Buffer (Buffer'Last));  -- sends a NAck

      if Send_Stop then
         This.Stop;
         This.Transmitting := False;
      end if;
      Acknowledged := True;
   end Request_From;

   ------------------
   -- Request_From --
   ------------------

   procedure Request_From
     (This             : in out I2C_Master;
      Device_Address   :        UInt8;
      Buffer           :    out Sequence;
      Quantity         :        Positive;
      Acknowledged     :    out Boolean;
      Send_Stop        :        Boolean := True)
   is
   begin
      This.Request_From
        (Device_Address,
         Register_Address => 0, -- thus unused and no subtransaction
         Quantity         => Quantity,
         Buffer           => Buffer,
         Acknowledged     => Acknowledged,
         Send_Stop        => Send_Stop);
   end Request_From;

   ----------
   -- Read --
   ----------

   procedure Read (This : in out I2C_Master; Value : out UInt8) is
   begin
      This.Read_Byte (Value);
      This.Write_Bit (Ack);
   end Read;

   ---------------
   -- Read_Last --
   ---------------

   procedure Read_Last (This : in out I2C_Master; Value : out UInt8) is
   begin
      This.Read_Byte (Value);
      This.Write_Bit (NAck);
   end Read_Last;

   ------------------------
   -- Write_7Bit_Address --
   ------------------------

   procedure Write_7Bit_Address
     (This           : in out I2C_Master;
      Device_Address :        UInt8;
      Role           :        Master_IO_Roles)
   is
      Destination : UInt8 := Device_Address;
      --  NB: we don't shift the address into the upper 7 bits, the user must
      --  do so beforehand
   begin
      --  If the low-order bit is zero the master is requesting to write to
      --  the slave at this device address. If the bit is 1 the master is
      --  requesting to read from the slave at this device address.
      if Role = Receiver then
         Destination := Destination or 16#1#;
      else  --  acting as transmitter
         Destination := Destination and (not 16#1#);
      end if;

      This.Write_Byte (Destination);
   end Write_7Bit_Address;

   -----------
   -- Start --
   -----------

   procedure Start
     (This           : in out I2C_Master;
      Device_Address :        UInt8;
      Role           :        Master_IO_Roles;
      Acknowledged   :    out Boolean)
   is
      Reply : UInt8;
   begin
      --  the clock line should be high here, already
      pragma Assert (This.Clock_Line.Set);

      This.Data_Line_Low;
      delay until Clock + This.Half_Clock_Cycle;

      This.Write_7Bit_Address (Device_Address, Role);
      This.Read_Bit (Reply);
      Acknowledged := Reply = Ack;
   end Start;

   -------------
   -- Restart --
   -------------

   procedure Restart
     (This           : in out I2C_Master;
      Device_Address :        UInt8;
      Role           :        Master_IO_Roles;
      Acknowledged   :    out Boolean)
   is
      Reply      : UInt8;
      Half_Cycle : constant Time_Span := This.Half_Clock_Cycle_Time;
      --  We will delay the full half-cycle time even though we are doing
      --  some processing before starting each delay (driving the lines high or
      --  low). In other words, ideally we'd subtract the time taken to do that
      --  processing from the half-cycle time, and only delay that resulting
      --  amount. However, I2C is not demanding in this regard so it won't be
      --  a problem.
   begin
      This.Clock_Line_Low;
      This.Data_Line_High;
      delay until Clock + Half_Cycle;

      This.Clock_Line_High;
      delay until Clock + Half_Cycle;

      This.Data_Line_Low;
      delay until Clock + Half_Cycle;

      This.Write_7Bit_Address (Device_Address, Role);
      This.Read_Bit (Reply);
      Acknowledged := Reply = Ack;
   end Restart;

   ----------
   -- Stop --
   ----------

   procedure Stop (This : in out I2C_Master) is
      Half_Cycle : constant Time_Span := This.Half_Clock_Cycle_Time;
      --  We will delay the full half-cycle time even though we are doing some
      --  processing before starting each delay (driving the lines high or
      --  low). In other words, ideally we'd subtract the time taken to do that
      --  processing from the half-cycle time, and only delay that resulting
      --  amount. However, I2C is not demanding in this regard so it won't be
      --  a problem.
   begin
      This.Clock_Line_Low;
      This.Data_Line_Low;
      delay until Clock + Half_Cycle;

      This.Clock_Line_High;
      delay until Clock + Half_Cycle;

      This.Data_Line_High;
      delay until Clock + Half_Cycle;
   end Stop;

end BitBanged_IO.SoftWire;
