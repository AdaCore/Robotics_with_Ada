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

--  This package provides a bit-banged implementation of the low-level I/O
--  required for driving the two lines, Clock (SCL) and Data (SDA), required
--  for I2C and other two-wire protocols.

--  This package is implemented in terms specific to the STM32 products.

with STM32.GPIO;    use STM32.GPIO;
with HAL;           use HAL;
with Ada.Real_Time; use Ada.Real_Time;

package BitBanged_I2C is

   type Port is tagged limited private;

   procedure Configure
     (This            : in out Port;
      Data_Pin        : GPIO_Point;
      Clock_Pin       : GPIO_Point;
      Clock_Frequency : UInt32)
   with
     Post => IO_Configured (This);
   --  Configures the two GPIO lines as outputs, with open_drain and internal
   --  pull-up resistors disabled.
   --
   --  According to the STM32 F4 Reference Manual, section 8.3.10, when the
   --  pins are configured as outputs with Open_Drain selected, a 0 in the
   --  Output register activates the N-MOS whereas a 1 in the Output register
   --  leaves the port in Hi-Z (the P-MOS is never activated). As a result,
   --  setting the pin to 0 will actively drive the line low, but setting the
   --  pin to a 1 passively allows the line to float high. The behavior is
   --  essential to communications based on two wires.

   function IO_Configured (This : Port) return Boolean;

   procedure Read_Bit (This : in out Port; Value : out UInt8) with
     Pre  => IO_Configured (This),
     Post => Value <= 1;
   --  Reads the next bit coming in over the Data_Line into Value, in one
   --  clock cycle. Drives the clock line using the routines below and delay
   --  until statements (using Half_Clock_Cycle) to create the appropriate-size
   --  clock cycle.

   procedure Write_Bit (This : in out Port; Value : UInt8) with
     Pre => IO_Configured (This);
   --  Writes the LSB of Value onto the Data_Line, in one clock cycle. Drives
   --  the clock line using the routines below and delay until statements
   --  (using Half_Clock_Cycle) to create the appropriate-size clock cycle.

   procedure Read_Byte (This : in out Port; Value : out UInt8) with
     Pre => IO_Configured (This);
   --  Reads the bits coming in over the Data_Line into Value, one bit per
   --  clock cycle. Drives the clock line using the routines below and delay
   --  until statements (using Half_Clock_Cycle) to create the appropriate-size
   --  clock cycles. The incoming bits are put into value per MSB_First.

   procedure Write_Byte (This  : in out Port; Output : UInt8) with
     Pre => IO_Configured (This);
   --  Writes the bits in Output onto the Data_Line, one bit per clock
   --  cycle. Drives the clock line using the routines below and delay until
   --  statements (using Half_Clock_Cycle) to create the appropriate-size clock
   --  cycles. The outgoing bits are put onto the line per MSB_First.

   --  These routines control whether the corresponding line is high or low.
   --  They do not simply set or clear the line in all cases.
   --
   --  Note that they do not include the delay required to have the line at the
   --  required state for the interval matching the clock frequency (because
   --  that is not always appropriate and because they could not be inlined in
   --  that case). Callers of these routines must make the delay calls, but can
   --  use the Half_Clock_Cycle_Time function below to get the time interval to
   --  use.

   procedure Clock_Line_High (This : in out Port) with
     Pre => IO_Configured (This),
     Inline;
   --  Releases the clock line high, but waits for the line to go high before
   --  returning in order to allow slaves to stretch the line low to keep the
   --  master waiting.

   procedure Clock_Line_Low (This : in out Port) with
     Pre => IO_Configured (This),
     Inline;
   --  Actively drives the clock line low

   procedure Data_Line_High (This : in out Port) with
     Pre => IO_Configured (This),
     Inline;
   --  Releases the data line high

   procedure Data_Line_Low (This : in out Port) with
     Pre => IO_Configured (This),
     Inline;
   --  Actively drives the data line low

   function Half_Clock_Cycle_Time (This : Port) return Time_Span with
     Pre => IO_Configured (This),
     Inline;
   --  Returns the amount of time representing 1/2 of the I2C clock cycle time.
   --  Useful for clients to insert delays, in combination with the clock/data
   --  line control routines above.

private

   type Port is tagged limited record
      Data_Line        : GPIO_Point;
      Clock_Line       : GPIO_Point;
      Half_Clock_Cycle : Time_Span;
      Configured       : Boolean := False;
   end record;

   function IO_Configured (This : Port) return Boolean is
      (This.Configured);

   function Half_Clock_Cycle_Time (This : Port) return Time_Span is
      (This.Half_Clock_Cycle);

end BitBanged_I2C;
