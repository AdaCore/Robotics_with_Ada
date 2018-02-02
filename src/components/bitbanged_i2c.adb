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

with STM32.Device;   use STM32.Device;

package body BitBanged_I2C is

   ---------------
   -- Configure --
   ---------------

   procedure Configure
     (This            : in out Port;
      Data_Pin        : GPIO_Point;
      Clock_Pin       : GPIO_Point;
      Clock_Frequency : UInt32)
   is
      Output_Config : constant GPIO_Port_Configuration :=
           (Mode_Out,
            Resistors   => Pull_Up,
            --  Open_Drain is essential for two-wire protocols like I2C, so
            --  that both masters and slaves can put the lines high and low.
            --  According to the STM32 F4 Reference Manual, section 8.3.10,
            --  when the pins are configured as outputs with Open_Drain
            --  selected, a 0 in the GPIO port's Output register activates the
            --  N-MOS whereas a 1 in the GPIO port's Output register leaves the
            --  port in Hi-Z (the P-MOS is never activated). Therefore, setting
            --  one of these outputs to 0 really does drive the line low,
            --  whereas setting it high on releases the line, for the sake
            --  of allowing a slave device to control it temporarily.
            Output_Type => Open_Drain,
            Speed       => Speed_Medium);

      Half_Period : constant UInt32 := (1_000_000 / Clock_Frequency) / 2;
      --  When the clock is cycling, this is the time that the clock signal is
      --  either high or low, ie 1/2 the total clock period, in microseconds. For
      --  example, if the clock freq is 9600Hz, the constant would be approx 52
      --  microseconds.

      Scaled_Half_Period : constant UInt32 := (Half_Period / 10) * 10;
      --  Note that we have modified and rebuilt the runtime library so
      --  that the clock resolution is 10 microseconds. See the constant
      --  Tick_Period in package body System.BB.Board_Support, located in file
      --  Ada_Drivers_Library\embedded-runtimes\BSPs\cortex-m\src\gnarl\s-bbbosu.adb
      --  The constant is now:
      --        Clock_Frequency / 100_000;
      --  instead of:
      --        Clock_Frequency / 1000;
      --  Therefore, we want the largest factor of 10 no greater than the
      --  half-period computed from the frequency.
   begin
      This.Data_Line := Data_Pin;
      This.Clock_Line := Clock_Pin;

      Enable_Clock (This.Data_Line);
      Enable_Clock (This.Clock_Line);

      --  According to the STM32 F4 Reference Manual, section 8.3.10, when
      --  the pins are configured as outputs with Open_Drain selected, a 0 in
      --  the Output register activates the N-MOS whereas a 1 in the Output
      --  register leaves the port in Hi-Z (the P-MOS is never activated). The
      --  default state is 0. Therefore, we set both lines to 1 here because
      --  the lines are expected to be high.
      --
      --  This is done before the Configure_IO call below so that the lines do
      --  not go low at all (which would happen because the default 0 values in
      --  the control bits would be effective after the configuration call).
      This.Clock_Line.Set;  -- put a 1 in the bit
      This.Data_Line.Set;   -- put a 1 in the bit
      Configure_IO (This.Clock_Line & This.Data_Line, Output_Config);

      This.Half_Clock_Cycle := Microseconds (Integer (Scaled_Half_Period));

      This.Configured := True;
   end Configure;

   ---------------------
   -- Clock_Line_High --
   ---------------------

   procedure Clock_Line_High (This : in out Port) is
   begin
      This.Clock_Line.Set;
      Clock_Stretched : loop
         exit Clock_Stretched when This.Clock_Line.Set;
      end loop Clock_Stretched;
   end Clock_Line_High;

   --------------------
   -- CLock_Line_Low --
   --------------------

   procedure Clock_Line_Low (This : in out Port) is
   begin
      This.Clock_Line.Clear;
   end Clock_Line_Low;

   --------------------
   -- Data_Line_High --
   --------------------

   procedure Data_Line_High (This : in out Port) is
   begin
      This.Data_Line.Set;
   end Data_Line_High;

   -------------------
   -- Data_Line_Low --
   -------------------

   procedure Data_Line_Low (This : in out Port) is
   begin
      This.Data_Line.Clear;
   end Data_Line_Low;

   ---------------
   --  Read_Bit --
   ---------------

   procedure Read_Bit (This : in out Port; Value : out UInt8) is
   begin
      This.Clock_Line_Low;
      This.Data_Line_High;
      delay until Clock + This.Half_Clock_Cycle;

      This.Clock_Line_High;
      Value := (if This.Data_Line.Set then 1 else 0);
      delay until Clock + This.Half_Clock_Cycle;
   end Read_Bit;

   ----------------
   --  Write_Bit --
   ----------------

   procedure Write_Bit (This : in out Port; Value : UInt8) is
   begin
      This.Clock_Line_Low;
      if Value > 0 then
         This.Data_Line_High;
      else
         This.Data_Line_Low;
      end if;
      delay until Clock + This.Half_Clock_Cycle;

      This.Clock_Line_High;
      delay until Clock + This.Half_Clock_Cycle;
   end Write_Bit;

   ---------------
   -- Read_Byte --
   ---------------

   procedure Read_Byte (This : in out Port; Value : out UInt8) is
      Next_Bit : UInt8;
   begin
      Value := 0;
      for Bit in 1 .. 8 loop
         This.Read_Bit (Next_Bit);
         Value := Shift_Left (Value, 1);
         Value := Value or Next_Bit;
      end loop;
   end Read_Byte;

   ----------------
   -- Write_Byte --
   ----------------

   procedure Write_Byte (This : in out Port; Output : UInt8) is
      Mask : UInt8 := 2#1000_0000#;
   begin
      for Bit in 1 .. 8 loop
         This.Write_Bit (Output and Mask);
         Mask := Shift_Right (Mask, 1);
      end loop;
   end Write_Byte;

end BitBanged_I2C;
