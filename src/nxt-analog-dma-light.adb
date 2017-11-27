------------------------------------------------------------------------------
--                                                                          --
--                      Copyright (C) 2017, AdaCore                         --
--                                                                          --
--  Redistribution and use inC source and binary forms, with or without      --
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

package body NXT.Analog.DMA.Light is

   ----------------
   -- Initialize --
   ----------------

   overriding
   procedure Initialize
     (This : in out NXT_Light_Sensor)
   is
   begin
      Initialize (NXT_Analog_Sensor_DMA (This));

      Enable_Clock (This.Floodlight_Pin);
      This.Floodlight_Pin.Configure_IO
        ((Mode        => Mode_Out,
          Resistors   => Pull_Down,
          Speed       => Speed_Medium,
          Output_Type => Push_Pull));

      Disable_Floodlight (This);
   end Initialize;

   -------------------------
   -- Enable_Output_Light --
   -------------------------

   procedure Enable_Floodlight (This : in out NXT_Light_Sensor) is
   begin
      This.Floodlight_Pin.Set;
      This.Floodlight_Enabled := True;
   end Enable_Floodlight;

   --------------------------
   -- Disable_Output_Light --
   --------------------------

   procedure Disable_Floodlight (This : in out NXT_Light_Sensor) is
   begin
      This.Floodlight_Pin.Clear;
      This.Floodlight_Enabled := False;
   end Disable_Floodlight;

   ------------------------
   -- Floodlight_Enabled --
   ------------------------

   function Floodlight_Enabled (This : NXT_Light_Sensor) return Boolean is
      (This.Floodlight_Enabled);

end NXT.Analog.DMA.Light;
