------------------------------------------------------------------------------
--                                                                          --
--                      Copyright (C) 2017, AdaCore                         --
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

package body NXT.Analog.DMA.Sound is

   ----------------
   -- Initialize --
   ----------------

   overriding
   procedure Initialize
     (This : in out NXT_Sound_Sensor)
   is
   begin
      Initialize (NXT_Analog_Sensor_DMA (This));

      Enable_Clock (This.Digital_0);
      Enable_Clock (This.Digital_1);

      Configure_IO
        (This.Digital_0 & This.Digital_1,
         (Mode        => Mode_Out,
          Resistors   => Pull_Down,
          Speed       => Speed_Medium,
          Output_Type => Push_Pull));

      Set_Mode (This, dB);
   end Initialize;

   --------------
   -- Set_Mode --
   --------------

   procedure Set_Mode (This : in out NXT_Sound_Sensor; Mode : Sound_Modes) is
   begin
      case Mode is
         when dB =>
            This.Digital_0.Set;
            This.Digital_1.Clear;
         when dBA =>
            This.Digital_0.Clear;
            This.Digital_1.Set;
      end case;
      This.Mode := Mode;
   end Set_Mode;

   ------------------
   -- Current_Mode --
   ------------------

   function Current_Mode (This : NXT_Sound_Sensor) return Sound_Modes is
      (This.Mode);

end NXT.Analog.DMA.Sound;
