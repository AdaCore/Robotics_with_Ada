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

package body NXT.Analog_Sensors is

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (This : in out NXT_Analog_Sensor)
   is
   begin
      Enable_Clock (This.Input_Pin.all);
      This.Input_Pin.Configure_IO ((Mode_Analog, Resistors => Floating));

      Enable_Clock (This.Converter.all);
      Configure_Unit
        (This.Converter.all,
         Resolution => NXT_Brick_ADC_Resolution,
         Alignment  => Right_Aligned);

      --  Further configuration of the ADC unit This.Converter.all is not done
      --  here because the configuration differs: some are continuous and some
      --  not
   end Initialize;

   ------------------------
   -- Get_Scaled_Reading --
   ------------------------

   procedure Get_Scaled_Reading
     (This    : in out NXT_Analog_Sensor;
      Reading : out Intensity;
      Status  : out Reading_Status)
   is
      Raw        : Integer;
      Scaled     : Integer;
      Successful : Boolean;
   begin
      Get_Raw_Reading (NXT_Analog_Sensor'Class (This), Raw, Successful);
      if not Successful then
         Reading := 0;
         Status := Reading_Failure;
         return;
      end if;
      Raw := As_Varying_Directly (Raw);
      Scaled := Mapped (Raw, This.Low, This.High, Intensity'First, Intensity'Last);
      Reading := Constrained (Scaled, Intensity'First, Intensity'Last);
      Status := Valid_Reading;
   end Get_Scaled_Reading;

   ------------
   -- Enable --
   ------------

   procedure Enable (This : in out NXT_Analog_Sensor) is
   begin
      STM32.ADC.Enable (This.Converter.all);
   end Enable;

   -------------
   -- Disable --
   -------------

   procedure Disable (This : in out NXT_Analog_Sensor) is
   begin
      STM32.ADC.Disable (This.Converter.all);
   end Disable;

   -------------
   -- Enabled --
   -------------

   function Enabled (This : NXT_Analog_Sensor) return Boolean is
      (STM32.ADC.Enabled (This.Converter.all));

   ---------------------
   -- Set_Calibration --
   ---------------------

   procedure Set_Calibration
     (This     : in out NXT_Analog_Sensor;
      Least    : Varying_Directly;
      Greatest : Varying_Directly)
   is
   begin
      This.Low := Least;
      This.High := Greatest;
   end Set_Calibration;

   -----------------
   -- Calibration --
   -----------------

   function Calibration (This : NXT_Analog_Sensor) return Sensor_Calibration is
     (This.Low, This.High);

end NXT.Analog_Sensors;
