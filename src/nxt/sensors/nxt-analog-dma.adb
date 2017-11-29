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

with STM32.Device.Mapping_Requests;
use STM32.Device;

package body NXT.Analog.DMA is

   procedure Initialize_DMA
     (Controller : not null access DMA_Controller;
      Stream     : DMA_Stream_Selector;
      Channel    : DMA_Channel_Selector);

   procedure Initialize_ADC
     (Unit    : not null access Analog_To_Digital_Converter;
      Channel : Analog_Input_Channel);

   ----------------
   -- Assign_DMA --
   ----------------

   procedure Assign_DMA
     (This       : in out NXT_Analog_Sensor_DMA;
      Controller : access DMA_Controller;
      Stream     : DMA_Stream_Selector)
   is
   begin
      This.Controller := Controller;
      This.Stream     := Stream;
   end Assign_DMA;

   ----------------
   -- Initialize --
   ----------------

   overriding
   procedure Initialize (This : in out NXT_Analog_Sensor_DMA) is
      use STM32.Device.Mapping_Requests;

      --  Only certain DMA channels can be mapped to ADC units, so we use this
      --  function to find the channel for This.Converter
      DMA_Channel : constant DMA_Channel_Selector := DMA2_ADC_Request_Mapping
        (This.Converter, This.Controller, This.Stream);
   begin
      Initialize (NXT_Analog_Sensor (This));

      Initialize_DMA (This.Controller, This.Stream, DMA_Channel);
      Initialize_ADC (This.Converter, This.Input_Channel);

      Enable (This.Converter.all);
      Start_Transfer
        (This.Controller.all,
         This.Stream,
         Source      => Data_Register_Address (This.Converter.all),
         Destination => This.Raw_Value'Address,
         Data_Count  => 1);  -- ie, 1 half-word
      Start_Conversion (This.Converter.all);
   end Initialize;

   ---------------------
   -- Get_Raw_Reading --
   ---------------------

   overriding
   procedure Get_Raw_Reading
     (This       : in out NXT_Analog_Sensor_DMA;
      Reading    : out Natural;
      Successful : out Boolean)
   is
   begin
      if Status (This.Converter.all, Overrun) or
         Status (This.Controller.all, This.Stream, FIFO_Error_Indicated) or
         Status (This.Controller.all, This.Stream, Direct_Mode_Error_Indicated) or
         Status (This.Controller.all, This.Stream, Transfer_Error_Indicated) or
         not Status (This.Controller.all, This.Stream, Transfer_Complete_Indicated)
      then
         Successful := False;
      else
         Reading := This.Raw_Value;
         Successful := True;
      end if;
   end Get_Raw_Reading;

   --------------------
   -- Initialize_DMA --
   --------------------

   procedure Initialize_DMA
     (Controller : not null access DMA_Controller;
      Stream     : DMA_Stream_Selector;
      Channel    : DMA_Channel_Selector)
   is
      Config : DMA_Stream_Configuration;
   begin
      Enable_Clock (Controller.all);

      Reset (Controller.all, Stream);

      Config.Channel                      := Channel;
      Config.Direction                    := Peripheral_To_Memory;
      Config.Memory_Data_Format           := HalfWords;
      Config.Peripheral_Data_Format       := HalfWords;
      Config.Increment_Peripheral_Address := False;
      Config.Increment_Memory_Address     := False;
      Config.Operation_Mode               := Circular_Mode;
      Config.Priority                     := Priority_Very_High;
      Config.FIFO_Enabled                 := False;
      Config.Memory_Burst_Size            := Memory_Burst_Single;
      Config.Peripheral_Burst_Size        := Peripheral_Burst_Single;

      Configure (Controller.all, Stream, Config);

      Clear_All_Status (Controller.all, Stream);
   end Initialize_DMA;

   --------------------
   -- Initialize_ADC --
   --------------------

   procedure Initialize_ADC
     (Unit    : not null access Analog_To_Digital_Converter;
      Channel : Analog_Input_Channel)
   is
   begin
      Configure_Regular_Conversions
        (Unit.all,
         Continuous  => True,
         Trigger     => Software_Triggered,
         Enable_EOC  => True,
         Conversions => Regular_Conversion (Channel));

      Enable_DMA (Unit.all);
      Enable_DMA_After_Last_Transfer (Unit.all);
   end Initialize_ADC;

end NXT.Analog.DMA;
