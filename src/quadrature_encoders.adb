------------------------------------------------------------------------------
--                                                                          --
--                    Copyright (C) 2017, AdaCore                           --
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
--     3. Neither the name of the copyright holder nor the names of its     --
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
with System;        use System;
with STM32_SVD;     use STM32_SVD;

package body Quadrature_Encoders is

   -----------------------
   -- Current_Direction --
   -----------------------

   function Current_Direction (This : Rotary_Encoder) return Counting_Direction is
   begin
      case Current_Counter_Mode (This.all) is
         when Up     => return Up;
         when Down   => return Down;
         when others => raise Program_Error;
      end case;
   end Current_Direction;

   -----------------
   -- Reset_Count --
   -----------------

   procedure Reset_Count (This : in out Rotary_Encoder) is
   begin
      Set_Counter (This.all, UInt16'(0));
   end Reset_Count;

   -------------------
   -- Current_Count --
   -------------------

   function Current_Count (This : Rotary_Encoder) return UInt32 is
   begin
      return Current_Counter (This.all);
   end Current_Count;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize_Encoder
     (This          : in out Rotary_Encoder;
      Encoder_TI1   : GPIO_Point;
      Encoder_TI2   : GPIO_Point;
      Encoder_Timer : not null access Timer;
      Encoder_AF    : GPIO_Alternate_Function)
   is
      Configuration : GPIO_Port_Configuration;

      Debounce_Filter : constant Timer_Input_Capture_Filter := 6;
      --  See the STM32 RM, pg 561, re: ICXF, to set the input filtering.

      Period : constant UInt32 := (if Has_32bit_Counter (Encoder_Timer.all)
                                 then UInt32'Last else UInt32 (UInt16'Last));
   begin
      This := Rotary_Encoder (Encoder_Timer);

      Enable_Clock (Encoder_TI1);
      Enable_Clock (Encoder_TI2);
      Enable_Clock (Encoder_Timer.all);

      Configuration.Mode := Mode_AF;
      Configuration.Output_Type := Push_Pull;
      Configuration.Resistors := Pull_Up;
      Configuration.Speed := Speed_100MHz;

      Encoder_TI1.Configure_IO (Configuration);
      Encoder_TI1.Configure_Alternate_Function (Encoder_AF);

      Encoder_TI2.Configure_IO (Configuration);
      Encoder_TI2.Configure_Alternate_Function (Encoder_AF);

      Encoder_TI1.Lock;
      Encoder_TI2.Lock;

      Configure
        (Encoder_Timer.all,
         Prescaler     => 0,
         Period        => Period,
         Clock_Divisor => Div1,
         Counter_Mode  => Up);

      Configure_Encoder_Interface
        (Encoder_Timer.all,
         Mode         => Encoder_Mode_TI1_TI2,
         IC1_Polarity => Rising,
         IC2_Polarity => Rising);

      Configure_Channel_Input
        (Encoder_Timer.all,
         Channel   => Channel_1,
         Polarity  => Rising,
         Selection => Direct_TI,
         Prescaler => Div1,
         Filter    => Debounce_Filter);

      Configure_Channel_Input
        (Encoder_Timer.all,
         Channel   => Channel_2,
         Polarity  => Rising,
         Selection => Direct_TI,
         Prescaler => Div1,
         Filter    => Debounce_Filter);

      Set_Autoreload (Encoder_Timer.all, Period);

      Enable_Channel (Encoder_Timer.all, Channel_1);
      Enable_Channel (Encoder_Timer.all, Channel_2);

      if Has_32bit_Counter (Encoder_Timer.all) then
         Set_Counter (Encoder_Timer.all, UInt32'(0));
      else
         Set_Counter (Encoder_Timer.all, UInt16'(0));
      end if;

      Enable (Encoder_Timer.all);
   end Initialize_Encoder;

   -------------------
   -- Bidirectional --
   -------------------

   function Bidirectional (This : Timer) return Boolean is
     (This'Address = TIM1_Base or
      This'Address = TIM2_Base or
      This'Address = TIM3_Base or
      This'Address = TIM4_Base or
      This'Address = TIM5_Base or
      This'Address = TIM8_Base);

end Quadrature_Encoders;
