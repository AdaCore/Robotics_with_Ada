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

with STM32;        use STM32;
with STM32.GPIO;   use STM32.GPIO;
with STM32.Timers; use STM32.Timers;

package body NXT_Shield is

   PWM_Frequency : constant := 490;

   procedure Initialize_Hardware is
   begin
      Motor1.Initialize
        (Encoder_Input1       => PA15,
         Encoder_Input2       => PB3,
         Encoder_Timer        => Timer_2'Access,
         Encoder_AF           => GPIO_AF_TIM2_1,

         PWM_Timer            => Timer_4'Access,
         PWM_Output_Frequency => PWM_Frequency,
         PWM_AF               => GPIO_AF_TIM4_2,
         PWM_Output           => PB6,
         PWM_Output_Channel   => Channel_1,

         Polarity1            => PA10,
         Polarity2            => PB1);

      Motor2.Initialize
        (Encoder_Input1       => PA0,
         Encoder_Input2       => PA1,
         Encoder_Timer        => Timer_5'Access,
         Encoder_AF           => GPIO_AF_TIM5_2,

         PWM_Timer            => Timer_3'Access,
         PWM_Output_Frequency => PWM_Frequency,
         PWM_AF               => GPIO_AF_TIM3_2,
         PWM_Output           => PB4,
         PWM_Output_Channel   => Channel_1,

         Polarity1            => PA2,
         Polarity2            => PA3);
   end Initialize_Hardware;

end NXT_Shield;
