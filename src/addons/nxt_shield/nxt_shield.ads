------------------------------------------------------------------------------
--                                                                          --
--                  Copyright (C) 2017-2018, AdaCore                        --
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

--  This package provides an interface to the "NXT Shield Version 2" produced
--  by TKJ Electronics.
--
--  http://blog.tkjelectronics.dk/2011/10/nxt-shield-ver2/
--  http://shop.tkjelectronics.dk/product_info.php?products_id=29

--  Any object of type NXT.Motors.Motor contains a quadrature rotary encoder
--  component to interface with the encoder hardware in a Lego NXT motor, a
--  pulse-width modulator (PWM) component for controlling the power applied
--  to the motor, and two discrete output components for controlling the
--  "polarity" of the power (thus the direction the motor rotates).
--
--  The encoder component requires a timer object and two GPIO points as
--  discrete inputs. The PWM component requires another timer to generate the
--  pulses, a timer channel, and a GPIO point attached to that channel for the
--  output. The two "polarity" discrete outputs require two GPIO points.
--
--  These components of a Motor object are set by an Initialize routine
--  for the type Motor. This initialization routine is called by procedure
--  NXT_Shield.Initialize_Hardware, once each for the two motor objects
--  (Motor1 and Motor2) declared in the spec of package NXT_Shield.
--
--  These calls to the Motor object's Initialize routine hard-code the choices
--  for the values passed to the Motor component parameters. This hard-coding
--  is done because the identification of a set of timers, timer channels,
--  and GPIO points that is technically appropriate, tied to a header on the
--  target board, and not already used for some other purpose is time-consuming
--  and relatively difficult. In particular, the STMicro timers have different
--  capabilities so not just any timer can be chosen for the encoder or PWM
--  generator. Similarly, not every GPIO point can be attached to every timer
--  channel. And since the targets boards we might be using are Discovery Kits
--  with limited headers, not every GPIO pin on the MCU is physically available
--  on the board. Once a correct timer and compatible GPIO points are found,
--  they may be in use already for some other part of the application.
--
--  The values passed to the Initialize routines are technicaly capable of
--  performing their function, have compatible GPIO points, and do not
--  conflict with each other.
--
--  Note, however, that PA0 is tied to the blue user button on (at least some
--  of) the STM Discovery Kits. (The F4 Discovery Kit is just such a board.) On
--  these boards YOU MUST DESOLDER A SOLDER BRIDGE ON THE BACK OF THE BOARD to
--  make PA0 available for other uses. Doing so, of course, renders the blue
--  user button useless. The solder bride to remove is labeled "SB20" and is
--  described in the "UM1472 Discovery Kit for STM32F407/417 Lines" document,
--  in Table 4.
--
--  The assignment of timers and GPIO points to Motor objects is as follows
--  (these are the actual calls and parameter associations):
--
--        Motor1.Initialize
--          (Encoder_Input1       => PA15,
--           Encoder_Input2       => PB3,
--           Encoder_Timer        => Timer_2'Access,
--           Encoder_AF           => GPIO_AF_TIM2_1,
--
--           PWM_Timer            => Timer_4'Access,
--           PWM_Output_Frequency => PWM_Frequency,
--           PWM_AF               => GPIO_AF_TIM4_2,
--           PWM_Output           => PB6,
--           PWM_Output_Channel   => Channel_1,
--
--           Polarity1            => PA10,
--           Polarity2            => PB1);
--
--
--        Motor2.Initialize
--          (Encoder_Input1       => PA0,
--           Encoder_Input2       => PA1,
--           Encoder_Timer        => Timer_5'Access,
--           Encoder_AF           => GPIO_AF_TIM5_2,
--
--           PWM_Timer            => Timer_3'Access,
--           PWM_Output_Frequency => PWM_Frequency,
--           PWM_AF               => GPIO_AF_TIM3_2,
--           PWM_Output           => PB4,
--           PWM_Output_Channel   => Channel_1,
--
--           Polarity1            => PA2,
--           Polarity2            => PA3);
--
--  The timers and GPIO points above should not be used for any other driver
--  interface, as long as this package is used in the same executable.

--        Sonar_Clock_Pin : GPIO_Point renames PB13;  -- SCL
--        Sonar_Data_Pin  : GPIO_Point renames PB11;  -- SDA
--  The choice of pins is largely arbitrary because we are bit-banging the
--  I/O instead of using an ob-board I2C device. Nonetheless, the internal
--  pull-up resistor values are not the same across all pins. See table 47
--  "I/O Static Characteristics" in the STM32F405xx STM32F407xx Datasheet.


with NXT.Motors;             use NXT.Motors;
with NXT.Ultrasonic_Sensors; use NXT.Ultrasonic_Sensors;

package NXT_Shield is
   pragma Elaborate_Body;

   Motor1 : Basic_Motor;
   Motor2 : Basic_Motor;

   Sonar : Ultrasonic_Sonar_Sensor (Hardware_Device_Address => 1);

   procedure Initialize_Hardware;

end NXT_Shield;
