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

--  This package provides an interface to a quadrature motor encoder (so
--  stricly speaking is is a decoder). It uses the specific capabilities of
--  selected ST Micro timers to perform this function, thereby relieving the
--  MCU of having to do so (eg via interrupts). The timer essentially acts as
--  an externally clocked counter, driven by the two discrete inputs. All that
--  clients must do, after initialization, is query the counter value. The
--  counter automatically follows the speed and direction of the motor. See
--  especially Application Note AN4013 (DM00042534). See also the RM (RM0090),
--  section 18.3.12 for discussion.
--
--  Note that the encoder count is provided as a value of type UInt32, ie 32
--  bits, but the value may only be 16-bits wide, depending on the timer
--  selected when calling Initialize.

with STM32.GPIO;   use STM32.GPIO;
with STM32.Timers; use STM32.Timers;
with STM32;        use STM32;
with HAL;          use HAL;

package Quadrature_Encoders is
   pragma Elaborate_Body;

   type Rotary_Encoder is limited private;

   function Current_Count (This : Rotary_Encoder) return UInt32
     with Inline;
   --  We ensure that 32 bits are actually provided by the timer via the
   --  precondition on the initialization routine.

   procedure Reset_Count (This : in out Rotary_Encoder) with
     Inline,
     Post => Current_Count (This) = 0;

   type Counting_Direction is (Up, Down);

   function Current_Direction (This : Rotary_Encoder) return Counting_Direction
     with Inline;

   procedure Initialize_Encoder
     (This          : in out Rotary_Encoder;
      Encoder_TI1   : GPIO_Point;  -- timer input discrete #1
      Encoder_TI2   : GPIO_Point;  -- timer input discrete #2
      Encoder_Timer : not null access Timer;
      Encoder_AF    : GPIO_Alternate_Function)
     with
       Pre  => Has_32bit_Counter (Encoder_Timer.all) and
               Bidirectional (Encoder_Timer.all),
       Post => Current_Count (This) = 0 and
               Current_Direction (This) = Up;
   --  Note that the encoder always uses channels 1 and 2 on the specified
   --  timer for Encoder_TI1 and Encoder_TI2, the two timer input discretes.

   function Bidirectional (This : Timer) return Boolean;
   --  The selected timer must be able to count both up and down, so not all
   --  are candidates. Only Timers 1..5 and 8 are bidirectional, per the F429
   --  Datasheet, Table 6, pg 33.
   --
   --  TODO: make this board-independent (move to STM32.Device package?)

private

   type Rotary_Encoder is access all Timer with Storage_Size => 0;

end Quadrature_Encoders;
