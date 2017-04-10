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

with STM32.GPIO;    use STM32.GPIO;
with Ada.Real_Time; use Ada.Real_Time;

package Discrete_Inputs is

   type Logic_Levels is (Low, High);

   type Discrete_Input (Pin : access GPIO_Point; Active : Logic_Levels) is
     tagged private;
   --  If Active is High, a non-zero voltage will be read on Pin when the input
   --  is considered "active". The GPIO pin will read as being "set" in that
   --  case. In contrast, when Active is Low, the input is active when there is
   --  no voltage present on Pin. The GPIO pin will read as not "set" in that
   --  case. Whether Active should be specified as High or Low depends on the
   --  electronic circuit connected to the pin.

   procedure Initialize_Hardware (This : in out Discrete_Input);
   --  To be called prior to use.  Enables the GPIO pin etc.

   function Active_Indicated (This : Discrete_Input) return Boolean is
     ((This.Active = High and then This.Pin.Set) or else
      (This.Active = Low and then not This.Pin.Set))
     with Inline;
   --  This routine reflects whether the pin is currently active, based on
   --  the corresponding logic level. Note this result is not debounced! The
   --  returned value can fluctuate over successive calls as the pin's input
   --  voltage changes, for example due to a button attached to the input being
   --  pressed or released. Therefore the pin is not necessarily active or
   --  inactive solely based upon what this function returns, hence the name.
   --  We consider it truly (in)active only if debounced: when the pin remains
   --  in one state or the other over some "debounce time interval" applied. We
   --  make the routine visible for the sake of users writing other routines,
   --  e.g., interrupt handlers, that need to interrogate the status of inputs.

   function Inactive_Indicated (This : Discrete_Input) return Boolean is
     (not Active_Indicated (This))
     with Inline;
   --  Exactly as above, but for the inactive state.

   Default_Debounce_Time : Time_Span := Milliseconds (75);
   --  The default amount of time used to debounce an input pin. It is passed
   --  to calls to the routines below as the default initial value, so changing
   --  this value at run-time is a convenient way to change the default
   --  globally. Note that any individual call can specify an actual for
   --  the parameter so any call can override this default.
   --
   --  This value is tunable. Too large a value will reduce the ability to
   --  detect events (e.g., button presses) that occur quickly. Too small a
   --  value will prevent the debouncing logic from working.

   procedure Await_Active
     (This          : Discrete_Input;
      Debounce_Time : Time_Span := Default_Debounce_Time);
   --  Wait until Active_Indicated is True for at least the interval of
   --  Debounce_Time. Note that it uses polling.

   procedure Await_Inactive
     (This          : Discrete_Input;
      Debounce_Time : Time_Span := Default_Debounce_Time);
   --  Wait until Active_Indicated is False for at least the interval of
   --  Debounce_Time. Note that it uses polling.

private

   type Discrete_Input (Pin : access GPIO_Point; Active : Logic_Levels) is
     tagged null record;

end Discrete_Inputs;
