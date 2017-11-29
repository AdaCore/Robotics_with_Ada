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

procedure Poll_For_Continuous_State
  (This              : Input;
   Required_Interval : Time_Span)
is
   Interval_Start             : Time;
   Previously_In_Target_State : Boolean := False;
begin
   loop
      if In_Target_State (This) then
         --  We are in the desired target state. This may be a transition into
         --  that state, or we may have been in there for some time.
         if not Previously_In_Target_State then
            --  This is a transition into the target state so we take a new
            --  starting timestamp. Doing so on each transition into the target
            --  state ensures that we only exit when we've been in that state
            --  continuously.
            Interval_Start := Clock;
            Previously_In_Target_State := True;
         else
            --  We are in the target state and were there in the previous
            --  iteration too (if not more). Have we been in that state,
            --  continuously, long enough? The function Clock represents "now"
            --  so the difference between the Clock and the starting time is
            --  the elapsed time we have been in the target state.
            exit when Clock - Interval_Start >= Required_Interval;
         end if;
      else
         --  We are not in the target state, at least not now. We may have been
         --  in there an iteration ago, but the input could be bouncing. By
         --  the same token, we may not have been in the target state in the
         --  previous iteration either, but setting the variable to False again
         --  doesn't incur a significant expense.
         Previously_In_Target_State := False;
      end if;
   end loop;
end Poll_For_Continuous_State;
