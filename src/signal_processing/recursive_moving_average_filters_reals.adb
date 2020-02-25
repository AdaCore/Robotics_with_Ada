------------------------------------------------------------------------------
--                                                                          --
--                     Copyright (C) 2020, AdaCore                          --
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

package body Recursive_Moving_Average_Filters_Reals is

   -----------
   -- Value --
   -----------

   function Value (This : RMA_Filter) return Output is
     (This.Averaged_Value);

   ------------
   -- Insert --
   ------------

   procedure Insert (This : in out RMA_Filter;  New_Sample : Sample) is
   begin
      if Empty (This.Samples) then
         Put (This.Samples, New_Sample);
         This.Total := Accumulator (New_Sample);
         This.Averaged_Value := New_Sample;
         return;
      end if;

      if Full (This.Samples) then -- delete the oldest sample
         --  TODO: why not just call Get???
         This.Total := This.Total - Accumulator (Next_Element_Out (This.Samples));
         Delete (This.Samples, Count => 1);
      end if;

      Put (This.Samples, New_Sample);

      This.Total := This.Total + Accumulator (New_Sample);
      --  The above potentially overflows, but there is no issue of accumulating
      --  round-off errors over time, unlike what would happen if we used a
      --  floating point type.

      This.Averaged_Value := Output (This.Total) / Output (Extent (This.Samples));
   end Insert;

   -----------
   -- Reset --
   -----------

   procedure Reset (This : out RMA_Filter) is
   begin
      Reset (This.Samples);
      This.Averaged_Value := 0.0;
      This.Total := 0;
   end Reset;

end Recursive_Moving_Average_Filters_Reals;
