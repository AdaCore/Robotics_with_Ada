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

package body Simple_Moving_Average_Filters_Reals is

   function New_Average (This : SMA_Filter) return Output;

   -----------
   -- Value --
   -----------

   function Value (This : SMA_Filter) return Output is
     (This.MA_Value);

   ------------
   -- Insert --
   ------------

   procedure Insert (This : in out SMA_Filter;  New_Sample : Sample) is
   begin
      Put (This.Samples, New_Sample);
      This.MA_Value := New_Average (This);
   end Insert;

   -----------------
   -- New_Average --
   -----------------

   function New_Average (This : SMA_Filter) return Output is
      Result : Output := 0.0;
   begin
      for Value of This.Samples loop
         Result := Result + Output (Value);
      end loop;

      if Extent (This.Samples) > 1 then
         Result := Result / Output (Extent (This.Samples));
      end if;
      return Result;
   end New_Average;

   -----------
   -- Reset --
   -----------

   procedure Reset (This : in out SMA_Filter) is
   begin
      Reset (This.Samples);
      This.MA_Value := 0.0;
   end Reset;

end Simple_Moving_Average_Filters_Reals;
