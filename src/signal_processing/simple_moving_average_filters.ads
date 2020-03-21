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

--  This implementation computes the sum of the current inputs each time a new
--  input is inserted (in order to compute the running average), so the cost is
--  paid per call to insert a new sample, but it should be able to run "forever"
--  without concern for eventual overflow because a total of the entire history
--  of inputs is not maintained (which would be the alternative to summing the
--  values each time, per the Recursive Moving Average design).

with Sequential_Bounded_Buffers;

generic

   type Sample is private;
   --  the type used for the input samples

   Default_Sample_Value : Sample;
   --  an arbitrary value used for internal initialization, but nothing else

   type Output is digits <>;
   --  the type used for the output average provided

   with function As_Output (Input : Sample) return Output;
   --  a conversion routne from Sample input value to the Output type

package Simple_Moving_Average_Filters is

   type SMA_Filter (Window_Size : Positive) is tagged limited private;

   procedure Insert (This : in out SMA_Filter;  New_Sample : Sample);

   function Value (This : SMA_Filter) return Output with Inline;

   procedure Reset (This : in out SMA_Filter);

private

   package Sample_Data is new Sequential_Bounded_Buffers
     (Element => Sample, Default_Value => Default_Sample_Value);
   use Sample_Data;

   type SMA_Filter (Window_Size : Positive) is tagged limited record
      Samples  : Sample_Data.Ring_Buffer (Window_Size);
      MA_Value : Output := 0.0;
   end record;

end Simple_Moving_Average_Filters;
