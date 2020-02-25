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

--  Recursive Moving Average (RMA) filters for any floating point sample (ie input)
--  type

--  The moving average is the most common filter in DSP, used for reducing
--  random noise while retaining a sharp step response.

--  This implementation keeps a running total of the input values rather than
--  iterating over the current inputs each time a new input sample is inserted
--  (in order to compute the running average). However, eventual overflow is
--  possible, given sufficiently large input sample values and a sufficiently
--  long executon time. Choose the generic actual type for the type Accumulator
--  accordingly.

--  This design assums new input sample values are acquired one at a time,
--  and thus inserted into filter objects individually. These filter objects
--  maintain their own buffers, of the size specified by their discriminant.

with Sequential_Bounded_Buffers;

generic

   type Sample is digits <>;
   --  the type used for the input samples

   type Output is digits <>;
   --  the type used for the output average provided

   type Accumulator is range <>;
   --  the type used for the running total of inputs

package Recursive_Moving_Average_Filters_Reals is

   subtype Filter_Window_Size is Integer range 1 .. Integer'Last / 2;

   type RMA_Filter (Window_Size : Filter_Window_Size) is tagged limited private;

   procedure Insert (This : in out RMA_Filter;  New_Sample : Sample);

   function Value (This : RMA_Filter) return Output with Inline;

   procedure Reset (This : out RMA_Filter);

private

   package Sample_Data is new Sequential_Bounded_Buffers
     (Element => Sample, Default_Value => 0.0);
   use Sample_Data;

   type RMA_Filter (Window_Size : Filter_Window_Size) is tagged limited record
      Samples        : Sample_Data.Ring_Buffer (Capacity => Window_Size);
      Averaged_Value : Output := 0.0;
      Total          : Accumulator;
   end record;

end Recursive_Moving_Average_Filters_Reals;
