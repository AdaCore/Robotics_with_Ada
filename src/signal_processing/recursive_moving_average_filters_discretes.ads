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

--  Recursive Moving Average (RMA) filters for any integer sample (ie input and
--  output) type.

--  This implementation keeps a running total of the input values rather
--  than iterating over the current inputs each time a new sample is inserted
--  (in order to compute the running average). In cases that would lead to
--  overflow,the total is limited to Accumulator'First or Accumulator'Last.
--  The average returned from function Value is limited to Sample'First or
--  Sample'Last.

--  This design assums new input sample values are acquired one at a time, and
--  thus inserted into RMA_Filter objects individually. These RMA_Filter objects
--  maintain their own buffers, of the size specified by their discriminant.

with Sequential_Bounded_Buffers;

generic

   type Sample is range <>;
   --  The type used for the input samples and output averages.

   type Accumulator is range <>;
   --  The type used for the running total of inputs. The intent is that this
   --  type has a larger range than that of type Sample, so that a larger total
   --  can be accomodated.

   --  For both types, null ranges are not allowed. We check that with the
   --  Compile_Time_Error pragmas below.

package Recursive_Moving_Average_Filters_Discretes is

   pragma Compile_Time_Error
      (Sample'First > Sample'Last,
       "Sample range must not be null");

   pragma Compile_Time_Error
      (Accumulator'First > Accumulator'Last,
       "Accumulator range must not be null");

   subtype Filter_Window_Size is Integer range 1 .. Integer'Last / 2;

   type RMA_Filter (Window_Size : Filter_Window_Size) is tagged limited private;

   procedure Insert (This : in out RMA_Filter;  New_Sample : Sample);
   --  Updates the new average value based on the value of New_Sample

   function Value (This : RMA_Filter) return Sample with Inline;
   --  simply returns the average value previously computed by Insert

   procedure Reset (This : out RMA_Filter) with
     Post'Class => Value (This) = 0;

private

   package Sample_Data is new Sequential_Bounded_Buffers (Element => Sample, Default_Value => 0);
   use Sample_Data;

   type RMA_Filter (Window_Size : Filter_Window_Size) is tagged limited record
      Samples        : Sample_Data.Ring_Buffer (Window_Size);
      Averaged_Value : Sample := 0;
      Total          : Accumulator := 0;
      --  There is no issue of accumulating round-off errors over time, unlike
      --  what would happen if we used a floating point type for the Total
   end record;

end Recursive_Moving_Average_Filters_Discretes;
