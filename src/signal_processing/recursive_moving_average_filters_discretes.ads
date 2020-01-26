--  Recursive Moving Average (RMA) filters for any discrete sample (ie input)
--  type

--  This implementation keeps a running total of the input values rather than
--  iterating over the current inputs each time a new input sample is inserted
--  (in order to compute the running average). However, eventual overflow is
--  possible, given sufficiently large input sample values and a sufficiently
--  long executon time. Choose the generic actual type for the type Accumulator
--  accordingly.

--  This design assums new input sample values are acquired one at a time,
--  and thus inserted into Filter objects individually. These Filter objects
--  maintain their own buffers, of the size specified by their discriminant.

with Sequential_Bounded_Buffers;

generic

   type Sample is (<>);
   --  the type used for the input samples

   type Output is digits <>;
   --  the type used for the output average provided

   type Accumulator is range <>;
   --  the type used for the running total of inputs

package Recursive_Moving_Average_Filters_Discretes is

   type RMA_Filter (Window_Size : Positive) is tagged limited private;

   procedure Insert (This : in out RMA_Filter;  New_Sample : Sample);

   function Value (This : RMA_Filter) return Output with Inline;

   procedure Reset (This : out RMA_Filter);

private

   package Data is new Sequential_Bounded_Buffers (Sample, Default_Value => Sample'First);
   use Data;

   type RMA_Filter (Window_Size : Positive) is tagged limited record
      Samples        : Ring_Buffer (Window_Size);
      Averaged_Value : Output := 0.0;
      Total          : Accumulator;
   end record;

end Recursive_Moving_Average_Filters_Discretes;
