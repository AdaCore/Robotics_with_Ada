--  Recursive Moving Average (RMA) filters for any integer sample (ie input)
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

   type Sample is range <>;
   --  the type used for the input samples

   type Accumulator is range <>;
   --  the type used for the running total of inputs

package Recursive_Moving_Average_Filters_Discretes is

   pragma Compile_Time_Error
      (Sample'Size * 2 > Accumulator'Size,
       "Accumulator size should be twice that of Sample");

   pragma Compile_Time_Error
      (Sample'First >= Sample'Last or
       Accumulator'First >= Accumulator'Last,
       "Accumulator and Sample ranges must not be null");

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
