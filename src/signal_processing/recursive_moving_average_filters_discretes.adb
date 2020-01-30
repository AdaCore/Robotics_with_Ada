package body Recursive_Moving_Average_Filters_Discretes is

   -----------
   -- Value --
   -----------

   function Value (This : RMA_Filter) return Sample is
     (This.Averaged_Value);

   ------------
   -- Insert --
   ------------

   procedure Insert (This : in out RMA_Filter;  New_Sample : Sample) is
   begin
      if Empty (This.Samples) then
         Put (This.Samples, New_Sample);
         This.Total := Accumulator (New_Sample);
         This.Averaged_Value := Sample (This.Total);
         return;
      end if;

      if Full (This.Samples) then -- delete the oldest sample and update total
         declare
            Oldest : Sample;
         begin
            Get (This.Samples, Oldest);
            This.Total := This.Total - Accumulator (Oldest);
         end;
      end if;

      Put (This.Samples, New_Sample);

      This.Total := This.Total + Accumulator (New_Sample);
      --  The above potentially overflows, but there is no issue of accumulating
      --  round-off errors over time, unlike what would happen if we used a
      --  floating point type.

      This.Averaged_Value := Sample (This.Total / Accumulator (Extent (This.Samples)));
   end Insert;

   -----------
   -- Reset --
   -----------

   procedure Reset (This : out RMA_Filter) is
   begin
      Reset (This.Samples);
      This.Averaged_Value := 0;
      This.Total := 0;
   end Reset;

end Recursive_Moving_Average_Filters_Discretes;
