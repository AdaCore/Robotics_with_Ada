package body Sequential_Bounded_Buffers is

   ---------
   -- Put --
   ---------

   procedure Put (This : in out Ring_Buffer; Item : Element) is
   begin
      if This.First <= This.Capacity - This.Length then
         This.Content (This.First + This.Length) := Item;
      else -- wrapped around
         This.Content (This.Length - This.Capacity + This.First) := Item;
      end if;
      This.Length := This.Length + 1;
   end Put;

   ---------
   -- Get --
   ---------

   procedure Get (This : in out Ring_Buffer; Item : out Element) is
   begin
      Item := This.Content (This.First);
      This.Length := This.Length - 1;
      if This.First < This.Capacity then
         This.First := This.First + 1;
      else
         This.First := 1;
      end if;
   end Get;

   ----------
   -- Copy --
   ----------

   procedure Copy (Source : Ring_Buffer; Target : in out Ring_Buffer) is
   begin
      Target.Length := Source.Length;
      Target.First := 1;

      for J in 1 .. Source.Length loop
         Target.Content (J) := Source.Content (Next_Out_Index (Source, J - 1));

         pragma Loop_Invariant
           (for all K in 1 .. J =>
              Target.Content (K) = Source.Content (Next_Out_Index (Source, K - 1)));
      end loop;
   end Copy;

   ------------
   -- Delete --
   ------------

   procedure Delete (This : in out Ring_Buffer;  Count : Buffer_Size) is
      Actual_Deletions : constant Integer := Integer'Min (Count, This.Length);
   begin
      This.First := Next_Out_Index (This, Actual_Deletions);
      This.Length := This.Length - Actual_Deletions;
   end Delete;

   ---------------
   -- Iteration --
   ---------------

   procedure Iteration (Over : Ring_Buffer) is
   begin
      for K in 1 .. Over.Length loop
         Action (Over.Content (Next_Out_Index (Over, Offset => K - 1)));
      end loop;
   end Iteration;

   -----------
   -- Reset --
   -----------

   procedure Reset (This : out Ring_Buffer) is
   begin
--        This := (This.Capacity, First => 1, Length => 0, Content => (others => Default_Value));
--  individual assignments for sake of bug in compiler
      This.First := 1;
      This.Length := 0;
      This.Content := (others => Default_Value);
   end Reset;

   -----------------
   -- Has_Element --
   -----------------

   function Has_Element (This : Ring_Buffer;  Position : Buffer_Size) return Boolean is
      Result           : Boolean;
      Pos              : Buffer_Size := Position;
      Iterator_Wrapped : Boolean := False;
   begin
      if Position > This.Capacity then
         if Position mod This.Capacity = 0 then
            Pos := Position mod This.Capacity + 1;
         else
            Pos := Position mod This.Capacity;
         end if;
         Iterator_Wrapped := True;
      end if;
         --  Note: we don't use Last_Index in the following because it must also
         --  check whether the data wraps around, which would waste cycles
      if not Content_Wraps_Around (This) then
         Result := Pos in This.First .. This.First + This.Length - 1;
      else
         --  we check the "upper part" of First .. Capacity unless we have
         --  already been there in this iteration
         if not Iterator_Wrapped then
            Result := Pos in This.First .. This.Capacity;
         else -- we check the "lower part" up to but not including First
            Result := Pos in 1 .. This.Length - This.Capacity + This.First - 1;
         end if;
      end if;
      return Result;
   end Has_Element;
   --  Position will never go past 2 * Capacity - 1 because when wrapping around
   --  past Capacity, the last used index will go no further than First - 1.
   --  Hence once Position is greater than Capacity, Position mod Capacity
   --  would be greater than zero and thus safe.
   --
   --  But the provers are concerned with range checks so we handle
   --  it anyway.

   ----------------
   -- Normalized --
   ----------------

   function Normalized (List : Element_Data) return Element_Data is
    --  This is a function instead of a subtype because we need it in a
    --  postcondition as well as the "&" subprogram body, and we cannot
    --  define subtypes in postconditions.
      Result : constant Element_Data (1 .. List'Length) := List;
   begin
      return Result;
   end Normalized;

end Sequential_Bounded_Buffers;
