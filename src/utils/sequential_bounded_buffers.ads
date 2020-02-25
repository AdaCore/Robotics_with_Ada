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

--  This package provided sequential bounded ring buffers that have been proven
--  to SPARK Gold level.

generic
   type Element is private;
   Default_Value : Element;
package Sequential_Bounded_Buffers is

   pragma Unevaluated_Use_Of_Old (Allow);

   subtype Buffer_Size is Integer range 1 .. Integer'Last / 2;
   --  The possible sizes for buffer objects. The upper bound is constrained in
   --  order to prevent overflow.

   subtype Natural_Buffer_Size is Integer range 0 .. Buffer_Size'Last;
   --  Allows zero, used e.g., for the result type of function Extent

   type Ring_Buffer (Capacity : Buffer_Size) is private with
     Default_Initial_Condition => Empty (Ring_Buffer),
     Iterable => (First       => First_Index,
                  Next        => Next_Index,
                  Has_Element => Has_Element,
                  Element     => Value);

   overriding function "=" (Left, Right : Ring_Buffer) return Boolean;
   --  An replacement for predefined equality, this routine only compares the
   --  parts of Left and Right that are logically contained.

   procedure Copy (Source : Ring_Buffer; Target : in out Ring_Buffer) with
     Pre    => Target.Capacity >= Extent (Source),
     Post   => Target = Source,
     Global => null;
   --  An alternative to assignment, this routine only copies to Target that
   --  part of Source which is actually contained at the time of the call.

   procedure Put (This : in out Ring_Buffer; Item : Element) with
     Pre    => not Full (This),  -- these buffers don't overwrite
     Post   => not Empty (This)
               and then Extent (This) = Extent (This)'Old + 1
               and then Latest_Insertion (This) = Item
               --  the rest is unchanged
               and then Model (This'Old) & Item = Model (This),
     Global => null;

   procedure Get (This : in out Ring_Buffer; Item : out Element) with
     Pre    => not Empty (This),
     Post   => not Full (This)
               and Extent (This) = Extent (This)'Old - 1
               and Item = Next_Element_Out (This)'Old
               --  the rest is unchanged
               and Item & Model (This) = Model (This'Old)
               and Model (This) = Model (This'Old) (2 .. Extent (This'Old)),
     Global => null;

   function Next_Element_Out (This : Ring_Buffer) return Element with
     Pre    => not Empty (This),
     Global => null,
     Inline;
   --  Returns the Element value that would be removed by a subsequent call to
   --  Get

   function Empty (This : Ring_Buffer) return Boolean with
     Post   => Empty'Result = (Extent (This) = 0),
     Global => null,
     Inline;

   function Full (This : Ring_Buffer) return Boolean with
     Post   => Full'Result = (Extent (This) = This.Capacity),
     Global => null,
     Inline;

   function Extent (This : Ring_Buffer) return Natural_Buffer_Size with
     Global => null,
     Inline;

   procedure Reset (This : out Ring_Buffer) with
     Post   => Empty (This) and
               Extent (This) = 0 and
               Model (This)'Length = 0,
     Global => null;

   procedure Delete (This : in out Ring_Buffer;  Count : Buffer_Size) with
     Post   => not Full (This) and then
               Extent (This) = Extent (This)'Old - Integer'Min (Count, Extent (This)'Old) and then
               (if Count = Extent (This)'Old then Empty (This)) and then
               --  the rest is unchanged
               (for all K in 1 .. Extent (This) =>
                  Model (This) (K) = Model (This)'Old (K + Integer'Min (Count, Extent (This)'Old))) and then
               --  next out is...
               (if not Empty (This) then Next_Element_Out (This) = Model (This) (1)),
     Global => null;

   generic
      with procedure Action (Value : in Element);
   procedure Iteration (Over : Ring_Buffer);
   --  An alternative to direct iteration using "for of" syntax, less convenient
   --  but may have better performance

   --  Ghost functions  --------------------------------------------------------

   type Model_Data is array (Buffer_Size range <>) of Element with Ghost;

   function Model (This : Ring_Buffer) return Model_Data with
      Post => Model'Result'First = 1,
      Ghost;

   function Latest_Insertion (This : Ring_Buffer) return Element with
     Pre => not Empty (This),
     Ghost;

  --  Iterator functions  ------------------------------------------------------

  --  These functions are defined purely for iteration support and are not
  --  intended to be used by application code.

   function First_Index (This : Ring_Buffer) return Buffer_Size;

   function Next_Index (Unused : Ring_Buffer; Position : Buffer_Size) return Buffer_Size;

   function Has_Element (This : Ring_Buffer;  Position : Buffer_Size) return Boolean;

   function Value (This : Ring_Buffer; Position : Buffer_Size) return Element;

private

   type Element_Data is array (Buffer_Size range <>) of Element;

   type Ring_Buffer (Capacity : Buffer_Size) is record
      Content : Element_Data (1 .. Capacity) := (others => Default_Value);
      First   : Buffer_Size := 1;
      Length  : Natural_Buffer_Size := 0;
   end record with
     Predicate => First  in 1 .. Capacity and
                  Length in 0 .. Capacity;

   -----------
   -- Empty --
   -----------

   function Empty (This : Ring_Buffer) return Boolean is
     (This.Length = 0);

   ----------
   -- Full --
   ----------

   function Full (This : Ring_Buffer) return Boolean is
     (This.Length = This.Capacity);

   ------------
   -- Extent --
   ------------

   function Extent (This : Ring_Buffer) return Natural_Buffer_Size is
     (This.Length);

   ----------------------
   -- Next_Element_Out --
   ----------------------

   function Next_Element_Out (This : Ring_Buffer) return Element is
     (This.Content (This.First));

   --------------------
   -- Next_Out_Index --
   --------------------

   function Next_Out_Index
     (This   : Ring_Buffer;
      Offset : Natural_Buffer_Size)
     return Buffer_Size
   is
     (if This.First + Offset <= This.Capacity then
         This.First + Offset
      else -- wrapping around
         Offset - This.Capacity + This.First)
   with
     Pre  => Offset in 0 .. This.Capacity,
     Post => Next_Out_Index'Result in 1 .. This.Capacity;

   ---------
   -- "=" --
   ---------

   overriding function "=" (Left, Right : Ring_Buffer) return Boolean is
     (Left.Length = Right.Length and then
        (for all Offset in 0 .. Left.Length - 1 =>
            Left.Content (Next_Out_Index (Left, Offset)) = Right.Content (Next_Out_Index (Right, Offset))));

   ----------------------
   -- Latest_Insertion --
   ----------------------

   function Latest_Insertion (This : Ring_Buffer) return Element is
     (This.Content (Next_Out_Index (This, Offset => This.Length - 1)));

   --------------------------
   -- Content_Wraps_Around --
   --------------------------

   function Content_Wraps_Around (This : Ring_Buffer) return Boolean is
     (This.First + This.Length - 1 > This.Capacity);

   ----------------
   -- Last_Index --
   ----------------

   function Last_Index (This : Ring_Buffer) return Natural_Buffer_Size is
     (if not Content_Wraps_Around (This) then
         This.First + This.Length - 1
      else
         This.First + This.Length - 1 - This.Capacity);

   ----------------
   -- Normalized --
   ----------------

   function Normalized (List : Element_Data) return Element_Data with
     Post => Normalized'Result'Length = List'Length
             and then Normalized'Result'First = 1
             and then Normalized'Result'Last = List'Length
             and then List = Normalized'Result;
   --  Ensure result is a one-based array object

   -----------
   -- Model --
   -----------

   function Model (This : Ring_Buffer) return Model_Data is
     (if Content_Wraps_Around (This) then
         Model_Data (Normalized (This.Content (This.First .. This.Capacity)) &
                     Normalized (This.Content (1 .. Last_Index (This))))
      else
         Model_Data (Normalized (This.Content (This.First .. Last_Index (This)))));

   -----------------
   -- First_Index --
   -----------------

   function First_Index (This : Ring_Buffer) return Buffer_Size is
     (This.First);

   ----------------
   -- Next_Index --
   ----------------

   function Next_Index (Unused : Ring_Buffer; Position : Buffer_Size) return Buffer_Size is
     (if Position = Buffer_Size'Last then 1 else Position + 1);
     --  Position will never go past 2 * Capacity - 1 because of the way the
     --  potential wrap-around of the Content array works. Specifically, when
     --  wrapping around past Capacity, the last used index will go no further
     --  than First - 1. But the provers are concerned with overflow so we
     --  prevent that.

   -----------
   -- Value --
   -----------

   function Value (This : Ring_Buffer; Position : Buffer_Size) return Element is
     (if Position > This.Capacity then
        (if (Position mod This.Capacity = 0) then
           This.Content (Position mod This.Capacity + 1)
         else This.Content (Position mod This.Capacity))
      else
        This.Content (Position));
     --  Position will never go past 2 * Capacity - 1 because of the way the
     --  potential wrap-around of the Content array works. Specifically, when
     --  wrapping around past Capacity, the last used index will go no further
     --  than First - 1 so it will never reach Capacity the second time. Hence
     --  once Position is greater than Capacity, Position mod Capacity would be
     --  greater than zero and thus safe.
     --
     --  But the provers are concerned with array index checks so we handle
     --  it anyway. Note that we cannot use preconditions to say that Position
     --  will be in the range 1 .. 2 * Capacity - 1, although that would allow
     --  simplifying the function expression, because that precondition wouldn't
     --  be provable at the call sites in the "for of" iterations.

end Sequential_Bounded_Buffers;
