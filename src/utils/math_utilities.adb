------------------------------------------------------------------------------
--                                                                          --
--                      Copyright (C) 2017, AdaCore                         --
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
--     3. Neither the name of STMicroelectronics nor the names of its       --
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

package body Math_Utilities is

   -----------------------------
   -- Range_To_Domain_Mapping --
   -----------------------------

   function Range_To_Domain_Mapping
     (Value, Range_Min, Range_Max, Domain_Min, Domain_Max : T)
   return T is
     ((Value - Range_Min) * (Domain_Max - Domain_Min) / (Range_Max - Range_Min) + Domain_Min);

   ---------------------------
   -- Bounded_Integer_Value --
   ---------------------------

   function Bounded_Integer_Value (Value, Low, High : T) return T is
     (if Value < Low then Low elsif Value > High then High else Value);

   -------------------------
   -- Bound_Integer_Value --
   -------------------------

   procedure Bound_Integer_Value (Value : in out T; Low, High : T) is
   begin
      if Value < Low then
         Value := Low;
      elsif Value > High then
         Value := High;
      end if;
   end Bound_Integer_Value;

   ----------------------------
   -- Bounded_Floating_Value --
   ----------------------------

   function Bounded_Floating_Value (Value, Low, High : T) return T is
     (if Value < Low then Low elsif Value > High then High else Value);

   --------------------------
   -- Bound_Floating_Value --
   --------------------------

   procedure Bound_Floating_Value (Value : in out T; Low, High : T) is
   begin
      if Value < Low then
         Value := Low;
      elsif Value > High then
         Value := High;
      end if;
   end Bound_Floating_Value;

end Math_Utilities;
