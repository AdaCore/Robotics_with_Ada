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

--  This package provides useful math utility routines.

package Math_Utilities is

   generic
      type T is range <>;
   function Range_To_Domain_Mapping
     (Value, Range_Min, Range_Max, Domain_Min, Domain_Max : T)
      return T
   with
     Pre  => Range_Min < Range_Max and Domain_Min < Domain_Max,
     Post => Range_To_Domain_Mapping'Result in Domain_Min .. Domain_Max,
     Inline;
   --  Maps the Value, with the range Range_Min .. Range_Max, to a value in the
   --  domain Domain_Min .. Domain_Max

   generic
      type T is range <>;
   function Bounded_Integer_Value
     (Value, Low, High : T)
      return T
   with
     Pre  => Low < High,
     Post => Bounded_Integer_Value'Result in Low .. High,
     Inline;
   --  Constrains the input value to the range Low .. High

   generic
      type T is digits <>;
   function Bounded_Floating_Value
     (Value, Low, High : T)
      return T
   with
     Pre  => Low < High,
     Post => Bounded_Floating_Value'Result in Low .. High,
     Inline;
   --  Constrains the input value to the range Low .. High

end Math_Utilities;
