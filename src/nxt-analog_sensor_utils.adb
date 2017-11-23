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

package body NXT.Analog_Sensor_Utils is

   -------------------------
   -- Get_Average_Reading --
   -------------------------

   procedure Get_Average_Reading
     (Sensor     : in out NXT_Analog_Sensor'Class;
      Interval   : Time_Span;
      Result     : out Integer;
      Successful : out Boolean)
   is
      Deadline : constant Time := Clock + Interval;
      Reading  : Integer;
      Total    : Integer := 0;
      Count    : Integer := 0;
   begin
      while Clock <= Deadline loop
         Get_Raw_Reading (Sensor, Reading, Successful);
         if not Successful then
            Result := 0;
            return;
         end if;
         --  Reading is in range 0 .. ADC_Conversion_Max_Value, ie 0 .. 1023,
         --  so we are not likely to overflow for a reasonable interval, but
         --  it is possible...
         Total := Total + Reading;
         Count := Count + 1;
      end loop;
      Result := Total / Count;
   end Get_Average_Reading;

   -----------
   -- Image --
   -----------

   function Image (Status : Reading_Status) return String is
   begin
      case Status is
         when Valid_Reading   => return "Valid_Reading";
         when Reading_Failure => return "Reading_Failure";
      end case;
   end Image;

end NXT.Analog_Sensor_Utils;
