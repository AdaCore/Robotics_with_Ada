------------------------------------------------------------------------------
--                                                                          --
--                      Copyright (C) 2018, AdaCore                         --
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
with Ada.Unchecked_Conversion;
with HAL;  use HAL;

package body HiTechnic.IR_Receivers is

   function As_Switch_Value is new Ada.Unchecked_Conversion
     (Source => UInt8, Target => Switch_Value);

   ------------------
   -- Get_Raw_Data --
   ------------------

   procedure Get_Raw_Data
     (This          : in out IR_Receiver;
      Data          :    out Raw_Sensor_Data;
      IO_Successful :    out Boolean)
   is
      Response  : Sequence (1 .. 8);
   begin
      This.Read_Register (Data_Registers, Response, IO_Successful);
      --  reading the one register will reply with multiple bytes in response
      if IO_Successful then
         Data.A (1) := As_Switch_Value (Response (1));
         Data.B (1) := As_Switch_Value (Response (2));
         Data.A (2) := As_Switch_Value (Response (3));
         Data.B (2) := As_Switch_Value (Response (4));
         Data.A (3) := As_Switch_Value (Response (5));
         Data.B (3) := As_Switch_Value (Response (6));
         Data.A (4) := As_Switch_Value (Response (7));
         Data.B (4) := As_Switch_Value (Response (8));
      else
         Data := (others => (others => 0));
      end if;
   end Get_Raw_Data;

end HiTechnic.IR_Receivers;
