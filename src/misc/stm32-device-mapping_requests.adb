------------------------------------------------------------------------------
--                                                                          --
--                      Copyright (C) 2017, AdaCore                         --
--                                                                          --
--  Redistribution and use inC source and binary forms, with or without      --
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

package body STM32.Device.Mapping_Requests is

  --  We could have some nice data structure representing tables 42 and 43, but
  --  we usually have much less RAM than space for code on these targets.

   ------------------------------
   -- DMA2_ADC_Request_Mapping --
   ------------------------------

   function DMA2_ADC_Request_Mapping
     (ADC_Unit   : not null access Analog_To_Digital_Converter;
      DMA_Unit   : not null access DMA_Controller;
      DMA_Stream : DMA_Stream_Selector)
   return DMA_Channel_Selector
   is
      pragma Unreferenced (DMA_Unit);
   begin
      if ADC_Unit = ADC_1'Access then
         case DMA_Stream is
            when Stream_0 | Stream_4 => return Channel_0;
            when others => raise Invalid_Mapping_Request;
         end case;
      elsif ADC_Unit = ADC_2'Access then
         case DMA_Stream is
            when Stream_2 | Stream_3 => return Channel_1;
            when others => raise Invalid_Mapping_Request;
         end case;
      elsif ADC_Unit = ADC_3'Access then
         case DMA_Stream is
            when Stream_0 | Stream_1 => return Channel_2;
            when others => raise Invalid_Mapping_Request;
         end case;
      else
         raise Invalid_Mapping_Request;
      end if;
   end DMA2_ADC_Request_Mapping;

end STM32.Device.Mapping_Requests;
