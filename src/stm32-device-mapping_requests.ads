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

--  This package provides device family-specific services

package STM32.Device.Mapping_Requests is

   Invalid_Mapping_Request : exception;

   function DMA2_ADC_Request_Mapping
     (ADC_Unit   : not null access Analog_To_Digital_Converter;
      DMA_Unit   : not null access DMA_Controller;
      DMA_Stream : DMA_Stream_Selector)
   return DMA_Channel_Selector
   with Pre => DMA_Unit = DMA_2'Access;
   --  Implements Table 43, the DMA2 Request Mapping table, for ADC units:
   --  given the specified DMA stream on DMA controller #2, returns the DMA
   --  channel that is mapped to the specified ADC unit on DMA controller #2.
   --
   --  Note that the DMA_Unit parameter is redundant, since this function only
   --  works for DMA2, i.e., only DMA2 can be mapped to ADC units on this
   --  device. However, we want to detect the potential mistake of calling
   --  this function for DMA1, and so we include the parameter for the sake of
   --  the precondition.

   --  Other mapping functions for tables 42 and 43, providing the mapping for
   --  various devices, such as USARTn_RX and USARTn_TX ...

end STM32.Device.Mapping_Requests;
