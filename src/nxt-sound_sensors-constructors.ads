with STM32.ADC;  use STM32.ADC;
with STM32.DMA;  use STM32.DMA;

package NXT.Sound_Sensors.Constructors is

   function New_Sound_Sensor
     (Converter     : not null access Analog_To_Digital_Converter;
      Input_Channel : Analog_Input_Channel;
      Input_Pin     : GPIO_Point;
      Controller    : not null access DMA_Controller;
      Stream        : DMA_Stream_Selector;
      Mode_Pin_0    : GPIO_Point;
      Mode_Pin_1    : GPIO_Point)
   return NXT_Sound_Sensor;

end NXT.Sound_Sensors.Constructors;
