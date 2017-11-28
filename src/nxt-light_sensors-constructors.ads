with STM32.ADC;  use STM32.ADC;
with STM32.DMA;  use STM32.DMA;

package NXT.Light_Sensors.Constructors is

   function New_Light_Sensor
     (Converter      : not null access Analog_To_Digital_Converter;
      Input_Channel  : Analog_Input_Channel;
      Input_Pin      : GPIO_Point;
      Controller     : not null access DMA_Controller;
      Stream         : DMA_Stream_Selector;
      Floodlight_Pin : GPIO_Point)
   return NXT_Light_Sensor;

end NXT.Light_Sensors.Constructors;
