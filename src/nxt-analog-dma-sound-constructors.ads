package NXT.Analog.DMA.Sound.Constructors is

   function New_Sound_Sensor
     (Converter     : not null access Analog_To_Digital_Converter;
      Input_Channel : Analog_Input_Channel;
      Input_Pin     : GPIO_Point;
      Controller    : not null access DMA_Controller;
      Stream        : DMA_Stream_Selector;
      Digital_0     : GPIO_Point;
      Digital_1     : GPIO_Point)
   return NXT_Sound_Sensor;

end NXT.Analog.DMA.Sound.Constructors;
