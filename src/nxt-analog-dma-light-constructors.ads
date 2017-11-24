package NXT.Analog.DMA.Light.Constructors is

   function New_Light_Sensor
     (Converter      : not null access Analog_To_Digital_Converter;
      Input_Channel  : Analog_Input_Channel;
      Input_Pin      : GPIO_Point;
      Controller     : not null access DMA_Controller;
      Stream         : DMA_Stream_Selector;
      Floodlight_Pin : GPIO_Point)
   return NXT_Light_Sensor;

end NXT.Analog.DMA.Light.Constructors;
