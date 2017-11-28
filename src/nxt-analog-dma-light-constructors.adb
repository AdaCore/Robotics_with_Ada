package body NXT.Analog.DMA.Light.Constructors is

   ----------------------
   -- New_Light_Sensor --
   ----------------------

   function New_Light_Sensor
     (Converter      : not null access Analog_To_Digital_Converter;
      Input_Channel  : Analog_Input_Channel;
      Input_Pin      : GPIO_Point;
      Controller     : not null access DMA_Controller;
      Stream         : DMA_Stream_Selector;
      Floodlight_Pin : GPIO_Point)
   return NXT_Light_Sensor
   is
   begin
      return Result : NXT_Light_Sensor do
         Result.Converter      := Converter;
         Result.Input_Channel  := Input_Channel;
         Result.Input_Pin      := Input_Pin;
         Result.Controller     := Controller;
         Result.Stream         := Stream;
         Result.Floodlight_Pin := Floodlight_Pin;
      end return;
   end New_Light_Sensor;

end NXT.Analog.DMA.Light.Constructors;
