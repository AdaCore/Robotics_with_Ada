package body NXT.Light_Sensors.Constructors is

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
         Result.Assign_ADC (Converter, Input_Channel, Input_Pin);
         Result.Assign_DMA (Controller, Stream);
         Result.Floodlight_Pin := Floodlight_Pin;
      end return;
   end New_Light_Sensor;

end NXT.Light_Sensors.Constructors;
