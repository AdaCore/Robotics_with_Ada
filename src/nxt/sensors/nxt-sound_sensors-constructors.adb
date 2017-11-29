package body NXT.Sound_Sensors.Constructors is

   ----------------------
   -- New_Light_Sensor --
   ----------------------

   function New_Sound_Sensor
     (Converter     : not null access Analog_To_Digital_Converter;
      Input_Channel : Analog_Input_Channel;
      Input_Pin     : GPIO_Point;
      Controller    : not null access DMA_Controller;
      Stream        : DMA_Stream_Selector;
      Mode_Pin_0    : GPIO_Point;
      Mode_Pin_1    : GPIO_Point)
   return NXT_Sound_Sensor
   is
   begin
      return Result : NXT_Sound_Sensor do
         Result.Assign_ADC (Converter, Input_Channel, Input_Pin);
         Result.Assign_DMA (Controller, Stream);
         Result.Mode_Pin_0 := Mode_Pin_0;
         Result.Mode_Pin_1 := Mode_Pin_1;
      end return;
   end New_Sound_Sensor;

end NXT.Sound_Sensors.Constructors;
