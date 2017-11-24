package body NXT.Analog.DMA.Sound.Constructors is

   ----------------------
   -- New_Light_Sensor --
   ----------------------

   function New_Sound_Sensor
     (Converter     : not null access Analog_To_Digital_Converter;
      Input_Channel : Analog_Input_Channel;
      Input_Pin     : GPIO_Point;
      Controller    : not null access DMA_Controller;
      Stream        : DMA_Stream_Selector;
      Digital_0     : GPIO_Point;
      Digital_1     : GPIO_Point)
   return NXT_Sound_Sensor
   is
   begin
      return Result : NXT_Sound_Sensor do
         Result.Converter     := Converter;
         Result.Input_Channel := Input_Channel;
         Result.Input_Pin     := Input_Pin;
         Result.Controller    := Controller;
         Result.Stream        := Stream;
         Result.Digital_0     := Digital_0;
         Result.Digital_1     := Digital_1;

         Initialize (Result);
      end return;
   end New_Sound_Sensor;

end NXT.Analog.DMA.Sound.Constructors;
