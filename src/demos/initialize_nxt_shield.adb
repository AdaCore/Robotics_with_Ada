with NXT_Shield;   use NXT_Shield;
with STM32.Device; use STM32.Device;
with STM32;        use STM32;
with STM32.GPIO;   use STM32.GPIO;
with STM32.Timers; use STM32.Timers;

with Hardware_Configuration; use Hardware_Configuration;

procedure Initialize_NXT_Shield is
   Successful      : Boolean;
begin
   Motor1.Initialize
     (Encoder_Input1       => Motor1_Encoder_Input1,
      Encoder_Input2       => Motor1_Encoder_Input2,
      Encoder_Timer        => Motor1_Encoder_Timer,
      Encoder_AF           => Motor1_Encoder_AF,
      PWM_Timer            => Motor1_PWM_Timer,
      PWM_Output_Frequency => Motor_PWM_Frequency,
      PWM_AF               => Motor1_PWM_AF,
      PWM_Output           => Motor1_PWM_Output,
      PWM_Output_Channel   => Motor1_PWM_Output_Channel,
      Polarity1            => Motor1_Polarity1,
      Polarity2            => Motor1_Polarity2);

   Motor2.Initialize
     (Encoder_Input1       => Motor2_Encoder_Input1,
      Encoder_Input2       => Motor2_Encoder_Input2,
      Encoder_Timer        => Motor2_Encoder_Timer,
      Encoder_AF           => Motor2_Encoder_AF,
      PWM_Timer            => Motor2_PWM_Timer,
      PWM_Output_Frequency => Motor_PWM_Frequency,
      PWM_AF               => Motor2_PWM_AF,
      PWM_Output           => Motor2_PWM_Output,
      PWM_Output_Channel   => Motor2_PWM_Output_Channel,
      Polarity1            => Motor2_Polarity1,
      Polarity2            => Motor2_Polarity2);

   Sonar.Configure
     (Data_Line       => Sonar_Data_Pin,
      Clock_Line      => Sonar_Clock_Pin,
      Clock_Frequency => Sonar_Clock_Frequency,
      Success         => Successful);
   if not Successful then
      raise Program_Error with "Sonar init";
   end if;
end Initialize_NXT_Shield;
