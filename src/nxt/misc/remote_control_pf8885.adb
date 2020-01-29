--  note this package is taken from another project so there are packages 
--  referenced that are only in that other project...

with Hardware_Configuration;
with Global_Initialization;
with Ada.Real_Time;  use Ada.Real_Time;
with STM32.Board;    use STM32.Board;
with HAL;            use HAL;
with HiTechnic.IR_Receivers; use HiTechnic.IR_Receivers;
with NXT.Digital;            use NXT.Digital;

package body Remote_Control_PF8885 is

   IR_Sensor_Address : constant I2C_Device_Address := 1;  --  unshifted!

   Receiver : IR_Receiver (Device_Hardware_Address => IR_Sensor_Address);

   Period : constant Time_Span := Milliseconds (System_Configuration.Remote_Control_Period);

   Current_Vector : Travel_Vector := (0, Forward, Emergency_Braking => False) with Volatile;

   Current_Steering_Target : Integer := 0 with Volatile;

   procedure Receive
     (Requested_Vector   : out Travel_Vector;
      Requested_Steering : out Integer);
   --  Get the requested control values from the IR Receiver sensor

   ----------------------
   -- Requested_Vector --
   ----------------------

   function Requested_Vector return Travel_Vector is
      (Current_Vector);

   ------------------------------
   -- Requested_Steering_Angle --
   ------------------------------

   function Requested_Steering_Angle return Integer is
      (Current_Steering_Target);

   ----------
   -- Pump --
   ----------

   task body Pump is
      Next_Release : Time;
   begin
      Global_Initialization.Completion.Wait;
      Next_Release := Clock;
      loop
         Receive (Current_Vector, Current_Steering_Target);
         Next_Release := Next_Release + Period;
         delay until Next_Release;
      end loop;
   end Pump;

   -------------
   -- Receive --
   -------------

   procedure Receive
     (Requested_Vector   : out Travel_Vector;
      Requested_Steering : out Integer)
   is
      Switches      : Raw_Sensor_Data;
      IO_Successful : Boolean;
      Power         : Integer;

      --  use with the LEGO Functions Power Functions IR TX 8885
      Momentary_Pressed_Forward  : constant := 100;
      Momentary_Pressed_Backward : constant := -100;
      --  note: the LEGO Functions Power Functions IR Speed Remote Control 8879
      --  will be different
   begin
      Receiver.Get_Raw_Data (Switches, IO_Successful);
      All_LEDs_Off;
      if IO_Successful then

         Requested_Vector.Direction := Forward;
         for Channel in Channel_Id loop

            --  show we recognized button press(es)
            if Switches.A (Channel) = Momentary_Pressed_Forward then
               Orange_LED.Set;
            end if;
            if Switches.A (Channel) = Momentary_Pressed_Backward then
               Blue_LED.Set;
            end if;
            if Switches.B (Channel) = Momentary_Pressed_Forward then
               Green_LED.Set;
            end if;
            if Switches.B (Channel) = Momentary_Pressed_Backward then
               Red_LED.Set;
            end if;

            --  capture backward direction request and change sign/value (for
            --  the 8855 remote)
            if Switches.B (Channel) = Momentary_Pressed_Backward then
               Requested_Vector.Direction := Backward;
               Switches.B (Channel) := 100;
            end if;
         end loop;

         --  Only one of Switches.B will be nonzero (assuming only one control
         --  used). We apply the scaling factors so that the channels can be
         --  used to control the percentage of power applied. Channel 1 selects
         --  30%, channel 2 selects 50%, and so on.
         Power := (3  * Integer (Switches.B (1)) +
                   5  * Integer (Switches.B (2)) +
                   7  * Integer (Switches.B (3)) +
                   10 * Integer (Switches.B (4))) / 10;
         Requested_Vector.Power := Integer'Min (100, Power);

         --  Compute the steering target. Only one of Switches.A will be
         --  nonzero (assuming only one control used).
         --
         --  The values with the 8855 remote will be either -156, 0, or +100
         --  but we have previously changed the -156 to -100 in the loop above.
         Requested_Steering := Integer (Switches.A (1)) +
                               Integer (Switches.A (2)) +
                               Integer (Switches.A (3)) +
                               Integer (Switches.A (4));

         --  With the 8885 remote we don't have anything other than a
         --  three-position toggle switch for the steering, so we arbitrarily
         --  use 30 degrees as the requested turn angle. Thus the values will
         --  be -30, 0, and +30 since Requested_Steering is 0, -100, or +100.
         Requested_Steering := (Requested_Steering * 30) / 100;

      else  -- IO failure
         Requested_Vector := (Power => 0, Direction => Forward, Emergency_Braking => False);
         Requested_Steering := 0;
      end if;
   end Receive;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
      use Hardware_Configuration;
   begin
      Receiver.Configure
        (Port        => Receiver_I2C_Port,
         SCL         => Receiver_I2C_Clock_Pin,
         SDA         => Receiver_I2C_Data_Pin,
         AF_Code     => Receiver_I2C_Port_AF,
         Clock_Speed => Lego_NXT_I2C_Frequency);
   end Initialize;

end Remote_Control_PF8885;
