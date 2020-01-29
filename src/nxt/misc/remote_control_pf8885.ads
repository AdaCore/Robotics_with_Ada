--  This package body implements the remote control package spec using a
--  Lego infrared receiver and a specific IR control pad using a matching
--  IR transmitter. The control pad is a Lego Power Functions 8885 with two
--  momentary switches.
--
--  When the sensor is working as expected, these are the only possible values
--  for the data. Other controllers may provide other values.
--
--     Momentary_Pressed_Forward  : constant := +100;
--     Momentary_Pressed_Backward : constant := -100;
--     Momentary_Not_Pressed      : constant := 0;

--  see https://www.lego.com/en-us/themes/power-functions/products/ir-tx-8885with System_Configuration;

package Remote_Control_PF8885 is

   pragma Elaborate_Body;

   subtype Percentage is Integer range 0 .. 100;

   type Travel_Directions is (Forward, Backward, Neither);

   type Travel_Vector is record
      Power             : Percentage;
      Direction         : Travel_Directions;
      Emergency_Braking : Boolean := False;
   end record
   with Size => 32, Atomic;
   --  The size is bigger than absolutely necessary but we fit the components
   --  to bytes for the sake of efficiency. The main concern is that the size
   --  remain such that objects of the record type can be atomically accessed
   --  because we are just using shared variables rather than protecting them
   --  with a protected object.

   for Travel_Vector use record
      Power             at 0 range 0 .. 7;
      Direction         at 0 range 8 .. 15;
      Emergency_Braking at 0 range 16 .. 23;
   end record;

   function Requested_Vector return Travel_Vector with Inline;

   function Requested_Steering_Angle return Integer with Inline;
   --  The units are angles, positive or negative, relative to the major axis of
   --  the vehicle, which is angle zero.

private

   task Pump with
      Storage_Size => 1 * 1024,
      Priority     => System_Configuration.Remote_Priority;

end Remote_Control_PF8885;
