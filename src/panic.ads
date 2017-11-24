with Ada.Real_Time; use Ada.Real_Time;

procedure Panic (Blink_Interval : Time_Span := Milliseconds (250)) with No_Return;
--  Flash the LEDs to indicate disaster, forever.


