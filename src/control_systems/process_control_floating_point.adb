------------------------------------------------------------------------------
--                                                                          --
--                     Copyright (C) 2020, AdaCore                          --
--                                                                          --
--  Redistribution and use in source and binary forms, with or without      --
--  modification, are permitted provided that the following conditions are  --
--  met:                                                                    --
--     1. Redistributions of source code must retain the above copyright    --
--        notice, this list of conditions and the following disclaimer.     --
--     2. Redistributions in binary form must reproduce the above copyright --
--        notice, this list of conditions and the following disclaimer in   --
--        the documentation and/or other materials provided with the        --
--        distribution.                                                     --
--     3. Neither the name of the copyright holder nor the names of its     --
--        contributors may be used to endorse or promote products derived   --
--        from this software without specific prior written permission.     --
--                                                                          --
--   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS    --
--   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT      --
--   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR  --
--   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT   --
--   HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, --
--   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT       --
--   LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,  --
--   DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY  --
--   THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT    --
--   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE  --
--   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.   --
--                                                                          --
------------------------------------------------------------------------------

package body Process_Control_Floating_Point with
  SPARK_Mode
is

   Max_Real : constant Long_Real := Long_Real (Real'Last);
   Min_Real : constant Long_Real := Long_Real (Real'First);

   -----------------------
   -- Constrain_To_Real --
   -----------------------

   procedure Constrain_To_Real (Value : in out Long_Real) with
     Post => Value in Min_Real .. Max_Real,
     Inline;

   ---------------
   -- Constrain --
   ---------------

   procedure Constrain (Value : in out Real;  Limits : Bounds) with
     Post => Value in Limits.Min .. Limits.Max,
     Inline;

   ----------
   -- Tune --
   ----------

   procedure Tune
     (This              : in out PID_Controller;
      Proportional_Gain : Real;
      Integral_Gain     : Real;
      Derivative_Gain   : Real;
      Period            : Positive_Milliseconds;
      Direction         : Controller_Directions)
   with
     Post => Specified_Kp (This) = Proportional_Gain  and
             Specified_Ki (This) = Integral_Gain      and
             Specified_Kd (This) = Derivative_Gain    and
             Current_Direction (This) = Current_Direction (This)'Old and
             Current_Output_Limits (This) = Current_Output_Limits (This)'Old and
             Enabled (This) = Enabled (This)'Old;
   --  Sets the PID gain values within This controller based on the Period at
   --  which the controller will compute the output and the Direction that the
   --  controller works. Called by Configure and the externally visible tuning
   --  routine (Reconfigure_Gain_Parameters). This is an internal routine
   --  so that all inputs are explicit parameters, rather than relying on
   --  components of This as the visible procedure does, which would require
   --  them to be set prior to calling the external version within Configure.
   --  That would be too delicate by half.

   ---------------
   -- Configure --
   ---------------

   procedure Configure
     (This              : in out PID_Controller;
      Proportional_Gain : Real;
      Integral_Gain     : Real;
      Derivative_Gain   : Real;
      Period            : Positive_Milliseconds;
      Output_Limits     : Bounds;
      Direction         : Controller_Directions := Direct)
   is
   begin
      This.Enabled := False;
      This.Period := Period;
      This.Output_Limits := Output_Limits;
      This.Direction := Direction;
      Tune (This, Proportional_Gain, Integral_Gain, Derivative_Gain, Period, Direction);
   end Configure;

   --------------------
   -- Compute_Output --
   --------------------

   procedure Compute_Output
     (This             : in out PID_Controller;
      Process_Variable : Real;
      Setpoint         : Real;
      Control_Variable : in out Real)
   is
      Error        : Long_Real;
      Input_Change : Long_Real;
      New_Output   : Long_Real;
   begin
      if not This.Enabled then
         return;
      end if;
      Error := Long_Real (Setpoint) - Long_Real (Process_Variable);
      Input_Change := Long_Real (Process_Variable) - Long_Real (This.Previous_Input);
      This.I_Term := This.I_Term + (Long_Real (This.Ki) * Error);
      Constrain (Real (This.I_Term), This.Output_Limits);
      New_Output := (Long_Real (This.Kp) * Error) + This.I_Term - (Long_Real (This.Kd) * Input_Change);
      Constrain (Real (New_Output), This.Output_Limits);
      Control_Variable := Real (New_Output);
      This.Previous_Input := Process_Variable;
   end Compute_Output;

   ----------
   -- Tune --
   ----------

   procedure Tune
     (This              : in out PID_Controller;
      Proportional_Gain : Real;
      Integral_Gain     : Real;
      Derivative_Gain   : Real;
      Period            : Positive_Milliseconds;
      Direction         : Controller_Directions)
   is
      Sample_Time_In_Sec : constant Long_Real := Long_Real (Period) / 1000.0;
      Temp  : Long_Real;
   begin
      This.Display_Kp := Proportional_Gain;
      This.Display_Ki := Integral_Gain;
      This.Display_Kd := Derivative_Gain;

      This.Kp := Proportional_Gain;

      --  This.Ki := Integral_Gain * Sample_Time_In_Sec;
      Temp := Long_Real (Integral_Gain) * Sample_Time_In_Sec;
      Constrain_To_Real (Temp);
      This.Ki := Real (Temp);

      --  This.Kd := Derivative_Gain / Sample_Time_In_Sec;
      Temp := Long_Real (Derivative_Gain) / Sample_Time_In_Sec;
      Constrain_To_Real (Temp);
      This.Kd := Real (Temp);

      if Direction = Reversed then
         This.Kp := -This.Kp;
         This.Ki := -This.Ki;
         This.Kd := -This.Kd;
      end if;
   end Tune;

   ------------------------
   -- Reconfigure_Period --
   ------------------------

   procedure Reconfigure_Period
     (This     : in out PID_Controller;
      Interval : Positive_Milliseconds)
   is
      Ratio : constant Long_Real := Long_Real (Interval) / Long_Real (This.Period);
      Temp  : Long_Real;
   begin
      --  This.Ki := This.Ki * Ratio;
      Temp := Long_Real (This.Ki) * Ratio;
      Constrain_To_Real (Temp);
      This.Ki := Real (Temp);

      --  This.Kd := This.Kd / Ratio;
      Temp := Long_Real (This.Kd) / Ratio;
      Constrain_To_Real (Temp);
      This.Kd := Real (Temp);

      This.Period := Interval;
   end Reconfigure_Period;

   -------------------------------
   -- Reconfigure_Output_Limits --
   -------------------------------

   procedure Reconfigure_Output_Limits
     (This             : in out PID_Controller;
      Control_Variable : in out Real;
      Limit            : Bounds)
   is
   begin
      This.Output_Limits := Limit;
      if This.Enabled then
         Constrain (Control_Variable, This.Output_Limits);
         Constrain (Real (This.I_Term), This.Output_Limits);
      end if;
   end Reconfigure_Output_Limits;

   ------------
   -- Enable --
   ------------

   procedure Enable
     (This             : in out PID_Controller;
      Process_Variable : Real;  -- the Process Variable
      Control_Variable : Real)  -- the Control/Manipulated Variable
   is
   begin
      if not This.Enabled then
         --  we just went from disabled to enabled so we should ensure a
         --  "bumpless" mode change
         This.I_Term := Long_Real (Control_Variable);
         This.Previous_Input := Process_Variable;
         Constrain (Real (This.I_Term), This.Output_Limits);
      end if;
      This.Enabled := True;
   end Enable;

   -------------
   -- Disable --
   -------------

   procedure Disable (This : in out PID_Controller) is
   begin
      This.Enabled := False;
   end Disable;

   ---------------------------
   -- Reconfigure_Direction --
   ---------------------------

   procedure Reconfigure_Direction
     (This      : in out PID_Controller;
      Direction : Controller_Directions)
   is
   begin
      if This.Enabled and This.Direction /= Direction then
         This.Kp := -This.Kp;
         This.Ki := -This.Ki;
         This.Kd := -This.Kd;
      end if;
      This.Direction := Direction;
   end Reconfigure_Direction;

   ---------------------------------
   -- Reconfigure_Gain_Parameters --
   ---------------------------------

   procedure Reconfigure_Gain_Parameters
     (This              : in out PID_Controller;
      Proportional_Gain : Real;
      Integral_Gain     : Real;
      Derivative_Gain   : Real)
   is
   begin
      Tune (This,
            Proportional_Gain,
            Integral_Gain,
            Derivative_Gain,
            This.Period,
            This.Direction);
   end Reconfigure_Gain_Parameters;

   ---------------
   -- Constrain --
   ---------------

   procedure Constrain (Value : in out Real;  Limits : Bounds) is
   begin
      if Value > Limits.Max then
         Value := Limits.Max;
      elsif Value < Limits.Min then
         Value := Limits.Min;
      end if;
   end Constrain;

   -----------------------
   -- Constrain_To_Real --
   -----------------------

   procedure Constrain_To_Real (Value : in out Long_Real) is
   begin
      if Value > Max_Real then
         Value := Max_Real;
      elsif Value < Min_Real then
         Value := Min_Real;
      end if;
   end Constrain_To_Real;

end Process_Control_Floating_Point;
