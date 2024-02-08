------------------------------------------------------------------------------
--                                                                          --
--                  Copyright (C) 2020-2024, AdaCore                        --
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

--  This package is based on the Arduino PID library, version 1.1.1,
--  available here: https://playground.arduino.cc/Code/PIDLibrary
--
--  The primary difference between this implementation and the Arduino version
--  is that the controller object does not compute the next time to execute.
--  The computation update procedure must be called at the rate specified to
--  the configuration routine. In addition, the PID object does not retain
--  pointers to the input, setpoint, and output objects (for the sake of
--  SPARK compatibility).
--
--  For a great explanation of the implementation, step-by-step, see
--  http://brettbeauregard.com/blog/2011/04/improving-the-beginners-pid-introduction/

--  see https://os.mbed.com/users/aberk/code/PID/file/6e12a3e5af19/PID.h/
--  for a version using percentages

generic
   type Real is digits <>;
   --  The type used for inputs, outputs, gain parameters, etc.
   type Long_Real is digits <>;
   --  Type Long_Real is used internally, within the PID controller
   --  implementation, to avoid overflow. The actual parameter must have at
   --  least twice the number of digits as the actual for Real, otherwise the
   --  compiler will reject the instantiation.
package Process_Control_Floating_Point with
  SPARK_Mode
is

   pragma Compile_Time_Error (Long_Real'Digits < 2 * Real'Digits, "insufficient Long_Real digits");

   pragma Unevaluated_Use_Of_Old (Allow);

   type PID_Controller is tagged limited private;

   type Bounds is record
      Min, Max : Real;
   end record with
     Predicate => Min < Max;
   --  A type used to express the minimum and maximum bounds for constraining
   --  output values at runtime

   type Controller_Directions is (Direct, Reversed);
   --  The PID controiller will either be connected to a Direct acting process
   --  (+output leads to +input) or a Reversed process(+output leads to -input.)

   type Millisecond_Units is mod 2**32;

   subtype Positive_Milliseconds is
     Millisecond_Units range 1 .. Millisecond_Units'Last;
   --  The (sub)type used for expressing the iterative period at which the
   --  output computation procedure will be called

   procedure Configure
     (This              : in out PID_Controller;
      Proportional_Gain : Real;
      Integral_Gain     : Real;
      Derivative_Gain   : Real;
      Period            : Positive_Milliseconds;
      Output_Limits     : Bounds;
      Direction         : Controller_Directions := Direct)
   with
     Global => null,
     Post'Class => not Enabled (This)                           and then
                   Current_Direction (This) = Direction         and then
                   Current_Output_Limits (This) = Output_Limits and then
                   Specified_Kp (This) = Proportional_Gain      and then
                   Specified_Ki (This) = Integral_Gain          and then
                   Specified_Kd (This) = Derivative_Gain;
   --  Configures the initial values. Only to be called once; after that some
   --  settings can be individually altered via dedicated procedures. Note that
   --  the Period is the interval at which procedure Compute_Output is called by
   --  the application, and (when this controller is used with a physical plant)
   --  must match reality!

   procedure Enable
     (This             : in out PID_Controller;
      Process_Variable : Real;  -- current input value from the process
      Control_Variable : Real)  -- current output valu
   with
     Global => null,
     Post'Class => Enabled (This) and
                   Current_Output_Limits (This) = Current_Output_Limits (This)'Old;
   --  In this mode calls to Compute_Output update the output argument. The
   --  Process_Variable and Control_Variable values are used for "bumpless"
   --  transitions from the Disabled to Enabled state.

   procedure Disable (This : in out PID_Controller) with
     Global => null,
     Post'Class => not Enabled (This);
   --  In this mode calls to Compute_Output will do nothing.

   procedure Compute_Output
     (This             : in out PID_Controller;
      Process_Variable : Real;         -- the input, Measured Value/Variable
      Setpoint         : Real;
      Control_Variable : in out Real)  -- the output, Manipulated Variable
   with
     Global => null,
     Post'Class => Current_Output_Limits (This) = Current_Output_Limits (This)'Old and then
                   (if Enabled (This)'Old
                      then Within_Limits (Control_Variable, Current_Output_Limits (This))
                      else Control_Variable = Control_Variable'Old);
   --  When Enabled, computes the new value for the parameter passed to
   --  Control_Variable, based on the values of Process_Variable and Setpoint. Can be called when not
   --  Enabled but in that case will do nothing.
   --
   --  When controlling a real, physical plant, must be called at the
   --  period specified by the value of Period passed to Configure or to
   --  Reconfigure_Period. This requireemnt exists because the elapsed time
   --  per call is part of the output calculations.

   procedure Reconfigure_Output_Limits
     (This             : in out PID_Controller;
      Control_Variable : in out Real;
      Limit            : Bounds)  -- note that Limit.Min must be < Limit.Max
   with
     Global => null,
     Post'Class => Current_Output_Limits (This) = Limit;
   --  Change the min and max output limits (e.g., from those specified via
   --  Configure).

   procedure Reconfigure_Period
     (This     : in out PID_Controller;
      Interval : Positive_Milliseconds)
   with
     Global => null,
     Pre'Class  => Interval <= Current_Period (This),
     Post'Class => Current_Period (This) = Interval;
   --  Inform the controller of the new interval at which Compute_Output will be
   --  called.

   procedure Reconfigure_Direction
     (This      : in out PID_Controller;
      Direction : Controller_Directions)
   with
     Global => null,
     Post'Class => Current_Direction (This) = Direction;
   --  The PID controiller will either be connected to a Direct acting process
   --  (+output leads to +input) or a Reversed process(+output leads to -input.)

   procedure Reconfigure_Gain_Parameters
     (This              : in out PID_Controller;
      Proportional_Gain : Real;
      Integral_Gain     : Real;
      Derivative_Gain   : Real)
   with
     Global => null,
     Post'Class => Specified_Kp (This) = Proportional_Gain and then
                   Specified_Ki (This) = Integral_Gain     and then
                   Specified_Kd (This) = Derivative_Gain;
   --  Allows these parameters to be changed on the fly. This facility is
   --  useful when we want the controller to be agressive at some times and
   --  conservative at others. For example, we can set the controller to use
   --  conservative tuning parameters when near the setpoint and more agressive
   --  tuning parameters when farther away.

   function Enabled (This : PID_Controller) return Boolean with
     Global => null;

   function Current_Output_Limits (This : PID_Controller) return Bounds with
     Global => null;

   function Current_Direction (This : PID_Controller) return Controller_Directions with
     Global => null;

   function Current_Period (This : PID_Controller) return Positive_Milliseconds with
     Global => null;

   --  These functions return the tuning parameters as specified by the client.
   --  The parameters actually used are based on those specified values but are
   --  not the same actual values so we provide a way to get those originally
   --  specified.

   function Specified_Kp (This : PID_Controller) return Real with
     Global => null;

   function Specified_Ki (This : PID_Controller) return Real with
     Global => null;

   function Specified_Kd (This : PID_Controller) return Real with
     Global => null;

   function Within_Limits (Value : Real; Limits : Bounds) return Boolean with
     Inline;
   --  util function used in postconditions

private

   type PID_Controller is tagged limited record
      --  the unaltered tuning parameters specified by the client
      Display_Kp     : Real := 0.0;
      Display_Ki     : Real := 0.0;
      Display_Kd     : Real := 0.0;

      --  the actual tuning parameters applied
      Kp             : Real := 0.0;
      Ki             : Real := 0.0;
      Kd             : Real := 0.0;

      Previous_Input : Real := 0.0;
      I_Term         : Long_Real := 0.0;

      Direction      : Controller_Directions := Direct;
      Period         : Positive_Milliseconds := 1;
      Output_Limits  : Bounds := (0.0, 0.1);

      Enabled        : Boolean := False;  -- not arbitrary, all others are.
   end record;

   -------------
   -- Enabled --
   -------------

   function Enabled (This : PID_Controller) return Boolean is
     (This.Enabled);

   -------------------
   -- Within_Limits --
   -------------------

   function Within_Limits (Value : Real; Limits : Bounds) return Boolean is
     (Value in Limits.Min .. Limits.Max);

   -----------------------
   -- Current_Direction --
   -----------------------

   function Current_Direction (This : PID_Controller) return Controller_Directions is
     (This.Direction);

   ---------------------------
   -- Current_Output_Limits --
   ---------------------------

   function Current_Output_Limits (This : PID_Controller) return Bounds is
     (This.Output_Limits);

   --------------------
   -- Current_Period --
   --------------------

   function Current_Period (This : PID_Controller) return Positive_Milliseconds is
     (This.Period);

   ------------------
   -- Specified_Kp --
   ------------------

   function Specified_Kp (This : PID_Controller) return Real is
     (This.Display_Kp);

   ------------------
   -- Specified_Ki --
   ------------------

   function Specified_Ki (This : PID_Controller) return Real is
     (This.Display_Ki);

   ------------------
   -- Specified_Kd --
   ------------------

   function Specified_Kd (This : PID_Controller) return Real is
     (This.Display_Kd);

end Process_Control_Floating_Point;
