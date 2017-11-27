------------------------------------------------------------------------------
--                                                                          --
--                      Copyright (C) 2017, AdaCore                         --
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
--     3. Neither the name of STMicroelectronics nor the names of its       --
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

--  This package provides an abstract base-class for NXT analog sensors, such
--  as the sound and light sensors. An an analog sensor, it uses an ADC to
--  convert the current reading into a value.

--  Note that you must have an external pull-up resistor tied to +5V on the
--  analog input pin. A 10K resistor works well.

--  In addition to the sensor-specific call to procedure Initialize, clients
--  are responsible for making the following calls to the ADC routines. They
--  are not called internally by Initialize because the functionality and
--  settings are independent of the specific ADC unit used for the sensor,
--  i.e., we don't want to hard-code them globally for one usage. The arguments
--  to procedure Configure_Common_Properties indicated below work with the NXT
--  sound and light sensors
--
--    STM32.ADC.Reset_All_ADC_Units;
--
--    STM32.ADC.Configure_Common_Properties
--      (Mode           => Independent,
--       Prescalar      => PCLK2_Div_2,
--       DMA_Mode       => Disabled,
--       Sampling_Delay => Sampling_Delay_5_Cycles);

with STM32.ADC;     use STM32.ADC;
with STM32.GPIO;    use STM32.GPIO;
with HAL;           use HAL;
with Math_Utilities;

package NXT.Analog is

   type NXT_Analog_Sensor is abstract tagged limited private;

   --  The wiring connections are as follows, for the standard Lego NXT
   --  connectors:
   --
   --  Pin 1 (white wire)  - Analog output from the device, required
   --  Pin 2 (black wire)  - Ground (either one, or both)
   --  Pin 3 (red wire)    - Ground (either one, or both)
   --  Pin 4 (green wire)  - Vcc (+5V), required to power the sensor
   --  Pin 5 (yellow wire) - Used in some sensors ("Digital_0" in literature)
   --  Pin 6 (blue wire)   - Used in some sensors ("Digital_1" in literature)

   procedure Initialize (This : in out NXT_Analog_Sensor);
   --  Initializes the input port/pin and the basics for the ADC itself, such
   --  as the resolution and alignment, but does not do anything because
   --  that will vary with the subclasses.
   --
   --  Must be overridden for concrete subclasses.
   --
   --  Sublcasses must also call this base-class version.

   subtype Varying_Directly is Natural;
   --  The raw values coming from the sensor vary inversely with the sensed
   --  input, i.e., "higher" inputs such as brighter light correspond to
   --  numerically lower raw values. In contrast, values of this subtype are
   --  meant to be those that vary directly with the sensed input, e.g., for
   --  the light sensor, 0 corresponds to darkest incoming light expected,
   --  whereas the max ADC resoluton value corresponds to the brightest
   --  incoming light expected. This intent is not enforced, so the user
   --  should ensure that the intent is honored.
   --
   --  Note the utility function As_Varying_Directly declared below.

   subtype Intensity is Varying_Directly range 0 .. 100;
   --  Sensed values presented as a percentage of the possible input range

   procedure Get_Intensity
     (This       : in out NXT_Analog_Sensor;
      Reading    : out Intensity;
      Successful : out Boolean)
     with Pre'Class => Enabled (This);
   --  Calls the Get_Raw_Reading function to get the ADC sample from
   --  This.Converter on This.Input_Channel (which is connected to
   --  This.Input_Pin) and returns that value as a percentage of the possible
   --  conversion range.
   --
   --  Unlike raw values, which vary inversely with the sensed inputs, the
   --  returned percentage values vary directly with sensed inputs. In other
   --  words, a lower returned percentage indicates a lesser incoming sensed
   --  value, e.g. less brightness sensed for a light sensor.

   procedure Get_Raw_Reading
     (This       : in out NXT_Analog_Sensor;
      Reading    : out Natural;
      Successful : out Boolean)
     is abstract
     with Pre'Class => Enabled (This);
   --  Interacts with the ADC indicated by This.Converter to acquire a raw
   --  sample on This.Input_Channel (which is connected to This.Input_Pin)
   --  and returns that raw value in the Reading parameter. The parameter
   --  Successful indicates whether the raw value acquisition succeeded.
   --
   --  NB: Raw values vary inversely with the brightness of the sensed light.
   --  This function does nothing to change that and simply returns the raw
   --  value sensed bythe ADC.
   --
   --  NB: This version polls for completion of the ADC conversion. If the
   --  conversion times out, Reading is zero and Successful is False.

   procedure Enable (This : in out NXT_Analog_Sensor) with
     Post'Class => Enabled (This);

   procedure Disable (This : in out NXT_Analog_Sensor) with
     Post'Class => not Enabled (This);

   function Enabled (This : NXT_Analog_Sensor) return Boolean;

   --  Manual calibration  ----------------------------------------------------

   type Sensor_Calibration is record
      Least, Greatest : Varying_Directly;
   end record;

   procedure Set_Calibration
     (This     : in out NXT_Analog_Sensor;
      Least    : Varying_Directly;
      Greatest : Varying_Directly)
   with Pre => Least < Greatest;
   --  Set the values corresponding to the least and greatest sensed input
   --  values acquired from this ADC, e.g., darkest and brightest sensed light.
   --  The specified values vary directly with the sensed input, as opposed to
   --  those values read immediately from the sensor.

   function Calibration (This : NXT_Analog_Sensor) return Sensor_Calibration;
   --  Returns the values corresponding to the least and greatestest sensed
   --  inputs from the ADC. The values vary directly with the incoming sensed
   --  values.
   --
   --  NB: The returned value can be passed to Set_Calibration without any
   --  changes by the client.

   --  Utility functions  ------------------------------------------------------

   function ADC_Conversion_Max_Value return Positive;
   --  Returns the maximum value the ADC unit can provide at the device's
   --  selected resolution.

   function As_Varying_Directly (Inverse_Value : Integer) return Integer with Inline;
   --  The raw values coming from the sensor vary inversely with the sensed
   --  input, i.e., "higher" inputs such as brighter light correspond to
   --  numerically lower raw values. This function translates the given value,
   --  presumably one that varies inversely with the sensed input, into a value
   --  that varies directly with the sensed input. This can be useful for
   --  passing values to the Set_Calibration routine above, for example.

private

   NXT_Brick_ADC_Resolution : constant ADC_Resolution := ADC_Resolution_10_Bits;

   Max_For_Resolution : constant Integer :=
     (case NXT_Brick_ADC_Resolution is
          when ADC_Resolution_12_Bits => 4095,
          when ADC_Resolution_10_Bits => 1023,
          when ADC_Resolution_8_Bits  => 255,
          when ADC_Resolution_6_Bits  => 63);

   type NXT_Analog_Sensor is abstract tagged limited record
      Converter     : access Analog_To_Digital_Converter;
      Input_Channel : Analog_Input_Channel;
      Input_Pin     : GPIO_Point;
      High          : Varying_Directly := Max_For_Resolution;
      Low           : Varying_Directly := 0;
   end record;
   --  This is an analog device abstraction so the underlying device is an
   --  analog-to-digital converter (ADC). Therefore, the components for
   --  the sensor object are those required for ADC use.

   function Mapped is new Math_Utilities.Range_To_Domain_Mapping (Integer);

   function Constrained is new Math_Utilities.Bounded_Value (Integer);

   function Regular_Conversion (Channel : Analog_Input_Channel)
     return Regular_Channel_Conversions is
     (1 => (Channel, Sample_Time => Sample_144_Cycles));
   --  A convenience function for creating a regular channel conversion list
   --  for the given channel. Used by subclasses within this package hierarchy.

   function ADC_Conversion_Max_Value return Positive is
      (Max_For_Resolution);

   function As_Varying_Directly (Inverse_Value : Integer) return Integer is
      (Max_For_Resolution - Inverse_Value);

end NXT.Analog;
