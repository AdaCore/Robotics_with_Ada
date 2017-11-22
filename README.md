# Robotics_with_Ada
Robotics with Ada, ARM, and Lego

This program demonstrates interfacing to Lego NXT Mindstorms sensors and
effectors using Ada and low-cost ARM evaluation boards instead of the
NXT Mindstorms Brick.

For the touch sensor interfacing demonstration:

* The program displays output to the LCD screen of an STM32F429I Discovery
board, but does not rely on the specific board other than that. Make sure
the project gpr file matches the target board used.

* The touch sensor is expected to be connected to ground and the external
circuit in an active-low configuration (per the discriminant value
specified in the source code). Use the sensor's red or black wire for
ground, and the white wire for the input. If a different circuit is used,
providing an active-high configuration, change the discriminant accordingly.

* The discrete input is to be connected from the external circuit to PB4.

For the analog sensor interfacing demonstrations:

* You must have a pull-up resistor for the analog input pin connected to the
+5V power pin. Otherwise you will see odd input (and hence output) values.

* Note that the two demonstration programs use different STM32 Discovery
Kits. The project gpr files handle this difference directly, when
building, but of course you must use the corresponding board for
execution. The "demo_analog_sensors" demonstration uses the STM32F429I
Discovery board because it displays the inputs on the kit's LCD screen.
That LCD is the only real reason that particular kit is used. The other
demonstration program "demo_sound_sensor" is set up to run on an STM32F4
Discovery kit, because that one can drive an LED with PWM, unlike the
F429I kit. Other supported STM32F4 targets with those capabilities can
be used instead, with minimal changes.
