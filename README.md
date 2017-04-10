# Robotics_with_Ada
Robotics with Ada, ARM, and Lego

This program demonstrates interfacing to Lego NXT Mindstorms sensors and 
effectors using Ada and low-cost ARM evaluation boards instead of the 
NXT Mindstorms Brick. 

Note: clone recursively, because a local copy of the Ada Driver Library
is expected, located at the project root level (as referenced in the
project gpr file).

For the touch sensor interfacing demonstration:

   The program displays output to the LCD screen of an STM32F429I Discovery
   board, but does not rely on the specific board other than that. Make sure
   the project gpr file matches the target board used.

   The touch sensor is expected to be connected to ground and the external
   circuit in an active-low configuration (per the discriminant value
   specified in the source code). Use the sensor's red or black wire for
   ground, and the white wire for the input. If a different circuit is used,
   providing an active-high configuration, change the discriminant accordingly.

   The discrete input is to be connected from the external circuit to PB4.


