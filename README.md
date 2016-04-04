# BBS-BBB-Ada
This repository contains a collection of Ada sources for working with the
BeagleBone Black Linux computer.  Some of these could probably be easily ported
to similar systems such as the Raspberry PI.

# Notice
Due to changes in the way that various I/O is handled, these routines are pretty much
guarenteed not to work on some versions of Linux.  The I2C routines were developed on
Debian Jessie, Linux V4.1.12-ti-r29 and use Ada 2012.

Some of the system files that control I/O are owned by root and do not allow world
write access.  A shell script is provided to set the permissions so that the Ada
program does not need to run as root.

# Contents
## LEDs
The BBS.BBB.LED package contains definitions and routines for controlling the four built-
in LEDs on the board next to the ethernet connector.

## i2c
The BBS.BBB.i2c package contains a set of routines for reading and writing to
devices on I2C bus 1 (pins P9-24 and P9-26).  Currently the other two I2C busses
are not supported.  They may be at some future point, but they seem to be used
for system functions so probably best not to mess with them.

An object oriented interface has been provided for the I2C bus as well as for
devices on the bus.  The functionality is the same as the non-object oriented
interface with the exception that the object oriented interface will allow for
mutliple devices of the same kind on an I2C bus.

## i2c subpackages
Subpackages have been created for the following devices to provide a higher level interface.

### BMP180
This chip contains a pressure sensor and a temperature sensor along with calibration
values.  The package also contains type definitions for a number of pressure,
temperature, and distance values with conversions between.  In addition routines
have been provided to return altitude given current pressure and altimeter setting
as well as altimeter setting given current pressure and altitude.

### LSM303DLHC
This chip contains a 3 axis accelerometer and a 3 axis magnetometer.  Routines are provided
to initialize the chip and to return the acceleration or magnetic field for each axis.
Combined acceleration or magnetic field for all three axis can also be returned.

The chip also contains a temperature sensor and a routine is provided to access it.

### L3GD20H
This is a gyroscope on a chip.  Routines are provided to configure the chip and to get
the rotation around the X, Y, and Z axis.  A routine is also provided to return all
rotations in a single record.

The chip also contains an temperature sensor and a routine is provided to access it.

### Devices
This package contains constants for other devices that do not yet have individual packages.

## GPIO
This is an object oriented interface to the GPIO pins.  It allows setting or
testing a pin.  This uses the /sys filesystem interface and this is not particularly
high performance.  It should be adequate for lighting indicators or checking the
state of switches.

## PWM
This is an object oriented interface to the PWM controllers.  The BeagleBone
Black processor has 8 PWM controllers.  Due to the lack of a pinmux structure
for P8_28, one of the PWMs is not really usable.  The interface allows control
of the period (or rate) and the time high (or duty cycle) of the PWM output.

## Analog inputs
This is a simple object oriented interface to the analog inputs.  There are seven
available and the pins that they are on are fixed purpose.  The A/D converted is
a 12 bit converter, so the result should be in the range 0..4095.
