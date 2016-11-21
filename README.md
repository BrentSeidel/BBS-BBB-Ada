# BBS-BBB-Ada
This repository contains a collection of Ada sources for working with the
BeagleBone Black Linux computer.  Some of these could probably be easily ported
to similar systems such as the Raspberry PI.

# Notices
## Note
Due to changes in the way that various I/O is handled, these routines are pretty much
guarenteed not to work on some versions of Linux.  The I2C routines were developed on
Debian Jessie, Linux V4.1.12-ti-r29 and use Ada 2012.

##Note
Some of the system files that control I/O are owned by root and do not allow world
write access.  A shell script is provided to set the permissions so that the Ada
program does not need to run as root.

##Note
I finally got around to developing a centralized units system.  The types for
many physical units are now defined in the BBS.units package.  This can be fetched
from the https://github.com/BrentSeidel/BBS-Ada.git repository on GitHub.  While
this adds an external dependancy for this repository, the result is that by using
it, all packages will have access to the same units, conversions, and functions.

An advantage of this approach is that if you use the proper units in your
calculations, the compiler will be able to catch mismatches between feet and
meters before you crash your probe into Mars.

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

**Note** after a bug fix, the I2C interface works on a Raspberry PI 3.  It should
work on other models as well.  Just pass "/dev/null" as the pin control files.  A
configuration routine has been provided that omits the pin control parameters.
The Linux version used is: Linux raspberrypi 4.4.13-v7+ #894 SMP
Mon Jun 13 13:13:27 BST 2016 armv7l GNU/Linux


## i2c subpackages
Subpackages have been created for the following devices to provide a higher level interface.

### BME280
This chip contains pressure, temperature, and humidity sensora along with calibration
values.  The package contains an object oriented interface to retrieve calibrated
measurements from the sensors.

### BMP180
This chip contains a pressure sensor and a temperature sensor along with calibration
values.  The package contains both an object oriented and non-object oriented
interface to the sensors to retrieve calibrated measurements.

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

### PCA9685
This is a 16 channel, 12 bit PWM controller.  Basic functions seem to work, but
this should be considered to be under development and even more experimental than
every thing else.  The basic function is available, more features are expected
to be added as I have time and energy.

### Devices
This package contains constants for other devices that do not yet have individual packages.

## GPIO
This is an object oriented interface to the GPIO pins.  It allows setting or
testing a pin.  This uses the /sys filesystem interface and this is not particularly
high performance.  It should be adequate for lighting indicators or checking the
state of switches.

The GPIO have now been tested to work on a Raspberry PI 3 as well as a BeagleBone
Black.  Using the following code to toggle a gpio as fast as possible:
~~~~
loop
   gpio.set(1);
   gpio.set(0);
end loop;
~~~~

I got the following results:

| System | Frequency |
|--------|-----------|
| BeagleBone Black | 60 kHz |
| Raspberry PI 3 | 160 kHz |

Note that the times are only approximate and can vary greatly depending on other
processing on the processor.


## PWM
This is an object oriented interface to the PWM controllers.  The BeagleBone
Black processor has 8 PWM controllers.  Due to the lack of a pinmux structure
for P8_28, one of the PWMs is not really usable.  The interface allows control
of the period (or rate) and the time high (or duty cycle) of the PWM output.

## Analog inputs
This is a simple object oriented interface to the analog inputs.  There are seven
available and the pins that they are on are fixed purpose.  The A/D converted is
a 12 bit converter, so the result should be in the range 0..4095.
