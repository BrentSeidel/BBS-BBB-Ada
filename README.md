# BBS-BBB-Ada
This repository contains a collection of Ada sources for working with the BeagleBone Black.

# Notice
Due to changes in the way that various I/O is handled, these routines are pretty much
guarenteed not to work on some versions of Linux.  The I2C routines were developed on
Debian Jessie, Linux V4.1.12-ti-r29 and use Ada 2012.

# Contents
## LEDs
The BBS.BBB.LED package contains definitions and routines for controlling the four built-
in LEDs on the board next to the ethernet connector.

## i2c
The BBS.BBB.i2c package contains a set of routines for reading and writing to devices on
I2C bus 1 (pins P9-24 and P9-26).

## i2c subpackages
Subpackages have been created for the following devices to provide a higher level interface.

### LMS303DLHC
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
