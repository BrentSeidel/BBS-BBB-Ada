# BBS-BBB-Ada
This repository contains a collection of Ada sources for working with embedded
Linux computers such as the BeagleBone Black or the Raspberry Pi, as well as the
ARM based Arduino Due.  Due to differences between platforms and the limited
amount of time I have to work on things, something may work on one platform,
but not on another.

[![Alire](https://img.shields.io/endpoint?url=https://alire.ada.dev/badges/bbs_embed_common.json)]
(https://alire.ada.dev/crates/bbs_embed_common.html)


[![Alire](https://img.shields.io/endpoint?url=https://alire.ada.dev/badges/bbs_embed_linux.json)]
(https://alire.ada.dev/crates/bbs_embed_linux.html)


# Notices
## Note
As a result of bringing in the Arduino Due, the non-object oriented interfaces
will generally be going away.  They may still exist at a low level, but are
not recommended for use.

## Note
Due to changes in the way that various I/O is handled, these routines are pretty much
guarenteed not to work on some versions of Linux.  The I2C routines were developed on
Debian Jessie, Linux V4.1.12-ti-r29 and use Ada 2012.

## Note
Some of the system files that control I/O are owned by root and do not allow world
write access.  A shell script is provided to set the permissions so that the Ada
program does not need to run as root.

## Note
I finally got around to developing a centralized units system.  The types for
many physical units are now defined in the BBS.units package.  This can be fetched
from the https://github.com/BrentSeidel/BBS-Ada.git repository on GitHub.  While
this adds an external dependancy for this repository, the result is that by using
it, all packages will have access to the same units, conversions, and functions.

An advantage of this approach is that if you use the proper units in your
calculations, the compiler will be able to catch mismatches between feet and
meters before you crash your probe into Mars.

## Note
Another reoganization is likely in work.  These routines seem to work fairly well on
Linux based embedded systems.  I am getting interested in trying out Ada on bare
ARM based systems such as the Arduino Due.  It would be nice to be able to re-use
some of this software.  The approach that I'm currently thinking of is to use abstract
base clases for the various types of I/O such as i2c or gpio.  These would define the
basic interface routines for these I/O.  The concrete classes would contain the
specific code for these I/O.  Thus, for example, there would be a base gpio class and
then a concrete gpio.linux class and a concrete gpio.arduion_due class.  This might not
be a big advantage for gpio, but for i2c it would mean that I don't need to update any
of the routines for the various i2c devices.  This will require a bit of planning and
experimentation.

# Contents
## LEDs
This works only on the BeagleBone Black which has four controllable LEDs on the
circuit board.  The BBS.embed.LED package contains definitions and routines for
controlling the four built-in LEDs on the board next to the ethernet connector.

## i2c
The BBS.embed.i2c package contains a set of objects and routines for reading and
writing to devices on I2C buses.  To actually use the I2C bus, you must create
an object from either the BBS.embed.I2C.Due or BBS.embed.I2C.Linux packages.

**Note** after a bug fix, the I2C interface works on a Raspberry PI 3.  It should
work on other models as well.  Just pass "/dev/null" as the pin control files.  A
configuration routine has been provided that omits the pin control parameters.
The Linux version tested are:
* Raspberry Pi-3 Linux 4.4.13-v7+ #894 SMP Mon Jun 13 13:13:27 BST 2016 armv7l GNU/Linux
* Raspberry PI 4 Linux 5.15.0-1033-raspi #36-Ubuntu SMP PREEMPT Thu Jun 22 08:10:31 UTC 2023 aarch64 aarch64 aarch64 GNU/Linux

## i2c subpackages
Subpackages have been created for the following devices to provide a higher level interface.

### ADS1015
This is a four channel 12 bit Analog to Digital Converter.  The four channels
can be used individually in a single ended mode.  Channel 0 and 1 can be
paired for differential measurements.  Channels 0, 1, and 2 can be paired
with channel 3 for differential measurements.  The 12 bit value is a signed
twos complement format.  This means that for single ended conversions, it
is effectively 11 bits.

### BME280
This chip contains pressure, temperature, and humidity sensors along with calibration
values.  The package contains an object oriented interface to retrieve calibrated
measurements from the sensors.

### BMP180
This chip contains a pressure sensor and a temperature sensor along with calibration
values.  The package contains both an object oriented and non-object oriented
interface to the sensors to retrieve calibrated measurements.

### L3GD20H
This is a gyroscope on a chip.  Routines are provided to configure the chip and to get
the rotation around the X, Y, and Z axis.  A routine is also provided to return all
rotations in a single record.

The chip also contains an temperature sensor and a routine is provided to access it.

### LSM303DLHC
This chip contains a 3 axis accelerometer and a 3 axis magnetometer.  Routines are provided
to initialize the chip and to return the acceleration or magnetic field for each axis.
Combined acceleration or magnetic field for all three axis can also be returned.

The chip also contains a temperature sensor and a routine is provided to access it.

### MCP4725
This is a single channel 12 bit DAC.  The fast write and write commands
have been tested and work.  The other features are likely to work, but haven't
been tested.

### MCP23008
This is an 8 bit I/O expander.  It can be used to add 8 GPIO pins per adapter.

### MCP23017
This is a 16 bit I/O expander.  It can be used to add 16 GPIO pins per adapter.

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
| Raspberry PI 4 | 657 kHz |
| Arduino Due | 384 kHz |

Note that the times are only approximate and can vary greatly depending on other
processing on the processor.

Note that the Raspberry Pi used the V2 ioctl() interface to GPIO while the BeagleBone
Black and Raspberry Pi 3 used the sysfs interface.

By way of comparison, an Adruino Mega 2560 got 126.6kHz or 128.2kHz (it toggled between
the two as I measured it) in a similar tight loop using digitalWrite().

### TB6612
This is used to control a stepper moter using 4 GPIO pins.

## PWM
This is an object oriented interface to the PWM controllers.  The BeagleBone
Black processor has 8 PWM controllers.  Due to the lack of a pinmux structure
for P8_28, one of the PWMs is not really usable.  The interface allows control
of the period (or rate) and the time high (or duty cycle) of the PWM output.

## Analog inputs
This is a simple object oriented interface to the analog inputs.  This base
class supports 12 bit conversions.  The concrete implementations for Linux or
Arduino Due may support other options.

On the BeagleBone Black, there are seven analog inputs available and the pins
that they are on are fixed purpose.  The A/D converter is a 12 bit converter,
so the result should be in the range 0..4095.

The Arduino Due has 16 analog inputs, 12 of which are actually wired up and are
available for use.  The A/D converter is a 12 bit converter.

## SPI
A basic API for the SPI interface is provided.  It does 8 bit reads and writes
using the default settings from Linux.  It has been tested on a Raspberry PI 3
and works well enough to interface with the AdaFruit RA8875 breakout board so
more features have not yet been developed.

### RA8875 LCD driver
This provides an interface to the RA8875 LCD driver and touchscreen interface.
It should be considered to be experimental.  Bugs are to be expected and full
functionality is not provided.  Drawing of most built in geometric shapes is
supported.  Text drawing is supported, but occasionally gets garbled.  I am
trying to figure this out.  Touch is supported with calibration, but to be
really useful will need filtering and debouncing.  If I get ambitious, I may
build a widget set on top of this.

**Note** When using the AdaFruit breakout board with the Raspberry PI 3 (and
probably any other 3V system, the power supply needs to be 5V.  The RA8875 itself
seems to work at 3V, but the backlight driver needs 5V.  This was a pain to debug.

**Note** The RA8875 does not tri-state the MISO line.  Thus it cannot share a SPI
interface with other devices without an additional tri-state device.

#### RA8875 Widgets
This is currently only a figment of my imagination.  I hope to eventually provide
some basic widgets for a touch interface.
