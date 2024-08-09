# Linux Specific Files

This directory contains files specific to Linux.  Some of these may also be specific
to the BeagleBone Black, Raspberry PI, or other Linux based board.

## bbs-embed-linux*
These files contain Ada code specific to Linux based systems (if present).

## bbs-embed-*-linux
These files contain Linux specific implementations of more general
packages.  Packages in this group include:

### bbs-embed-gpio-linux
This is the Linux implementation of the bbs-embed-gpio package.  It
provides access to the digital input and output pins.

### bbs-embed-i2c-linux
This provide the Linux specific implementation of the I2C interface.  It
is usable by all of the I2C based devices.  This allows these devices to be used
on either the Arduino Due or Linux based systems.

### bbs-embed-log-linux
This is the Linux specific logging function.  The implementation allows output
to be turned on and off under program control.

### bbs-embed-spi-linux
This is the Linux specific implementation of the SPI interface.

### bbs-embed-AIN-linux
The Linux specific implementation of analog inputs.  Not all Linux implementations
have analog inputs so this will not work everywhere.

