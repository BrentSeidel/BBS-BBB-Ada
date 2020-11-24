#!/bin/sh
#
# This is the initialization script for a Raspberry Pi.  This is based on the
# BeagleBone Black initialization script.  I/O for the Raspberry Pi is much
# simpler than for the BeagleBone Black so much of the BeagleBone Black initialization
# is not applicable.
#
# Run this shell script as root to set protections on some of the device control files
# so that non-root users can access them.  It seems that the protection on these gets
# set back to 644 every time the Raspberry Pi boots.
#
# The GPIO pins are not exported by default on the Raspberry Pi.  The following
# commands export them.
#
echo "Additional GPIO"
echo 4 > /sys/class/gpio/export
echo 5 > /sys/class/gpio/export
echo 6 > /sys/class/gpio/export
echo 12 > /sys/class/gpio/export
echo 13 > /sys/class/gpio/export
echo 16 > /sys/class/gpio/export
echo 17 > /sys/class/gpio/export
echo 18 > /sys/class/gpio/export
echo 19 > /sys/class/gpio/export
echo 20 > /sys/class/gpio/export
echo 21 > /sys/class/gpio/export
echo 22 > /sys/class/gpio/export
echo 23 > /sys/class/gpio/export
echo 24 > /sys/class/gpio/export
echo 25 > /sys/class/gpio/export
echo 26 > /sys/class/gpio/export
echo 27 > /sys/class/gpio/export
#
# Set direction to "in" or "out" for input or output.
# Set value to "0" or "1" for output or read "0" or "1" for input.
#
echo "GPIO control files"
chmod 666 /sys/class/gpio/*/direction
chmod 666 /sys/class/gpio/*/value
#
