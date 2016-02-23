#!/bin/sh
#
# Run this shell script as root to set protections on some of the device control files
# so that non-root users can access them.  It seems that the protection on these gets
# set back to 644 every time the BeagleBone Black boots.
#
#
# LED Control files
#
chmod 666 /sys/class/leds/beaglebone:green:usr0/brightness
chmod 666 /sys/class/leds/beaglebone:green:usr1/brightness
chmod 666 /sys/class/leds/beaglebone:green:usr2/brightness
chmod 666 /sys/class/leds/beaglebone:green:usr3/brightness
#
# Pin control files.  These are used to set what the particular pin is used
# for.
#
chmod 666 /sys/devices/platform/ocp/ocp:P8_07_pinmux/state
chmod 666 /sys/devices/platform/ocp/ocp:P8_08_pinmux/state
chmod 666 /sys/devices/platform/ocp/ocp:P8_09_pinmux/state
chmod 666 /sys/devices/platform/ocp/ocp:P8_10_pinmux/state
chmod 666 /sys/devices/platform/ocp/ocp:P8_11_pinmux/state
chmod 666 /sys/devices/platform/ocp/ocp:P8_12_pinmux/state
chmod 666 /sys/devices/platform/ocp/ocp:P8_13_pinmux/state
chmod 666 /sys/devices/platform/ocp/ocp:P8_14_pinmux/state
chmod 666 /sys/devices/platform/ocp/ocp:P8_15_pinmux/state
chmod 666 /sys/devices/platform/ocp/ocp:P8_16_pinmux/state
chmod 666 /sys/devices/platform/ocp/ocp:P8_17_pinmux/state
chmod 666 /sys/devices/platform/ocp/ocp:P8_18_pinmux/state
chmod 666 /sys/devices/platform/ocp/ocp:P8_19_pinmux/state
chmod 666 /sys/devices/platform/ocp/ocp:P8_26_pinmux/state
chmod 666 /sys/devices/platform/ocp/ocp:P9_11_pinmux/state
chmod 666 /sys/devices/platform/ocp/ocp:P9_12_pinmux/state
chmod 666 /sys/devices/platform/ocp/ocp:P9_13_pinmux/state
chmod 666 /sys/devices/platform/ocp/ocp:P9_14_pinmux/state
chmod 666 /sys/devices/platform/ocp/ocp:P9_15_pinmux/state
chmod 666 /sys/devices/platform/ocp/ocp:P9_16_pinmux/state
chmod 666 /sys/devices/platform/ocp/ocp:P9_17_pinmux/state
chmod 666 /sys/devices/platform/ocp/ocp:P9_18_pinmux/state
chmod 666 /sys/devices/platform/ocp/ocp:P9_21_pinmux/state
chmod 666 /sys/devices/platform/ocp/ocp:P9_22_pinmux/state
chmod 666 /sys/devices/platform/ocp/ocp:P9_23_pinmux/state
chmod 666 /sys/devices/platform/ocp/ocp:P9_24_pinmux/state
chmod 666 /sys/devices/platform/ocp/ocp:P9_26_pinmux/state
chmod 666 /sys/devices/platform/ocp/ocp:P9_27_pinmux/state
chmod 666 /sys/devices/platform/ocp/ocp:P9_30_pinmux/state
chmod 666 /sys/devices/platform/ocp/ocp:P9_42_pinmux/state
chmod 666 /sys/devices/platform/ocp/ocp:P9_91_pinmux/state
chmod 666 /sys/devices/platform/ocp/ocp:P9_92_pinmux/state
#
# Options for each pin can be found in the of_node/pinctrl-names file in the
# *_pinmux directory.  It appears that delimiters are not used between the
# options so when you cat the file you might see something like this:
# defaultgpiogpio_pugpio_pduartcani2cpruin
#
# Pin Control files for I2C bus 1
#
# Write "i2c" to the state file for P9_24 and P9_26 to enable the I2C bus.
# Then communicate on the bus using the device file /dev/i2c-?, where ? is
# probably 1, but apparently this is not fixed and may change.
#
# Example GPIO control files.  Copy and modify this for whichever GPIO
# pins that you need to use.  You can use the board diagrams at the end
# of http://beagleboard.org/support/bone101 to determine which ones to
# use.  To ensure that gpio is selected, write "gpio" to the desired
# state file above.
#
# Set direction to "in" or "out" for input or output.
# Set value to "0" or "1" for output or read "0" or "1" for input.
#
chmod 666 /sys/class/gpio/gpio49/direction
chmod 666 /sys/class/gpio/gpio49/value
