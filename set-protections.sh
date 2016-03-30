#!/bin/sh
#
# Run this shell script as root to set protections on some of the device control files
# so that non-root users can access them.  It seems that the protection on these gets
# set back to 644 every time the BeagleBone Black boots.
#
#
# LED Control files
#
echo "LED Control files"
chmod 666 /sys/class/leds/beaglebone:green:usr0/brightness
chmod 666 /sys/class/leds/beaglebone:green:usr1/brightness
chmod 666 /sys/class/leds/beaglebone:green:usr2/brightness
chmod 666 /sys/class/leds/beaglebone:green:usr3/brightness
#
# Pin control files.  These are used to set what the particular pin is used
# for.
#
echo "Pin control files"
chmod 666 /sys/devices/platform/ocp/ocp:P*_pinmux/state
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
# Some GPIOs are not naturally exported.  These are generally on pins that have
# only a single use.  The following exports them and creates the /sys/class/gpio*
# file structure for them.
#
echo "Additional GPIO"
echo 110 > /sys/class/gpio/export
echo 111 > /sys/class/gpio/export
echo 113 > /sys/class/gpio/export
echo 117 > /sys/class/gpio/export
#
# There are a number of GPIO pins on connector P8.  These are used by some of
# on board functions (MMC and HDMI).  Don't fiddle with them unless you know
# what you're doing and really need to.
#
# You can use the board diagrams at the end
# of http://beagleboard.org/support/bone101 to determine which ones to
# use.  To ensure that gpio is selected, write "gpio" to the desired
# state file above.
#
# Set direction to "in" or "out" for input or output.
# Set value to "0" or "1" for output or read "0" or "1" for input.
#
echo "GPIO control files"
chmod 666 /sys/class/gpio/*/direction
chmod 666 /sys/class/gpio/*/value
#
# Build the directory structures for PWMs
#
echo "PWM Directory structures"
echo 0 > /sys/class/pwm/pwmchip0/export # EHRPWM0A
echo 1 > /sys/class/pwm/pwmchip0/export # EHRPWM0B
echo 0 > /sys/class/pwm/pwmchip2/export # EHRPWM1A
echo 1 > /sys/class/pwm/pwmchip2/export # EHRPWM1B
echo 0 > /sys/class/pwm/pwmchip5/export # EHRPWM2A
echo 1 > /sys/class/pwm/pwmchip5/export # EHRPWM2B
echo 0 > /sys/class/pwm/pwmchip4/export # ECAPPWM0
#echo 0 > /sys/class/pwm/pwmchip4/export # ECAPPWM2
# Note that ECAPPWM2 is on P9_28 and there is no *pinumx/state file for this
# pin.  So this PWM currently can't be used.
#
# Set protections for the PWM control files
#
echo "Set PWM protections"
chmod 666 /sys/class/pwm/pwmchip*/pwm*/enable
chmod 666 /sys/class/pwm/pwmchip*/pwm*/duty_cycle
chmod 666 /sys/class/pwm/pwmchip*/pwm*/period
#
