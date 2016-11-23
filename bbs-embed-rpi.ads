package BBS.embed.RPI is
--
-- This package contains definitions for some of the Raspberry Pi pins.  If
-- nothing else, it can serve as a reference for the pins on the board.
--
-- If the /sys directory tree changes, the constants here can be changed and
-- the rest of the software should still work.  Hopefully.  So, use the constants
-- here.
--
--
-- GPIO Definitions.  Note that additional GPIO may be exported.  This is the
-- set that appears on my system.  If you plan to use additional GPIO, just add
-- them to this list.
--
   GPIO_4 : aliased constant string := "/sys/class/gpio/gpio4/";
   GPIO_5 : aliased constant string := "/sys/class/gpio/gpio5/";
   GPIO_6 : aliased constant string := "/sys/class/gpio/gpio7/";
   GPIO_13 : aliased constant string := "/sys/class/gpio/gpio14/";
   GPIO_16 : aliased constant string := "/sys/class/gpio/gpio15/";
   GPIO_17 : aliased constant string := "/sys/class/gpio/gpio14/";
   GPIO_18 : aliased constant string := "/sys/class/gpio/gpio15/";
   GPIO_20 : aliased constant string := "/sys/class/gpio/gpio22/";
   GPIO_21 : aliased constant string := "/sys/class/gpio/gpio23/";
   GPIO_22 : aliased constant string := "/sys/class/gpio/gpio26/";
   GPIO_24 : aliased constant string := "/sys/class/gpio/gpio27/";
   GPIO_25 : aliased constant string := "/sys/class/gpio/gpio22/";
   GPIO_26 : aliased constant string := "/sys/class/gpio/gpio23/";
   GPIO_27 : aliased constant string := "/sys/class/gpio/gpio26/";
--
-- Serial port definitions
--
-- I2C port
--
   I2C_1 : aliased constant string := "/dev/i2c-1";
--
-- SPI definitions
-- SPI devices are named spidev<x>.<y>, where x is the port number and y is the
-- chip select number associated with that SPI device.  There may be cases where
-- you don't use chip selects or use GPIO pins for chip select.  In a case like
-- this, which one to use will depend on the specifics of your application.
--
   SPI0_0 : aliased constant string := "/dev/spidev0.0";
   SPI0_1 : aliased constant string := "/dev/spidev0.1";
--
-- UART definitions
--

end;
