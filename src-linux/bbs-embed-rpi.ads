--
--  Author: Brent Seidel
--  Date: 9-Aug-2024
--
--  This file is part of bbs_embed.
--  Bbs_embed is free software: you can redistribute it and/or modify it
--  under the terms of the GNU General Public License as published by the
--  Free Software Foundation, either version 3 of the License, or (at your
--  option) any later version.
--
--  bbs_embed is distributed in the hope that it will be useful, but
--  WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
--  Public License for more details.
--
--  You should have received a copy of the GNU General Public License along
--  with bbs_embed. If not, see <https://www.gnu.org/licenses/>.--
--
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
   GPIO_6 : aliased constant string := "/sys/class/gpio/gpio6/";
   GPIO_13 : aliased constant string := "/sys/class/gpio/gpio13/";
   GPIO_16 : aliased constant string := "/sys/class/gpio/gpio16/";
   GPIO_17 : aliased constant string := "/sys/class/gpio/gpio17/";
   GPIO_18 : aliased constant string := "/sys/class/gpio/gpio18/";
   GPIO_19 : aliased constant string := "/sys/class/gpio/gpio19/";
   GPIO_20 : aliased constant string := "/sys/class/gpio/gpio20/";
   GPIO_21 : aliased constant string := "/sys/class/gpio/gpio21/";
   GPIO_22 : aliased constant string := "/sys/class/gpio/gpio22/";
   GPIO_23 : aliased constant string := "/sys/class/gpio/gpio23/";
   GPIO_24 : aliased constant string := "/sys/class/gpio/gpio24/";
   GPIO_25 : aliased constant string := "/sys/class/gpio/gpio25/";
   GPIO_26 : aliased constant string := "/sys/class/gpio/gpio26/";
   GPIO_27 : aliased constant string := "/sys/class/gpio/gpio27/";
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
