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
--
--  This package provides a list of device addresses for various know I2C devices.
--
package BBS.embed.i2c.devices is
   --
   --  List of addresses for devices in separate packages.  These are listed
   --  here in address order to help check for duplicate addresses.
   --
   addr_LMS303DLHC_accel : constant addr7 := 16#19#; -- Accelerometer
   addr_LMS303DLHC_mag   : constant addr7 := 16#1e#; -- Magnetometer
   --
   --  Addresses for MCP23008 - 8 Bit I/O Expander
   --
   --  The MCP23008 has 3 address select pins so it can have one of 8 addresses.
   --  This allows up to 8 of the devices on a single I2C bus.  The MCP23S08 has
   --  2 address select pins allowing only 4 addresses.  These are the same as
   --   the first 4 addresses of the MCP23008.
   --
   addr_MCP23008_1 : constant addr7 := 16#20#;
   addr_MCP23008_2 : constant addr7 := 16#21#;
   addr_MCP23008_3 : constant addr7 := 16#22#;
   addr_MCP23008_4 : constant addr7 := 16#23#;
   addr_MCP23008_5 : constant addr7 := 16#24#;
   addr_MCP23008_6 : constant addr7 := 16#25#;
   addr_MCP23008_7 : constant addr7 := 16#26#;
   addr_MCP23008_8 : constant addr7 := 16#27#;
   --
   --  The MCP23017 16 bit I/O expander uses the same addresses as the MCP23008.
   --
   addr_MCP23017_1 : constant addr7 := 16#20#;
   addr_MCP23017_2 : constant addr7 := 16#21#;
   addr_MCP23017_3 : constant addr7 := 16#22#;
   addr_MCP23017_4 : constant addr7 := 16#23#;
   addr_MCP23017_5 : constant addr7 := 16#24#;
   addr_MCP23017_6 : constant addr7 := 16#25#;
   addr_MCP23017_7 : constant addr7 := 16#26#;
   addr_MCP23017_8 : constant addr7 := 16#27#;
   --
   --  The ADS1015 is a four channel, 12 bit ADC.  It has four possible
   --  addresses.  The address the device responds to is selected by
   --  connecting the address pin to one of four other pins.
   --
   addr_ADS1015_1 : constant addr7 := 16#48#;  --  GND
   addr_ADS1015_2 : constant addr7 := 16#49#;  --  Vdd
   addr_ADS1015_3 : constant addr7 := 16#4A#;  --  SDA
   addr_ADS1015_4 : constant addr7 := 16#4B#;  --  SCL
   --
   --  Addresses for PCA9685 PWM Controllers.  This has 6 address select pins so
   --  it can have 64 different addresses (apparently only 62 are usable).
   --  The addresses have the forms: 16#4x#, 16#5x#, 16#6x#, and 16#7x#.  Be
   --  careful as these can conflict with other devices.
   --
   addr_PCA9685_00 : constant addr7 := 16#40#;
   --
   addr_L3GD20H : constant addr7 := 16#6b#; -- Gyroscope
   --
   --  Addresses for MCP4725 12 bit Digital to Analog converter
   --
   addr_MCP4724_0 : constant addr7 := 16#60#;
   addr_MCP4724_1 : constant addr7 := 16#61#;
   addr_MCP4724_2 : constant addr7 := 16#62#;  --  Default address of AdaFruit unit
   addr_MCP4724_3 : constant addr7 := 16#63#;
   addr_MCP4724_4 : constant addr7 := 16#64#;
   addr_MCP4724_5 : constant addr7 := 16#65#;
   addr_MCP4724_6 : constant addr7 := 16#66#;
   addr_MCP4724_7 : constant addr7 := 16#67#;
   --
   addr_BMP180 : constant addr7 := 16#77#;
   addr_BME280 : constant addr7 := 16#77#; --  Same as BMP180
   -- Define some useful constants.
   --
end;
