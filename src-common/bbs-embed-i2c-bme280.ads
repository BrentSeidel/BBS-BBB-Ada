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
with BBS.embed.i2c;
with BBS.units;
use type BBS.units.len_m;
use type BBS.units.press_p;
--
-- This package contains constants and routines to communicate with the BME280
-- temperature, pressure, and humidity sensor on the i2c bus.
--
-- The interface is fairly basic and doesn't use the advanced features of the
-- device.  If you wish a more sophisticated interface, this could provide a
-- useful starting point.
--
-- Please refer to the BME280 datasheet for more information.
--
package BBS.embed.i2c.BME280 is
   --
   type BME280_record is new i2c_device_record with private;
   type BME280_ptr is access BME280_record;
   --
   -- The configure procedure needs to be called first to initialize the
   -- calibration constants from the device.
   --
   procedure configure(self : in out BME280_record; port : i2c_interface;
                       addr : addr7; error : out err_code);
   --
   --  Check to see if configured device is present
   --
   function present(self : BME280_record) return Boolean;
   --
   -- Starts the BME280 converting data.  Temperature, pressure, and humidity
   -- are converted at the same time.
   --
   procedure start_conversion(self : BME280_record; error : out err_code);
   --
   -- Check for data ready.  Reading a value before data is ready will have
   -- undesirable results.
   --
   function data_ready(self : BME280_record; error : out err_code) return boolean;
   --
   -- Read the temperature, pressure, and humidity value (there's less overhead
   -- to read all three value than to try and read each individually) and compute
   -- the calibrated values
   --
   procedure read_data(self : in out BME280_record; error : out err_code);
   --
   -- Return the raw uncompensated values.  Used for debugging purposes after
   -- read_data() has been called.
   --
   procedure get_raw(self : BME280_record; raw_temp : out uint32;
                     raw_press : out uint32; raw_hum : out uint32);
   --
   -- Return the t_fine value.  Used for debugging purposes after
   -- read_data() has been called.
   --
   function get_t_fine(self : BME280_record) return int32;
   --
   -- Return the calibrated temperature value.  Temperature is returned in units
   -- of 0.01 degrees Celsius.
   --
   function get_temp(self : BME280_record) return integer;
   --
   -- Return temperature in various units.
   --
   function get_temp(self : BME280_record) return BBS.units.temp_c;
   function get_temp(self : BME280_record) return BBS.units.temp_f;
   function get_temp(self : BME280_record) return BBS.units.temp_k;
   --
   -- Return the calibrated pressure value.  Pressure is returned in units of
   -- 1/256 Pascals.
   --
   function get_press(self : BME280_record) return integer;
   --
   -- Return pressure in various units.
   --
   function get_press(self : BME280_record) return BBS.units.press_p;
   function get_press(self : BME280_record) return BBS.units.press_mb;
   function get_press(self : BME280_record) return BBS.units.press_atm;
   function get_press(self : BME280_record) return BBS.units.press_inHg;
   --
   -- Return the calibrated relative humidity.  The result is in units of
   -- 1/1024 %.
   --
   function get_hum(self : BME280_record) return integer;
   --
   -- Return the relative humidity in percent.
   --
   function get_hum(self : BME280_record) return float;
   --
private
   --
   -- Addresses for the BME280 pressure and temperature sensor
   --
   addr : constant addr7 := 16#77#; -- Device address on I2C bus
   data_start : constant uint8 := 16#f7#;
   up : constant uint8 := 16#f7#; -- uncomponsated pressure (msb, lsb, xlsb)
   ut : constant uint8 := 16#fa#; -- uncomponsated temperature (msb, lsb, xlsb)
   uh : constant uint8 := 16#fd#; -- uncomponsated humidity (msb, lsb)
   ctrl_hum : constant uint8 := 16#f2#;
   status : constant uint8 := 16#f3#;
   ctrl_meas : constant uint8 := 16#f4#;
   config : constant uint8 := 16#f5#;
   reset : constant uint8 := 16#e0#;
   id : constant uint8 := 16#d0#;
   dig_T1 : constant uint8 := 16#88#; -- uint16
   dig_T2 : constant uint8 := 16#8a#; -- int16
   dig_T3 : constant uint8 := 16#8d#; -- int16
   dig_P1 : constant uint8 := 16#8e#; -- uint16
   dig_P2 : constant uint8 := 16#90#; -- int16
   dig_P3 : constant uint8 := 16#92#; -- int16
   dig_P4 : constant uint8 := 16#94#; -- int16
   dig_P5 : constant uint8 := 16#96#; -- int16
   dig_P6 : constant uint8 := 16#98#; -- int16
   dig_P7 : constant uint8 := 16#9c#; -- int16
   dig_P8 : constant uint8 := 16#9e#; -- int16
   dig_P9 : constant uint8 := 16#9f#; -- int16
   dig_H1 : constant uint8 := 16#a1#; -- uint8
   dig_H2 : constant uint8 := 16#e1#; -- int16
   dig_H3 : constant uint8 := 16#e3#; -- uint8
   --
   -- Note that H4 and H5 are actually 12 bit integers packed into 3 bytes.
   --
   dig_H4 : constant uint8 := 16#e4#; -- int12
   dig_H45 : constant uint8 := 16#e5#; -- uint8
   dig_H5 : constant uint8 := 16#e6#; -- int12
   dig_H6 : constant uint8 := 16#e7#; -- uint8
   --
   --  Filter types
   --
   type filter_type is (filt_off, filt_2, filt_4, filt_8, filt_16);
   for filter_type use (filt_off => 0, filt_2 => 1, filt_4 => 2, filt_8 => 3,
                        filt_16 => 4);
   for filter_type'Size use 3;
   --
   --  Standby time type
   --
   type t_stby_type is (s_0_5, s_62_5, s_125, s_250, s_500, s_1000, s_10, s_20);
   for t_stby_type use (s_0_5 => 0, s_62_5 => 1, s_125 => 2, s_250 => 3,
                        s_500 => 4, s_1000 => 5, s_10 => 6, s_20 => 7);
   for t_stby_type'Size use 3;
   --
   -- Mode constants
   --
   type mode is (sleep, force, normal);
   for mode use (sleep => 0, force => 2, normal => 3);
   for mode'Size use 2;
   --
   -- Oversampling constants
   --
   type over_sample is (over_0, over_1, over_2, over_4, over_8, over_16);
   for over_sample use (over_0 => 0, over_1 => 1, over_2 => 2, over_4 => 3,
                        over_8 => 4, over_16 => 5);
   for over_sample'Size use 3;
   --
   --  Set to True to print debugging data.
   --
   debug : Boolean := False;
   --
   type BME280_record is new i2c_device_record with record
      T1 : uint16 := 0;
      T2 : int16 := 0;
      T3 : int16 := 0;
      P1 : uint16 := 0;
      P2 : int16 := 0;
      P3 : int16 := 0;
      P4 : int16 := 0;
      P5 : int16 := 0;
      P6 : int16 := 0;
      P7 : int16 := 0;
      P8 : int16 := 0;
      P9 : int16 := 0;
      H1 : uint8 := 0;
      H2 : int16 := 0;
      H3 : uint8 := 0;
      H4 : int16 := 0;
      H5 : int16 := 0;
      H6 : uint8 := 0;
      --
      -- Data read from device
      --
      raw_press : uint32;
      raw_temp : uint32;
      raw_hum : uint32;
      --
      -- Compensated values
      --
      t_fine : int32;
      p_cal : uint32; -- LSB = Pa/256
      h_cal : uint32; -- LSB = %/1024
   end record;
   --
   --  Types for device registers
   --
   type BME_config is record
      spi3e_en : boolean;
      dummy    : boolean;
      filter   : filter_type;
      t_stby   : t_stby_type;
   end record;
   for BME_config use record
      spi3e_en at 0 range 0 .. 0;
      dummy    at 0 range 1 .. 1;
      filter   at 0 range 2 .. 4;
      t_stby   at 0 range 5 .. 7;
   end record;
   for BME_config'Size use 8;
   --
   type BME_ctrl_meas is record
      BME_mode : mode;
      osrs_p   : over_sample;
      osrs_t   : over_sample;
   end record;
   for BME_ctrl_meas use record
      BME_mode at 0 range 0 .. 1;
      osrs_p   at 0 range 2 .. 4;
      osrs_t   at 0 range 5 .. 7;
   end record;
   for BME_ctrl_meas'Size use 8;
   --
   type BME_ctrl_hum is record
      osrs_h : over_sample;
   end record;
   for BME_ctrl_hum use record
      osrs_h at 0 range 0 .. 2;
   end record;
   for BME_ctrl_hum'Size use 8;
   --
   type BME_status is record
      measuring : Boolean;
      dummy0    : Boolean;
      dummy1    : Boolean;
      im_update : Boolean;
      dummy2    : Boolean;
      dummy3    : Boolean;
      dummy4    : Boolean;
      dummy5    : Boolean;
   end record;
   for BME_status use record
      measuring at 0 range 0 .. 0;
      dummy0    at 0 range 1 .. 1;
      dummy1    at 0 range 2 .. 2;
      im_update at 0 range 3 .. 3;
      dummy2    at 0 range 4 .. 4;
      dummy3    at 0 range 5 .. 5;
      dummy4    at 0 range 6 .. 6;
      dummy5    at 0 range 7 .. 7;
   end record;
   for BME_status'Size use 8;
end;
