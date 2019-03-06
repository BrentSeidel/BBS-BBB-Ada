with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Unchecked_Conversion;
with Ada.Numerics.Generic_Elementary_Functions;
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
   -- Mode constants
   --
   mode_sleep  : constant uint8 := 2#000_000_00#;
   mode_force  : constant uint8 := 2#000_000_10#; -- 2#01# also works
   mode_normal : constant uint8 := 2#000_000_11#;
   --
   -- Oversampling constants
   -- Humidity
   --
   hum_over_0  : constant uint8 := 2#000#; -- datasheet says skipped
   hum_over_1  : constant uint8 := 2#001#;
   hum_over_2  : constant uint8 := 2#010#;
   hum_over_4  : constant uint8 := 2#011#;
   hum_over_8  : constant uint8 := 2#100#;
   hum_over_16 : constant uint8 := 2#101#; -- apparently the other values work as well
   --
   -- Pressure
   --
   press_over_0  : constant uint8 := 2#000_000_00#; -- skipped
   press_over_1  : constant uint8 := 2#001_000_00#;
   press_over_2  : constant uint8 := 2#010_000_00#;
   press_over_4  : constant uint8 := 2#011_000_00#;
   press_over_8  : constant uint8 := 2#100_000_00#;
   press_over_16 : constant uint8 := 2#101_000_00#;
   --
   -- Temperature
   --
   temp_over_0  : constant uint8 := 2#000_000_00#; -- skipped
   temp_over_1  : constant uint8 := 2#000_001_00#;
   temp_over_2  : constant uint8 := 2#000_010_00#;
   temp_over_4  : constant uint8 := 2#000_011_00#;
   temp_over_8  : constant uint8 := 2#000_100_00#;
   temp_over_16 : constant uint8 := 2#000_101_00#;
   --
   -- Status bits
   --
   stat_measuring : constant uint8 := 2#0000_1000#;
   stat_im_update : constant uint8 := 2#0000_0001#;
   --
   --
   -- Stuff for object oriented interface.  A non-object oriented interface
   -- is not provided for this device.  If you need one, it should be fairly
   -- easy to write one.
   --
   type BME280_record is new i2c_device_record with private;
   type BME280_ptr is access BME280_record;
   --
   function i2c_new return BME280_ptr;
   --
   -- The configure procedure needs to be called first to initialize the
   -- calibration constants from the device.
   --
   procedure configure(self : in out BME280_record; port : i2c_interface;
                       addr : addr7; error : out err_code);
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
   debug : Boolean := False;
--   buff : aliased buffer;
   --
   type BME280_record is new i2c_device_record with record
--      buff : aliased buffer;
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
      t_fine : int32;
      p_cal : uint32; -- LSB = Pa/256
      h_cal : uint32; -- LSB = %/1024
   end record;

end;
