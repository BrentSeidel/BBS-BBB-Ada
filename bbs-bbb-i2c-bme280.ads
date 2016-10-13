with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Unchecked_Conversion;
with Ada.Numerics.Generic_Elementary_Functions;
with BBS.BBB.i2c;
with BBS.units;
use type BBS.units.len_m;
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
package BBS.BBB.i2c.BME280 is
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
   dig_H2 : constant uint8 := 16#e2#; -- int16
   dig_H3 : constant uint8 := 16#e3#; -- uint8
   dig_H4 : constant uint8 := 16#e4#; -- int16
   dig_H5 : constant uint8 := 16#e5#; -- int16
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
   press_over_0  : constant uint8 := 2#000_000_00#;
   press_over_1  : constant uint8 := 2#001_000_00#;
   press_over_2  : constant uint8 := 2#010_000_00#;
   press_over_4  : constant uint8 := 2#011_000_00#;
   press_over_8  : constant uint8 := 2#100_000_00#;
   press_over_16 : constant uint8 := 2#101_000_00#;
   --
   -- Temperature
   --
   temp_over_0  : constant uint8 := 2#000_000_00#;
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
   -- Conversion types
   --
--   cvt_temp : constant uint8 := 16#2e#;
--   cvt_press0 : constant uint8 := 16#34#;
--   cvt_press1 : constant uint8 := 16#74#;
--   cvt_press2 : constant uint8 := 16#b4#;
--   cvt_press3 : constant uint8 := 16#f4#;
   --
   -- Given local pressure and altimeter setting, determine the pressure
   -- altitude.  Given local pressure and altitude, determine the altimeter
   -- setting.
   --
--   function pressure_altitude(pressure : BBS.units.press_p;
--                              altm : BBS.units.press_p) return BBS.units.len_m;
--   function altimeter(pressure : BBS.units.press_p;
--                      altitude : BBS.units.len_m) return BBS.units.press_p;
   --
   -- The configure procedure needs to be called first to initialize the
   -- calibration constants from the device.
   --
   -- Note that a temperature value need to be processed before any pressure
   -- values are read.  Processing the temperature produces some values that
   -- are needed to produce the calibrated pressure.
   --
   -- The temperature and pressure are read out of the same registers.  The value
   -- in the registers is determined by the start conversion procedure.  The
   -- start_conversion routine records the type of reading requested.  Then when
   -- get_temp or get_press is called, the type is checked to ensure that the
   -- proper reading was started.
   --
--   procedure configure(error : out integer);
   --
   -- Starts the BME280 converting data.  The conversion types are listed above.
   -- Note that the conversion type must match the get value function.  An
   -- exception will be thrown if it doesn't match.
   --
--   procedure start_conversion(kind : uint8; error : out integer);
   --
   -- Check for data ready.  Reading a value before data is ready will have
   -- undesirable results.
   --
--   function data_ready(error : out integer) return boolean;
   --
   -- Return a calibrated temperature value.  Temperature is returned in units
   -- of degrees Celsius.
   --
--   function get_temp(error : out integer) return float;
   --
   -- Return calibrated temperature in units of 0.1 degrees Celsius.
   --
--   function get_temp(error : out integer) return integer;
   --
   -- Return temperature in various units.
   --
--   function get_temp(error : out integer) return BBS.units.temp_c;
--   function get_temp(error : out integer) return BBS.units.temp_f;
--   function get_temp(error : out integer) return BBS.units.temp_k;
   --
   -- Return a calibrated pressure value.  Note that a temperature reading must
   -- be made before calibrated pressure can be successfully computed.  Pressure
   -- is returned in units of Pascals.
   --
--   function get_press(error : out integer) return integer;
   --
   -- Return pressure in various units.
   --
--   function get_press(error : out integer) return BBS.units.press_p;
--   function get_press(error : out integer) return BBS.units.press_mb;
--   function get_press(error : out integer) return BBS.units.press_atm;
--   function get_press(error : out integer) return BBS.units.press_inHg;
   --
   -- Stuff for object oriented interface.  These basically emulate the function
   -- of the conventional routines above.
   --
   type BME280_record is new i2c_device_record with private;
   type BME280_ptr is access BME280_record;
   --
   function i2c_new return BME280_ptr;
   procedure configure(self : not null access BME280_record'class; port : i2c_interface;
                       addr : addr7; error : out integer);
   --
   procedure start_conversion(self : not null access BME280_record'class; error : out integer);
   function data_ready(self : not null access BME280_record'class; error : out integer) return boolean;
   procedure read_data(self : not null access BME280_record'class; error : out integer);
   function get_temp(self : not null access BME280_record'class) return integer;
   function get_temp(self : not null access BME280_record'class) return BBS.units.temp_c;
   function get_temp(self : not null access BME280_record'class) return BBS.units.temp_f;
   function get_temp(self : not null access BME280_record'class) return BBS.units.temp_k;
   function get_press(self : not null access BME280_record'class) return integer;
   function get_press(self : not null access BME280_record'class) return BBS.units.press_p;
   function get_press(self : not null access BME280_record'class) return BBS.units.press_mb;
   function get_press(self : not null access BME280_record'class) return BBS.units.press_atm;
   function get_press(self : not null access BME280_record'class) return BBS.units.press_inHg;
   --
private
   buff : aliased buffer;
   --
   -- Calibration constants.  These are read from the BME280.
   --
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
   --  Values from temperature conversion
   --
   x1 : integer;
   x2 : integer;
   b5 : integer;
   --
   --
   -- Some unchecked conversions are needed in pressure conversion.
   --
   function int_to_uint32 is
     new Ada.Unchecked_Conversion(source => integer, target => uint32);
   function uint32_to_int is
     new Ada.Unchecked_Conversion(source => uint32, target => integer);
   --
   type BME280_record is new i2c_device_record with record
      buff : aliased buffer;
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
      --  Values from temperature conversion (from BMP180 may now be different)
      --
      x1 : integer;
      x2 : integer;
      b5 : integer;
      --
      -- Data read from device
      --
      raw_press : uint32;
      raw_temp : uint32;
      raw_hum : uint32;
   end record;

end;
