with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Unchecked_Conversion;
with Ada.Numerics.Generic_Elementary_Functions;
with BBS.embed.i2c;
with BBS.units;
use type BBS.units.len_m;
--
-- This package contains constants and routines to communicate with the BMP180
-- temperature and pressure on the i2c bus.
--
-- The interface is fairly basic and doesn't use the advanced features of the
-- device.  If you wish a more sophisticated interface, this could provide a
-- useful starting point.
--
-- Please refer to the BMP180 datasheet for more information.
--
package BBS.embed.i2c.BMP180 is
   --
   -- Addresses for the BMP180 pressure and temperature sensor
   --
   addr : constant addr7 := 16#77#;
   xlsb : constant uint8 := 16#f8#;
   lsb : constant uint8 := 16#f7#;
   msb : constant uint8 := 16#f6#;
   ctrl : constant uint8 := 16#f4#;
   reset : constant uint8 := 16#e0#;
   id : constant uint8 := 16#d0#;
   cal_start : constant uint8 := 16#aa#;
   cal_end : constant uint8 := 16#bf#;
   --
   -- Command constants
   --
   start_cvt : constant uint8 := 16#20#;
   --
   -- Conversion types
   --
   cvt_temp : constant uint8 := 16#2e#;
   cvt_press0 : constant uint8 := 16#34#;
   cvt_press1 : constant uint8 := 16#74#;
   cvt_press2 : constant uint8 := 16#b4#;
   cvt_press3 : constant uint8 := 16#f4#;
   --
   -- Given local pressure and altimeter setting, determine the pressure
   -- altitude.  Given local pressure and altitude, determine the altimeter
   -- setting.
   -- *** The following two functions have moved to the package BBS.units.
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
--     procedure configure(error : out integer);
--     --
--     -- Starts the BMP180 converting data.  The conversion types are listed above.
--     -- Note that the conversion type must match the get value function.  An
--     -- exception will be thrown if it doesn't match.
--     --
--     procedure start_conversion(kind : uint8; error : out integer);
--     --
--     -- Check for data ready.  Reading a value before data is ready will have
--     -- undesirable results.
--     --
--     function data_ready(error : out integer) return boolean;
--     --
--     -- Return a calibrated temperature value.  Temperature is returned in units
--     -- of degrees Celsius.
--     --
--     function get_temp(error : out integer) return float;
--     --
--     -- Return calibrated temperature in units of 0.1 degrees Celsius.
--     --
--     function get_temp(error : out integer) return integer;
--     --
--     -- Return temperature in various units.
--     --
--     function get_temp(error : out integer) return BBS.units.temp_c;
--     function get_temp(error : out integer) return BBS.units.temp_f;
--     function get_temp(error : out integer) return BBS.units.temp_k;
--     --
--     -- Return a calibrated pressure value.  Note that a temperature reading must
--     -- be made before calibrated pressure can be successfully computed.  Pressure
--     -- is returned in units of Pascals.
--     --
--     function get_press(error : out integer) return integer;
--     --
--     -- Return pressure in various units.
--     --
--     function get_press(error : out integer) return BBS.units.press_p;
--     function get_press(error : out integer) return BBS.units.press_mb;
--     function get_press(error : out integer) return BBS.units.press_atm;
--     function get_press(error : out integer) return BBS.units.press_inHg;
   --
   -- Stuff for object oriented interface.  These basically emulate the function
   -- of the conventional routines above.
   --
   type BMP180_record is new i2c_device_record with private;
   type BMP180_ptr is access BMP180_record;
   --
--   function i2c_new return BMP180_ptr;
   procedure configure(self : in out BMP180_record; port : i2c_interface;
                       addr : addr7; error : out err_code);
   --
   procedure start_conversion(self : in out BMP180_record; kind : uint8; error : out err_code);
   function data_ready(self : BMP180_record; error : out err_code) return boolean;
   function get_temp(self : in out BMP180_record; error : out err_code) return float;
   function get_temp(self : in out BMP180_record; error : out err_code) return integer;
   function get_temp(self : in out BMP180_record; error : out err_code) return BBS.units.temp_c;
   function get_temp(self : in out BMP180_record; error : out err_code) return BBS.units.temp_f;
   function get_temp(self : in out BMP180_record; error : out err_code) return BBS.units.temp_k;
   function get_press(self : BMP180_record; error : out err_code) return integer;
   function get_press(self : BMP180_record; error : out err_code) return BBS.units.press_p;
   function get_press(self : BMP180_record; error : out err_code) return BBS.units.press_mb;
   function get_press(self : BMP180_record; error : out err_code) return BBS.units.press_atm;
   function get_press(self : BMP180_record; error : out err_code) return BBS.units.press_inHg;
   --
private
--   buff : aliased buffer;
   --
   -- Calibration constants.  These are read from the BMP180.
   --
   ac1 : int16 := 0;
   ac2 : int16 := 0;
   ac3 : int16 := 0;
   ac4 : uint16 := 0;
   ac5 : uint16 := 0;
   ac6 : uint16 := 0;
   b1 : int16 := 0;
   b2 : int16 := 0;
   mb : int16 := 0;
   mc : int16 := 0;
   md : int16 := 0;
   --
   --  Values from temperature conversion
   --
   x1 : integer;
   x2 : integer;
   b5 : integer;
   --
   last_cvt : uint8 := 0;
   --
   -- Some unchecked conversions are needed in pressure conversion.
   --
   function int_to_uint32 is
     new Ada.Unchecked_Conversion(source => integer, target => uint32);
   function uint32_to_int is
     new Ada.Unchecked_Conversion(source => uint32, target => integer);
   --
   type BMP180_record is new i2c_device_record with record
--      buff : aliased buffer;
      ac1 : int16 := 0;
      ac2 : int16 := 0;
      ac3 : int16 := 0;
      ac4 : uint16 := 0;
      ac5 : uint16 := 0;
      ac6 : uint16 := 0;
      b1 : int16 := 0;
      b2 : int16 := 0;
      mb : int16 := 0;
      mc : int16 := 0;
      md : int16 := 0;
      x1 : integer;
      x2 : integer;
      b5 : integer;
      last_cvt : uint8 := 0;
   end record;

end;
