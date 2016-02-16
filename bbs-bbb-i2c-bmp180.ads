with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Unchecked_Conversion;
with BBS.BBB.i2c;
--
-- This package contains constants and routines to communicate with the BMP180
-- temperature and pressure on the i2c bus.
--
-- The interface is fairly basic and doesn't use the advanced features of the
-- device.  If you wish a more sophisticated interface, this could provide a
-- useful starting point.
--
package BBS.BBB.i2c.BMP180 is
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
   -- The configure procedure needs to be called first to initialize the
   -- calibration constants from the device.
   --
   -- Note that a temperature value need to be processed before any pressure
   -- values are read.  Processing the temperature produces some values that
   -- are needed to produce the calibrated pressure.
   --
   -- The temperature and pressure are read out of the same registers.  The value
   -- in the registers is determined by the start conversion procedure.  If one
   -- gets mixed up and attempts to read a pressure after starting a temperature
   -- conversion or vice versa, the values returned will not be useful.
   --
   procedure configure(error : out integer);
   --
   -- Starts the BMP180 converting data.  The conversion types are listed above.
   -- Note that the conversion type must match the get value function.  An
   -- exception will be thrown if it doesn't match.
   --
   procedure start_conversion(kind : uint8; error : out integer);
   --
   -- Check for data ready.  Reading a value before data is ready will have
   -- undesirable results.
   --
   function data_ready(error : out integer) return boolean;
   --
   -- Return a calibrated temperature value.
   --
   function get_temp(error : out integer) return float;
   --
   -- Return a calibrated pressure value.  Note that a temperature reading must
   -- be made before calibrated pressure can be successfully computed.
   --
   function get_press(error : out integer) return integer;
   --
private
   buff : aliased buffer;
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
   cal_temp : integer;
   --
   last_cvt : uint8 := 0;
   dump_values : constant boolean := false;
   --
   -- Some unchecked conversions are needed in pressure conversion.
   --
   function int_to_uint32 is
     new Ada.Unchecked_Conversion(source => integer, target => uint32);

end;
