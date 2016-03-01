with BBS.BBB.i2c;
--
-- This package contains constants and routines to communicate with the L3GD20H
-- gyroscope on the i2c bus.
--
-- The interface is fairly basic and doesn't use the advanced features of the
-- device.  If you wish a more sophisticated interface, this could provide a
-- useful starting point.
--
package BBS.BBB.i2c.L3GD20H is
   --
   -- Addresses for L3GD20H - gyroscope
   --
   addr : constant addr7 := 16#6b#;
   who_am_i : constant uint8 := 16#0f#;
   ctrl1 : constant uint8 := 16#20#;
   ctrl2 : constant uint8 := 16#21#;
   ctrl3 : constant uint8 := 16#22#;
   ctrl4 : constant uint8 := 16#23#;
   --
   -- Full scale selection
   fs_245dps : constant uint8 := 16#00#; -- default value
   fs_500dps : constant uint8 := 16#10#;
   fs_2000dps : constant uint8 := 16#20#;
   --
   ctrl5 : constant uint8 := 16#24#;
   ref : constant uint8 := 16#25#;
   out_temp : constant uint8 := 16#26#;
   status : constant uint8 := 16#27#;
   --
   -- Status bits
   zyx_or : constant uint8 := 16#80#; -- Underscore inserted because xor is a
   z_or : constant uint8 := 16#40#;   -- reserved word.
   y_or : constant uint8 := 16#20#;
   x_or : constant uint8 := 16#10#;
   zyxda : constant uint8 := 16#08#;
   zda : constant uint8 := 16#04#;
   yda : constant uint8 := 16#02#;
   xda : constant uint8 := 16#01#;
   --
   out_x_l : constant uint8 := 16#28#;
   out_x_h : constant uint8 := 16#29#;
   out_y_l : constant uint8 := 16#2a#;
   out_y_h : constant uint8 := 16#2b#;
   out_z_l : constant uint8 := 16#2c#;
   out_z_h : constant uint8 := 16#2d#;
   fifo_ctrl : constant uint8 := 16#2e#;
   fifo_src : constant uint8 := 16#2f#;
   ig_cfg : constant uint8 := 16#30#;
   ig_src : constant uint8 := 16#31#;
   ig_ths_xh : constant uint8 := 16#32#;
   ig_ths_xl : constant uint8 := 16#33#;
   ig_ths_yh : constant uint8 := 16#34#;
   ig_ths_yl : constant uint8 := 16#35#;
   ig_ths_zh : constant uint8 := 16#36#;
   ig_ths_zl : constant uint8 := 16#37#;
   ig_duration : constant uint8 := 16#38#;
   low_odr : constant uint8 := 16#39#;
   --
   -- Define some datatypes
   --
   --  Type for scaled rotation in degrees per second
   --
   type rate_dps is new float;
   --
   -- Record to hold all three rotation values in sensor values
   --
   type rotations is
      record
         x : integer;
         y : integer;
         z : integer;
      end record;
   --
   -- Record to hold all three rotation values in degrees per second
   --
   type rotations_dps is
      record
         x : rate_dps;
         y : rate_dps;
         z : rate_dps;
      end record;
   --
   -- Temperature.  If you need more temperature datatypes than just Celsius,
   -- you can copy them from the BMP180 package.
   --
   type Celsius is new integer;
   --
   -- Configure the device
   --
   procedure configure(error : out integer);
   procedure configure(deflection : uint8; error : out integer);
   --
   -- Get values from the device.  The integer values are the value read from
   -- the device registers.
   --
   function get_temperature(error : out integer) return integer;
   function get_rotation_x(error : out integer) return integer;
   function get_rotation_y(error : out integer) return integer;
   function get_rotation_z(error : out integer) return integer;
   --
   -- get_rotations gets the rotations around all three axises in a single
   -- transaction and returns them in a structure.
   --
   function get_rotations(error : out integer) return rotations;
   --
   -- Get values in engineering units.  These have been scaled and offset to
   -- convert them into standard units.
   --
   function get_temperature(error : out integer) return Celsius;
   function get_rotation_x(error : out integer) return rate_dps;
   function get_rotation_y(error : out integer) return rate_dps;
   function get_rotation_z(error : out integer) return rate_dps;
   function get_rotations(error : out integer) return rotations_dps;
   --
   function get_status(error : out integer) return uint8;
   function data_ready(error : out integer) return boolean;
   --
   -- Stuff for object oriented interface.  These basically emulate the function
   -- of the conventional routines above.
   --
   type L3GD20H_record is new i2c_device_record with private;
   type L3GD20H_ptr is access L3GD20H_record;
   --
   function i2c_new return L3GD20H_ptr;
   procedure configure(self : not null access L3GD20H_record'class; port : i2c_interface;
                       addr : addr7; error : out integer);
   procedure configure(self : not null access L3GD20H_record'class; port : i2c_interface;
                       addr : addr7; deflection : uint8; error : out integer);
   --
   function get_temperature(self : not null access L3GD20H_record'class; error : out integer) return integer;
   function get_rotation_x(self : not null access L3GD20H_record'class; error : out integer) return integer;
   function get_rotation_y(self : not null access L3GD20H_record'class; error : out integer) return integer;
   function get_rotation_z(self : not null access L3GD20H_record'class; error : out integer) return integer;
   function get_rotations(self : not null access L3GD20H_record'class; error : out integer) return rotations;
   --
   function get_temperature(self : not null access L3GD20H_record'class; error : out integer) return Celsius;
   function get_rotation_x(self : not null access L3GD20H_record'class; error : out integer) return rate_dps;
   function get_rotation_y(self : not null access L3GD20H_record'class; error : out integer) return rate_dps;
   function get_rotation_z(self : not null access L3GD20H_record'class; error : out integer) return rate_dps;
   function get_rotations(self : not null access L3GD20H_record'class; error : out integer) return rotations_dps;
   --
   function get_status(self : not null access L3GD20H_record'class; error : out integer) return uint8;
   function data_ready(self : not null access L3GD20H_record'class; error : out integer) return boolean;
   --
   -- When stationary, the sensors may not report 0.  This function should be
   -- called when the sensor is stationary.  It reads the rotations several times
   -- and averages the results.  This is used to calculate offset values.  Note
   -- that this feature is only available using the object oriented interface.
   --
   procedure measure_offsets(self : not null access L3GD20H_record'class);
   --
private
   buff : aliased buffer;
   --
   -- The temperature offset is emperically determined and seems to work for my
   -- application.  You may want to check the values that you get from the
   -- temperature sensor and compare them with a calibrated thermometer to
   -- determine your own value.
   --
   temperature_offset : constant integer := 37;
   --
   -- The selected full scale deflection.
   --
   dps_scale : float := 245.0/32767.0;
   --
   type L3GD20H_record is new i2c_device_record with record
      buff : aliased buffer;
      temp_offset : integer := 37;
      offset_x : integer := 0;
      offset_y : integer := 0;
      offset_z : integer := 0;
      scale : float := 245.0/32767.0;
   end record;
end;
