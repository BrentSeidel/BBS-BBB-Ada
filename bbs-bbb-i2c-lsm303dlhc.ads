with BBS.BBB.i2c;
with BBS.units;
use type BBS.units.accel_g;
with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Text_IO;
with Ada.Float_Text_IO;
with Ada.Integer_Text_IO;
--
-- This package contains constants and routines to communicate with the LSM303DLHC
-- accelerometer and magnetometer on the i2c bus.
--
-- The interface is fairly basic and doesn't use the advanced features of the
-- device.  If you wish a more sophisticated interface, this could provide a
-- useful starting point.
--
package BBS.BBB.i2c.LSM303DLHC is
   package Math is new Ada.Numerics.Generic_Elementary_Functions(float);
   --
   -- Addresses for LSM303DLHC - accelerometer and magnetometer
   -- Note that though the accelerometer and magnetometer are on the same
   -- physical chip, they have different addresses on the I2C bus.
   --
   -- Accelerometer
   addr_accel : constant addr7 := 16#19#;
   accel_ctrl1 : constant uint8 := 16#20#;
   accel_ctrl2 : constant uint8 := 16#21#;
   accel_ctrl3 : constant uint8 := 16#22#;
   accel_ctrl4 : constant uint8 := 16#23#;
   --
   fs_2g : constant uint8 := 16#00#; -- Default
   fs_4g : constant uint8 := 16#10#;
   fs_8g : constant uint8 := 16#20#;
   fs_16g : constant uint8 := 16#40#;
   --
   accel_ctrl5 : constant uint8 := 16#24#;
   accel_ctrl6 : constant uint8 := 16#25#;
   accel_ref : constant uint8 := 16#26#;
   accel_status : constant uint8 := 16#27#;
   --
   -- Status bits
   accel_stat_zyxor : constant uint8 := 16#80#;
   accel_stat_zor : constant uint8 := 16#40#;
   accel_stat_yor : constant uint8 := 16#20#;
   accel_stat_xor : constant uint8 := 16#10#;
   accel_stat_zyxda : constant uint8 := 16#08#;
   accel_stat_zda : constant uint8 := 16#04#;
   accel_stat_yda : constant uint8 := 16#02#;
   accel_stat_xda : constant uint8 := 16#01#;
   --
   accel_out_x_h : constant uint8 := 16#28#;
   accel_out_x_l : constant uint8 := 16#29#;
   accel_out_y_h : constant uint8 := 16#2a#;
   accel_out_y_l : constant uint8 := 16#2b#;
   accel_out_z_h : constant uint8 := 16#2c#;
   accel_out_z_l : constant uint8 := 16#2d#;
   accel_fifo_ctrl : constant uint8 := 16#2e#;
   accel_fifo_src : constant uint8 := 16#2f#;
   accel_int1_cfg : constant uint8 := 16#30#;
   accel_int1_src : constant uint8 := 16#31#;
   accel_int1_ths : constant uint8 := 16#32#;
   accel_int1_duration : constant uint8 := 16#33#;
   accel_int2_cfg : constant uint8 := 16#34#;
   accel_int2_src : constant uint8 := 16#35#;
   accel_int2_ths : constant uint8 := 16#36#;
   accel_int2_duration : constant uint8 := 16#37#;
   accel_click_cfg : constant uint8 := 16#38#;
   accel_click_src : constant uint8 := 16#39#;
   accel_click_ths : constant uint8 := 16#3a#;
   accel_time_limit : constant uint8 := 16#3b#;
   accel_time_latency : constant uint8 := 16#3c#;
   accel_time_window : constant uint8 := 16#3d#;
   --
   -- Magnetometer
   addr_mag : constant addr7 := 16#1e#;
   mag_cra : constant uint8 := 16#00#;
   mag_crb : constant uint8 := 16#01#;
   --
   fs_1_3_gauss : constant uint8 := 16#20#;
   fs_1_9_gauss : constant uint8 := 16#40#;
   fs_2_5_gauss : constant uint8 := 16#60#;
   fs_4_0_gauss : constant uint8 := 16#80#;
   fs_4_7_gauss : constant uint8 := 16#a0#;
   fs_5_6_gauss : constant uint8 := 16#c0#;
   fs_8_1_gauss : constant uint8 := 16#e0#;
   --
   mag_mr : constant uint8 := 16#02#;
   mag_out_x_h : constant uint8 := 16#03#;
   mag_out_x_l : constant uint8 := 16#04#;
   mag_out_y_h : constant uint8 := 16#07#; -- note not ascending order
   mag_out_y_l : constant uint8 := 16#08#;
   mag_out_z_h : constant uint8 := 16#05#;
   mag_out_z_l : constant uint8 := 16#06#;
   mag_sr : constant uint8 := 16#09#;
   --
   -- Status bits
   mag_lock : constant uint8 := 16#02#;
   mag_drdy : constant uint8 := 16#01#;
   --
   mag_ira : constant uint8 := 16#0a#;
   mag_irb : constant uint8 := 16#0b#;
   mag_irc : constant uint8 := 16#0c#;
   mag_temp_h : constant uint8 := 16#31#;
   mag_temp_l : constant uint8 := 16#32#;
   --
   -- Define some types
   --
   type accelerations is
      record
         x : integer;
         y : integer;
         z : integer;
      end record;
   --
   type accelerations_g is
      record
         x : BBS.units.accel_g;
         y : BBS.units.accel_g;
         z : BBS.units.accel_g;
      end record;
   --
   --
   type magnetism is
      record
         x : integer;
         y : integer;
         z : integer;
      end record;
   --
   type magnetism_gauss is
      record
         x : BBS.units.mag_g;
         y : BBS.units.mag_g;
         z : BBS.units.mag_g;
      end record;
   --
   --
   procedure configure(error : out integer);
   procedure configure(accel_fs : uint8; mag_fs: uint8; error : out integer);
   --
   function get_acceleration_x(error : out integer) return integer;
   function get_acceleration_y(error : out integer) return integer;
   function get_acceleration_z(error : out integer) return integer;
   function get_accelerations(error : out integer) return accelerations;
   --
   function get_acceleration_x(error : out integer) return BBS.units.accel_g;
   function get_acceleration_y(error : out integer) return BBS.units.accel_g;
   function get_acceleration_z(error : out integer) return BBS.units.accel_g;
   function get_accelerations(error : out integer) return accelerations_g;
   --
   function get_accel_status(error : out integer) return uint8;
   function accel_data_ready(error : out integer) return boolean;
   --
   function get_temperature(error : out integer) return integer;
   function get_temperature(error : out integer) return float;
   function get_temperature(error : out integer) return BBS.units.temp_c;
   --
   function get_magnet_x(error : out integer) return integer;
   function get_magnet_y(error : out integer) return integer;
   function get_magnet_z(error : out integer) return integer;
   function get_magnetism(error : out integer) return magnetism;
   --
   function get_magnet_x(error : out integer) return BBS.units.mag_g;
   function get_magnet_y(error : out integer) return BBS.units.mag_g;
   function get_magnet_z(error : out integer) return BBS.units.mag_g;
   function get_magnetism(error : out integer) return magnetism_gauss;
   --
   function get_mag_status(error : out integer) return uint8;
   function mag_data_ready(error : out integer) return boolean;
   --
   -- Definitions for object oriented interface.  They basically do the same
   -- thing as the standard routines above.
   --
   type LSM303DLHC_record is new i2c_device_record with private;
   type LSM303DLHC_ptr is access LSM303DLHC_record;
   --
   function i2c_new return LSM303DLHC_ptr;
   procedure configure(self : not null access LSM303DLHC_record'class; port : i2c_interface;
                       accel : addr7; mag : addr7; error : out integer);
   procedure configure(self : not null access LSM303DLHC_record'class;
                       port : i2c_interface; addr_accel : addr7; addr_mag : addr7;
                       accel_fs : uint8; mag_fs : uint8; error : out integer);
   --
   -- The calibrate accel procedure can be called when the sensor is stationary
   -- in a 1G acceleration or gravitational field.  It takes multiple measurements
   -- of the X, Y, and Z acceleration and computes the average of X^2 + Y^2 + Z^2.
   -- This value should be 1.0.  A more sophesticated approach would be to
   -- compute a calibration value for each of the axis separately, but that would
   -- require the sensor to be precicely positioned three time.
   --
   procedure calibrate_accel(self : not null access LSM303DLHC_record'class);
   --
   function get_acceleration_x(self : not null access LSM303DLHC_record'class; error : out integer) return integer;
   function get_acceleration_y(self : not null access LSM303DLHC_record'class; error : out integer) return integer;
   function get_acceleration_z(self : not null access LSM303DLHC_record'class; error : out integer) return integer;
   function get_accelerations(self : not null access LSM303DLHC_record'class; error : out integer) return accelerations;
   --
   function get_acceleration_x(self : not null access LSM303DLHC_record'class; error : out integer) return BBS.units.accel_g;
   function get_acceleration_y(self : not null access LSM303DLHC_record'class; error : out integer) return BBS.units.accel_g;
   function get_acceleration_z(self : not null access LSM303DLHC_record'class; error : out integer) return BBS.units.accel_g;
   function get_accelerations(self : not null access LSM303DLHC_record'class; error : out integer) return accelerations_g;
   --
   function get_accel_status(self : not null access LSM303DLHC_record'class; error : out integer) return uint8;
   function accel_data_ready(self : not null access LSM303DLHC_record'class; error : out integer) return boolean;
   --
   function get_temperature(self : not null access LSM303DLHC_record'class; error : out integer) return integer;
   function get_temperature(self : not null access LSM303DLHC_record'class; error : out integer) return float;
   function get_temperature(self : not null access LSM303DLHC_record'class; error : out integer) return BBS.units.temp_c;
   --
   function get_magnet_x(self : not null access LSM303DLHC_record'class; error : out integer) return integer;
   function get_magnet_y(self : not null access LSM303DLHC_record'class; error : out integer) return integer;
   function get_magnet_z(self : not null access LSM303DLHC_record'class; error : out integer) return integer;
   function get_magnetism(self : not null access LSM303DLHC_record'class; error : out integer) return magnetism;
   --
   function get_magnet_x(self : not null access LSM303DLHC_record'class; error : out integer) return BBS.units.mag_g;
   function get_magnet_y(self : not null access LSM303DLHC_record'class; error : out integer) return BBS.units.mag_g;
   function get_magnet_z(self : not null access LSM303DLHC_record'class; error : out integer) return BBS.units.mag_g;
   function get_magnetism(self : not null access LSM303DLHC_record'class; error : out integer) return magnetism_gauss;
   --
   function get_mag_status(self : not null access LSM303DLHC_record'class; error : out integer) return uint8;
   function mag_data_ready(self : not null access LSM303DLHC_record'class; error : out integer) return boolean;
   --
private
   buff : aliased buffer;
   --
   -- The temperature offset is emperically determined and seems to work for my
   -- application.  You may want to check the values that you get from the
   -- temperature sensor and compare them with a calibrated thermometer to
   -- determine your own value.
   --
   temperature_offset : constant integer := 136;
   --
   accel_scale : float := 2.0 / 32768.0;
   mag_scale_xy : float := 1.0 / 1100.0;
   mag_scale_z : float := 1.0 / 980.0;
   --
   -- Object definition
   --
   type LSM303DLHC_record is new i2c_device_record with record
      buff : aliased buffer;
      addr_accel : addr7;
      addr_mag : addr7;
      temp_offset :  integer := 136;
      accel_scale : float := 2.0 / 32768.0;
      accel_calib : float := 1.0;
      mag_scale_xy : float := 1.0 / 1100.0;
      mag_scale_z : float := 1.0 / 980.0;
   end record;
end;
