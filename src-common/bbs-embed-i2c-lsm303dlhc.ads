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
use type BBS.units.accel_g;
--
--  This package contains constants and routines to communicate with the LSM303DLHC
--  accelerometer and magnetometer on the i2c bus.
--
--  The interface is fairly basic and doesn't use the advanced features of the
--  device.  If you wish a more sophisticated interface, this could provide a
--  useful starting point.
--
package BBS.embed.i2c.LSM303DLHC is
   --
   -- Define some types
   --
   --  Accelerometer full scale deflection
   --
   type accel_fsd is (fs_2g, fs_4g, fs_8g, fs_16g);
   for accel_fsd use (fs_2g => 0, fs_4g => 16#10#, fs_8g => 16#20#, fs_16g => 16#40#);
   for accel_fsd'Size use 8;
   --
   --  Magnetometer full scale deflection
   --
   type mag_fsd is (fs_1_3_gauss, fs_1_9_gauss, fs_2_5_gauss, fs_4_0_gauss,
                    fs_4_7_gauss, fs_5_6_gauss, fs_8_1_gauss);
   for mag_fsd use (fs_1_3_gauss => 16#20#, fs_1_9_gauss => 16#40#, fs_2_5_gauss => 16#60#,
                    fs_4_0_gauss => 16#80#, fs_4_7_gauss => 16#a0#, fs_5_6_gauss => 16#c0#,
                    fs_8_1_gauss => 16#e0#);
   for mag_fsd'Size use 8;
   --
   -- Accelerometer status bits
   --
   type accel_status_type is record
      xda   : Boolean;
      yda   : Boolean;
      zda   : Boolean;
      zyxda : Boolean;
      x_or  : Boolean;
      y_or  : Boolean;
      z_or  : Boolean;
      zyxor : Boolean;
   end record;
   for accel_status_type use record
      xda   at 0 range 0 .. 0;
      yda   at 0 range 1 .. 1;
      zda   at 0 range 2 .. 2;
      zyxda at 0 range 3 .. 3;
      x_or  at 0 range 4 .. 4;
      y_or  at 0 range 5 .. 5;
      z_or  at 0 range 6 .. 6;
      zyxor at 0 range 7 .. 7;
   end record;
   for accel_status_type'Size use 8;
   --
   --  Acceleration results
   --
   type accelerations is record
      x : integer;
      y : integer;
      z : integer;
   end record;
   --
   type accelerations_g is record
      x : BBS.units.accel_g;
      y : BBS.units.accel_g;
      z : BBS.units.accel_g;
   end record;
   --
   --  Magnetometer status bits
   --
   type mag_status_type is record
      drdy : Boolean;
      lock : Boolean;
   end record;
   for mag_status_type use record
      drdy at 0 range 0 .. 0;
      lock at 0 range 1 .. 1;
   end record;
   for mag_status_type'Size use 8;
   --
   --  Magnetometer results
   --
   type magnetism is record
      x : integer;
      y : integer;
      z : integer;
   end record;
   --
   type magnetism_gauss is record
      x : BBS.units.mag_g;
      y : BBS.units.mag_g;
      z : BBS.units.mag_g;
   end record;
   --
   -- Definitions for object oriented interface.
   --
   type LSM303DLHC_record is new i2c_device_record with private;
   type LSM303DLHC_ptr is access LSM303DLHC_record;
   --
   procedure configure(self : in out LSM303DLHC_record; port : i2c_interface;
                       accel : addr7; mag : addr7; error : out err_code);
   procedure configure(self : in out LSM303DLHC_record;
                       port : i2c_interface; addr_accel : addr7; addr_mag : addr7;
                       accel_fs : accel_fsd; mag_fs : mag_fsd; error : out err_code);
   --
   -- The calibrate accel procedure can be called when the sensor is stationary
   -- in a 1G acceleration or gravitational field.  It takes multiple measurements
   -- of the X, Y, and Z acceleration and computes the average of X^2 + Y^2 + Z^2.
   -- This value should be 1.0.  A more sophisticated approach would be to
   -- compute a calibration value for each of the axis separately, but that would
   -- require the sensor to be precisely positioned three time.
   --
   procedure calibrate_accel(self : in out LSM303DLHC_record);
   --
   function get_acceleration_x(self : LSM303DLHC_record; error : out err_code) return integer;
   function get_acceleration_y(self : LSM303DLHC_record; error : out err_code) return integer;
   function get_acceleration_z(self : LSM303DLHC_record; error : out err_code) return integer;
   function get_accelerations(self : LSM303DLHC_record; error : out err_code) return accelerations;
   --
   function get_acceleration_x(self : LSM303DLHC_record; error : out err_code) return BBS.units.accel_g;
   function get_acceleration_y(self : LSM303DLHC_record; error : out err_code) return BBS.units.accel_g;
   function get_acceleration_z(self : LSM303DLHC_record; error : out err_code) return BBS.units.accel_g;
   function get_accelerations(self : LSM303DLHC_record; error : out err_code) return accelerations_g;
   --
   function get_accel_status(self : LSM303DLHC_record; error : out err_code) return accel_status_type;
   function accel_data_ready(self : LSM303DLHC_record; error : out err_code) return boolean;
   --
   function get_temp(self : LSM303DLHC_record; error : out err_code) return integer;
   function get_temp(self : LSM303DLHC_record; error : out err_code) return float;
   function get_temp(self : LSM303DLHC_record; error : out err_code) return BBS.units.temp_c;
   --
   function get_magnet_x(self : LSM303DLHC_record; error : out err_code) return integer;
   function get_magnet_y(self : LSM303DLHC_record; error : out err_code) return integer;
   function get_magnet_z(self : LSM303DLHC_record; error : out err_code) return integer;
   function get_magnetism(self : LSM303DLHC_record; error : out err_code) return magnetism;
   --
   function get_magnet_x(self : LSM303DLHC_record; error : out err_code) return BBS.units.mag_g;
   function get_magnet_y(self : LSM303DLHC_record; error : out err_code) return BBS.units.mag_g;
   function get_magnet_z(self : LSM303DLHC_record; error : out err_code) return BBS.units.mag_g;
   function get_magnetism(self : LSM303DLHC_record; error : out err_code) return magnetism_gauss;
   --
   function get_mag_status(self : LSM303DLHC_record; error : out err_code) return mag_status_type;
   function mag_data_ready(self : LSM303DLHC_record; error : out err_code) return boolean;
   --
private
   --
   -- Register addresses for LSM303DLHC - accelerometer and magnetometer
   --
   -- Accelerometer
   --
   accel_ctrl1         : constant uint8 := 16#20#;
   accel_ctrl2         : constant uint8 := 16#21#;
   accel_ctrl3         : constant uint8 := 16#22#;
   accel_ctrl4         : constant uint8 := 16#23#;
   accel_ctrl5         : constant uint8 := 16#24#;
   accel_ctrl6         : constant uint8 := 16#25#;
   accel_ref           : constant uint8 := 16#26#;
   accel_status        : constant uint8 := 16#27#;
   accel_out_x_h       : constant uint8 := 16#28#;
   accel_out_x_l       : constant uint8 := 16#29#;
   accel_out_y_h       : constant uint8 := 16#2a#;
   accel_out_y_l       : constant uint8 := 16#2b#;
   accel_out_z_h       : constant uint8 := 16#2c#;
   accel_out_z_l       : constant uint8 := 16#2d#;
   accel_fifo_ctrl     : constant uint8 := 16#2e#;
   accel_fifo_src      : constant uint8 := 16#2f#;
   accel_int1_cfg      : constant uint8 := 16#30#;
   accel_int1_src      : constant uint8 := 16#31#;
   accel_int1_ths      : constant uint8 := 16#32#;
   accel_int1_duration : constant uint8 := 16#33#;
   accel_int2_cfg      : constant uint8 := 16#34#;
   accel_int2_src      : constant uint8 := 16#35#;
   accel_int2_ths      : constant uint8 := 16#36#;
   accel_int2_duration : constant uint8 := 16#37#;
   accel_click_cfg     : constant uint8 := 16#38#;
   accel_click_src     : constant uint8 := 16#39#;
   accel_click_ths     : constant uint8 := 16#3a#;
   accel_time_limit    : constant uint8 := 16#3b#;
   accel_time_latency  : constant uint8 := 16#3c#;
   accel_time_window   : constant uint8 := 16#3d#;
   --
   -- Magnetometer
   --
   mag_cra     : constant uint8 := 16#00#;
   mag_crb     : constant uint8 := 16#01#;
   mag_mr      : constant uint8 := 16#02#;
   mag_out_x_h : constant uint8 := 16#03#;
   mag_out_x_l : constant uint8 := 16#04#;
   mag_out_y_h : constant uint8 := 16#07#; -- note not ascending order
   mag_out_y_l : constant uint8 := 16#08#;
   mag_out_z_h : constant uint8 := 16#05#;
   mag_out_z_l : constant uint8 := 16#06#;
   mag_sr      : constant uint8 := 16#09#;
   mag_ira     : constant uint8 := 16#0a#;
   mag_irb     : constant uint8 := 16#0b#;
   mag_irc     : constant uint8 := 16#0c#;
   mag_temp_h  : constant uint8 := 16#31#;
   mag_temp_l  : constant uint8 := 16#32#;
   --
   -- The temperature offset is emperically determined and seems to work for my
   -- application.  You may want to check the values that you get from the
   -- temperature sensor and compare them with a calibrated thermometer to
   -- determine your own value.
   --
   temp_offset : constant Integer := 136;
   --
   -- Object definition
   --
   type LSM303DLHC_record is new i2c_device_record with record
      addr_accel   : addr7;
      addr_mag     : addr7;
      accel_scale  : Float;
      accel_calib  : Float := 1.0;
      mag_scale_xy : Float;
      mag_scale_z  : Float;
   end record;
end;
