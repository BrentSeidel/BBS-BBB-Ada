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
-- This package spec and body provide an interface to the L3GD20H gyroscope.  They
-- are largely based on the datasheet for the device.
--
with BBS.embed.i2c;
with BBS.units;
--
-- This package contains constants and routines to communicate with the L3GD20H
-- gyroscope on the i2c bus.
--
-- The interface is fairly basic and doesn't use the advanced features of the
-- device.  If you wish a more sophisticated interface, this could provide a
-- useful starting point.
--
package BBS.embed.i2c.L3GD20H is
   --
   -- Define some datatypes
   --
   -- Full scale selection
   --
   type fsd is (fs_245dps, fs_500dps, fs_2000dps);
   for fsd use (fs_245dps => 16#00#, fs_500dps => 16#10#, fs_2000dps => 16#20#);
   for fsd'Size use 8;
   --
   -- Status bits
   --
   type status_type is record
      xda   : Boolean;
      yda   : Boolean;
      zda   : Boolean;
      zyxda : Boolean;
      x_or  : Boolean;
      y_or  : Boolean;
      z_or  : Boolean;
      zyxor : Boolean;
   end record;
   for status_type use record
      xda   at 0 range 0 .. 0;
      yda   at 0 range 1 .. 1;
      zda   at 0 range 2 .. 2;
      zyxda at 0 range 3 .. 3;
      x_or  at 0 range 4 .. 4;
      y_or  at 0 range 5 .. 5;
      z_or  at 0 range 6 .. 6;
      zyxor at 0 range 7 .. 7;
   end record;
   for status_type'Size use 8;
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
         x : BBS.units.rot_d_s;
         y : BBS.units.rot_d_s;
         z : BBS.units.rot_d_s;
      end record;
   --
   -- Stuff for object oriented interface.
   --
   type L3GD20H_record is new i2c_device_record with private;
   type L3GD20H_ptr is access L3GD20H_record;
   --
   procedure configure(self : in out L3GD20H_record; port : i2c_interface;
                       addr : addr7; error : out err_code);
   procedure configure(self : in out L3GD20H_record; port : i2c_interface;
                       addr : addr7; deflection : fsd; error : out err_code);
   --
   --  Check to see if the configured device is present.
   --
   function present(self : in out L3GD20H_record) return boolean;
   --
   function get_temp(self : L3GD20H_record; error : out err_code) return integer;
   function get_rotation_x(self : L3GD20H_record; error : out err_code) return integer;
   function get_rotation_y(self : L3GD20H_record; error : out err_code) return integer;
   function get_rotation_z(self : L3GD20H_record; error : out err_code) return integer;
   function get_rotations(self : L3GD20H_record; error : out err_code) return rotations;
   --
   function get_temp(self : L3GD20H_record; error : out err_code) return BBS.units.temp_c;
   function get_rotation_x(self : L3GD20H_record; error : out err_code) return BBS.units.rot_d_s;
   function get_rotation_y(self : L3GD20H_record; error : out err_code) return BBS.units.rot_d_s;
   function get_rotation_z(self : L3GD20H_record; error : out err_code) return BBS.units.rot_d_s;
   function get_rotations(self : L3GD20H_record; error : out err_code) return rotations_dps;
   --
   function get_status(self : L3GD20H_record; error : out err_code) return status_type;
   function data_ready(self : L3GD20H_record; error : out err_code) return boolean;
   --
   -- When stationary, the sensors may not report 0.  This function should be
   -- called when the sensor is stationary.  It reads the rotations several times
   -- and averages the results.  This is used to calculate offset values.
   --
   -- This function returns true if the measurement was successful - that is all
   -- of the values measured are reasonably close to the mean.  If it returns false,
   -- the sensor may be moving.
   --
   function measure_offsets(self : in out L3GD20H_record) return boolean;
   --
private
   --
   -- Addresses for L3GD20H - gyroscope
   --
   who_am_i    : constant uint8 := 16#0f#;
   ctrl1       : constant uint8 := 16#20#;
   ctrl2       : constant uint8 := 16#21#;
   ctrl3       : constant uint8 := 16#22#;
   ctrl4       : constant uint8 := 16#23#;
   ctrl5       : constant uint8 := 16#24#;
   ref         : constant uint8 := 16#25#;
   out_temp    : constant uint8 := 16#26#;
   status      : constant uint8 := 16#27#;
   out_x_l     : constant uint8 := 16#28#;
   out_x_h     : constant uint8 := 16#29#;
   out_y_l     : constant uint8 := 16#2a#;
   out_y_h     : constant uint8 := 16#2b#;
   out_z_l     : constant uint8 := 16#2c#;
   out_z_h     : constant uint8 := 16#2d#;
   fifo_ctrl   : constant uint8 := 16#2e#;
   fifo_src    : constant uint8 := 16#2f#;
   ig_cfg      : constant uint8 := 16#30#;
   ig_src      : constant uint8 := 16#31#;
   ig_ths_xh   : constant uint8 := 16#32#;
   ig_ths_xl   : constant uint8 := 16#33#;
   ig_ths_yh   : constant uint8 := 16#34#;
   ig_ths_yl   : constant uint8 := 16#35#;
   ig_ths_zh   : constant uint8 := 16#36#;
   ig_ths_zl   : constant uint8 := 16#37#;
   ig_duration : constant uint8 := 16#38#;
   low_odr     : constant uint8 := 16#39#;
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
      temp_offset : integer := 37;
      offset_x : integer := 0;
      offset_y : integer := 0;
      offset_z : integer := 0;
      scale : float := 245.0/32767.0;
   end record;
   --
   -- Set this to True to display some debugging information.  Set it to False
   -- to eliminate outputs.
   --
   debug : constant boolean := true;
end;
