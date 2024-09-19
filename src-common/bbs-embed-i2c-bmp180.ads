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
--
--  This package contains constants and routines to communicate with the BMP180
--  temperature and pressure on the i2c bus.
--
--  The interface is fairly basic and doesn't use the advanced features of the
--  device.  If you wish a more sophisticated interface, this could provide a
--  useful starting point.
--
--  Please refer to the BMP180 datasheet for more information.
--
--  Note that the BMP180 has been discontinued by its manufacturer.  This
--  Driver probably won't get much attention or testing.
--
package BBS.embed.i2c.BMP180 is
   --
   --  Conversion start commands.
   --
   type cvt_type is (cvt_null, cvt_temp, cvt_press0, cvt_press1, cvt_press2, cvt_press3);
   for cvt_type use (cvt_null => 0, cvt_temp => 16#2e#, cvt_press0 => 16#34#, cvt_press1 => 16#74#,
                     cvt_press2 => 16#b4#, cvt_press3 => 16#f4#);
   for cvt_type'Size use 8;
   --
   type BMP180_record is new i2c_device_record with private;
   type BMP180_ptr is access BMP180_record;
   --
   procedure configure(self : in out BMP180_record; port : i2c_interface;
                       addr : addr7; error : out err_code);
   --
   procedure start_conversion(self : in out BMP180_record; kind : cvt_type; error : out err_code);
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
   --
   --  Addresses for the BMP180 pressure and temperature sensor
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
   --  Measurement control register.  This is poorly documented.  Some
   --  fields are explained, but the measurement control field isn't.
   --  Constants are given for different types of conversions (see cvt_type).
   --
   type ctrl_meas_type is record
      meas_ctrl : uint5;    --  Measurement control?
      sco       : Boolean;  --  Start of conversion
      oss       : uint2;    --  Oversampling
   end record;
   for ctrl_meas_type use record
      meas_ctrl at 0 range 0 .. 4;
      sco       at 0 range 5 .. 5;
      oss       at 0 range 6 .. 7;
   end record;
   for ctrl_meas_type'Size use 8;
   --
   type BMP180_record is new i2c_device_record with record
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
      last_cvt : cvt_type := cvt_null;
   end record;

end;
