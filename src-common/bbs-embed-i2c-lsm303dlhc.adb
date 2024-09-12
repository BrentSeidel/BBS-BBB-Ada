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
with BBS.embed.log;
package body BBS.embed.i2c.LSM303DLHC is
   --
   procedure configure(self : in out LSM303DLHC_record; port : i2c_interface;
                       accel : addr7; mag : addr7; error : out err_code) is
   begin
      self.hw := port;
      self.addr_accel := accel;
      self.addr_mag := mag;
      --
      -- 100Hz data rate, X, Y, Z, channels enabled.
      self.hw.write(self.addr_accel, accel_ctrl1, uint8(16#57#), error);
      --
      -- 75Hz data rate, temperature enabled.
      self.hw.write(self.addr_mag, mag_cra, uint8(16#98#), error);
      --
      -- Full scale range is +/-1.3 gauss.
      self.hw.write(self.addr_mag, mag_crb, fs_1_3_gauss, error);
      self.hw.write(self.addr_mag, mag_mr, uint8(16#00#), error);
   end;
   --
   procedure configure(self : in out LSM303DLHC_record;
                       port : i2c_interface; addr_accel : addr7; addr_mag : addr7;
                       accel_fs : uint8; mag_fs : uint8; error : out err_code) is
   begin
      self.hw := port;
      self.addr_accel := addr_accel;
      self.addr_mag := addr_mag;
      --
      case accel_fs is
         when fs_2g =>
            accel_scale := 2.0 / 32768.0;
         when fs_4g =>
            accel_scale := 4.0 / 32768.0;
         when fs_8g =>
            accel_scale := 8.0 / 32768.0;
         when fs_16g =>
            accel_scale := 16.0 / 32768.0;
         when others =>
            BBS.embed.log.error.Put_Line("Unknown value for LSM303DLHC accelerometer full scale deflection");
            raise Program_Error;
      end case;
      case mag_fs is
         when fs_1_3_gauss =>
            mag_scale_xy := 1.0 / 1100.0;
            mag_scale_z := 1.0 / 980.0;
         when fs_1_9_gauss =>
            mag_scale_xy := 1.0 / 855.0;
            mag_scale_z := 1.0 / 760.0;
         when fs_2_5_gauss =>
            mag_scale_xy := 1.0 / 670.0;
            mag_scale_z := 1.0 / 600.0;
         when fs_4_0_gauss =>
            mag_scale_xy := 1.0 / 450.0;
            mag_scale_z := 1.0 / 400.0;
         when fs_4_7_gauss =>
            mag_scale_xy := 1.0 / 400.0;
            mag_scale_z := 1.0 / 355.0;
         when fs_5_6_gauss =>
            mag_scale_xy := 1.0 / 330.0;
            mag_scale_z := 1.0 / 295.0;
         when fs_8_1_gauss =>
            mag_scale_xy := 1.0 / 230.0;
            mag_scale_z := 1.0 / 205.0;
         when others =>
            BBS.embed.log.error.Put_Line("Unknown value for LSM303DLHC magnetometer full scale deflection");
            raise Program_Error;
      end case;
      --
      -- Select accelerometer full scale range
      self.hw.write(self.addr_accel, accel_ctrl4, accel_fs, error);
      --
      -- 100Hz data rate, X, Y, Z, channels enabled.
      self.hw.write(self.addr_accel, accel_ctrl1, uint8(16#57#), error);
      --
      -- 75Hz data rate, temperature enabled.
      self.hw.write(self.addr_mag, mag_cra, uint8(16#98#), error);
      --
      -- Select magnetometer full scale range.
      self.hw.write(self.addr_mag, mag_crb, mag_fs, error);
      self.hw.write(self.addr_mag, mag_mr, uint8(16#00#), error);
   end;
   --
   procedure calibrate_accel(self : in out LSM303DLHC_record) is
      sum_sq : float := 0.0;
      accel : accelerations_g;
      samples : constant integer := 100;
      err : err_code;
   begin
      for i in 1 .. samples loop
         loop
            exit when self.accel_data_ready(err);
         end loop;
         accel := self.get_accelerations(err);
         sum_sq := sum_sq + Math.Sqrt(float(accel.x*accel.x) + float(accel.y*accel.y) +
           float(accel.z*accel.z));
      end loop;
      self.accel_calib := 1.0 / (sum_sq/float(samples));
      BBS.embed.log.info.put_line("Acceleration calibration is: <" & Float'Image(self.accel_calib) &
                            ">");
   end;
   --
   function get_acceleration_x(self : LSM303DLHC_record;
                               error : out err_code) return integer is
      word : uint16 := self.hw.readm2(self.addr_accel, accel_out_x_h + 16#80#, error);
   begin
      return Integer(uint16_to_int16(word));
   end;
   --
   function get_acceleration_y(self : LSM303DLHC_record;
                               error : out err_code) return integer is
      word : uint16 := self.hw.readm2(self.addr_accel, accel_out_y_h + 16#80#, error);
   begin
      return Integer(uint16_to_int16(word));
   end;
   --
   function get_acceleration_z(self : LSM303DLHC_record;
                               error : out err_code) return integer is
      word : uint16 := self.hw.readm2(self.addr_accel, accel_out_z_h + 16#80#, error);
   begin
      return Integer(uint16_to_int16(word));
   end;
   --
   function get_accelerations(self : LSM303DLHC_record;
                              error : out err_code) return accelerations is
      accel : accelerations;
   begin
      self.hw.read(self.addr_accel, accel_out_x_h + 16#80#, buff_index(6), error);
      accel.x := Integer(uint16_to_int16(uint16(self.hw.b(0)) + uint16(self.hw.b(1))*256));
      accel.y := Integer(uint16_to_int16(uint16(self.hw.b(2)) + uint16(self.hw.b(3))*256));
      accel.z := Integer(uint16_to_int16(uint16(self.hw.b(4)) + uint16(self.hw.b(5))*256));
      return accel;
   end;
   --
   --
   function get_acceleration_x(self : LSM303DLHC_record;
                               error : out err_code) return BBS.units.accel_g is
      accel : integer := self.get_acceleration_x(error);
   begin
      return BBS.units.accel_g(float(accel) * self.accel_scale * self.accel_calib);
   end;
   --
   function get_acceleration_y(self : LSM303DLHC_record;
                               error : out err_code) return BBS.units.accel_g is
      accel : integer := self.get_acceleration_y(error);
   begin
      return BBS.units.accel_g(float(accel) * self.accel_scale * self.accel_calib);
   end;
   --
   function get_acceleration_z(self : LSM303DLHC_record;
                               error : out err_code) return BBS.units.accel_g is
      accel : integer := self.get_acceleration_z(error);
   begin
      return BBS.units.accel_g(float(accel) * self.accel_scale * self.accel_calib);
   end;
   --
   function get_accelerations(self : LSM303DLHC_record;
                              error : out err_code) return accelerations_g is
      accel : accelerations := self.get_accelerations(error);
      accel_gs : accelerations_g;
   begin
      accel_gs.x := BBS.units.accel_g(float(accel.x) * self.accel_scale * self.accel_calib);
      accel_gs.y := BBS.units.accel_g(float(accel.y) * self.accel_scale * self.accel_calib);
      accel_gs.z := BBS.units.accel_g(float(accel.z) * self.accel_scale * self.accel_calib);
      return accel_gs;
   end;
   --
   function get_accel_status(self : LSM303DLHC_record;
                             error : out err_code) return uint8 is
   begin
      return self.hw.read(addr_accel, accel_status, error);
   end;
   --
   function accel_data_ready(self : LSM303DLHC_record;
                             error : out err_code) return boolean is
      err : err_code;
      byte : uint8 := self.hw.read(addr_accel, accel_status, err);
   begin
      error := err;
      if ((byte and accel_stat_zyxda) = accel_stat_zyxda) and (err = none) then
         return true;
      else
         return false;
      end if;
   end;
   --
   function get_temp(self : LSM303DLHC_record;
                     error : out err_code) return integer is
      word : uint16 := self.hw.readm1(addr_mag, mag_temp_h, error);
   begin
      return integer(uint16_to_int16(word)/16);
   end;
   --
   function get_temp(self : LSM303DLHC_record;
                     error : out err_code) return float is
      temp : integer := self.get_temp(error);
   begin
      return float(self.temp_offset + temp)/8.0;
   end;
   --
   function get_temp(self : LSM303DLHC_record;
                     error : out err_code) return BBS.units.temp_c is
      temp : integer := self.get_temp(error);
   begin
      return BBS.units.temp_c(float(self.temp_offset + temp)/8.0);
   end;
   --
   function get_magnet_x(self : LSM303DLHC_record;
                         error : out err_code) return integer is
      word : uint16 := self.hw.readm1(self.addr_mag, mag_out_x_h, error);
   begin
      return Integer(uint16_to_int16(word));
   end;
   --
   function get_magnet_y(self : LSM303DLHC_record;
                         error : out err_code) return integer is
      word : uint16 := self.hw.readm1(self.addr_mag, mag_out_y_h, error);
   begin
      return Integer(uint16_to_int16(word));
   end;
   --
   function get_magnet_z(self : LSM303DLHC_record;
                         error : out err_code) return integer is
      word : uint16 := self.hw.readm1(self.addr_mag, mag_out_z_h, error);
   begin
      return Integer(uint16_to_int16(word));
   end;
   --
   function get_magnetism(self : LSM303DLHC_record;
                          error : out err_code) return magnetism is
      mag : magnetism;
   begin
      self.hw.read(self.addr_mag, mag_out_x_h, buff_index(6), error);
      mag.x := Integer(uint16_to_int16(uint16(self.hw.b(1)) + uint16(self.hw.b(0))*256));
      mag.z := Integer(uint16_to_int16(uint16(self.hw.b(3)) + uint16(self.hw.b(2))*256));
      mag.y := Integer(uint16_to_int16(uint16(self.hw.b(5)) + uint16(self.hw.b(4))*256));
      return mag;
   end;
   --
   function get_magnet_x(self : LSM303DLHC_record;
                         error : out err_code) return BBS.units.mag_g is
      mag : integer := self.get_magnet_x(error);
   begin
      return BBS.units.mag_g(float(mag) * self.mag_scale_xy);
   end;
   --
   function get_magnet_y(self : LSM303DLHC_record;
                         error : out err_code) return BBS.units.mag_g is
      mag : integer := self.get_magnet_y(error);
   begin
      return BBS.units.mag_g(float(mag) * self.mag_scale_xy);
   end;
   --
      function get_magnet_z(self : LSM303DLHC_record;
                            error : out err_code) return BBS.units.mag_g is
      mag : integer := self.get_magnet_z(error);
   begin
      return BBS.units.mag_g(float(mag) * self.mag_scale_z);
   end;
   --
      function get_magnetism(self : LSM303DLHC_record;
                             error : out err_code) return magnetism_gauss is
      mag : magnetism := self.get_magnetism(error);
      mag_g : magnetism_gauss;
   begin
      mag_g.x := BBS.units.mag_g(float(mag.x) * self.mag_scale_xy);
      mag_g.y := BBS.units.mag_g(float(mag.y) * self.mag_scale_xy);
      mag_g.z := BBS.units.mag_g(float(mag.z) * self.mag_scale_z);
      return mag_g;
   end;
   --
   function get_mag_status(self : LSM303DLHC_record;
                           error : out err_code) return uint8 is
   begin
      return self.hw.read(self.addr_mag, mag_sr, error);
   end;
   --
   function mag_data_ready(self : LSM303DLHC_record;
                           error : out err_code) return boolean is
      err : err_code;
      byte : uint8 := self.hw.read(self.addr_mag, mag_sr, err);
   begin
      error := err;
      if ((byte and mag_drdy) = mag_drdy) and (err = none) then
         return true;
      else
         return false;
      end if;
   end;
   --
end;
