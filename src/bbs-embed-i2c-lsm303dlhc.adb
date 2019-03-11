with BBS.embed.log;
package body BBS.embed.i2c.LSM303DLHC is
   --
   -- Do a basic configuration.  Turn on the accelerometers, magnetometers, and
   -- temperature sensor and let most everything else in the default state.
   -- Any application specific setup can be added here.
   --
--     procedure configure(error : out integer) is
--     begin
--        --
--        -- 100Hz data rate, X, Y, Z, channels enabled.
--        write(addr_accel, accel_ctrl1, 16#57#, error);
--        --
--        -- 75Hz data rate, temperature enabled.
--        write(addr_mag, mag_cra, 16#98#, error);
--        --
--        -- Full scale range is +/-1.3 gauss.
--        write(addr_mag, mag_crb, fs_1_3_gauss, error);
--        write(addr_mag, mag_mr, 16#00#, error);
--     end;
--     --
--     -- Configuration that sets full scale ranges
--     --
--     procedure configure(accel_fs : uint8; mag_fs: uint8; error : out integer) is
--     begin
--        case accel_fs is
--           when fs_2g =>
--              accel_scale := 2.0 / 32768.0;
--           when fs_4g =>
--              accel_scale := 4.0 / 32768.0;
--           when fs_8g =>
--              accel_scale := 8.0 / 32768.0;
--           when fs_16g =>
--              accel_scale := 16.0 / 32768.0;
--           when others =>
--              Ada.Text_IO.Put_Line("Unknown value for LSM303DLHC accelerometer full scale deflection");
--              raise Program_Error;
--        end case;
--        case mag_fs is
--           when fs_1_3_gauss =>
--              mag_scale_xy := 1.0 / 1100.0;
--              mag_scale_z := 1.0 / 980.0;
--           when fs_1_9_gauss =>
--              mag_scale_xy := 1.0 / 855.0;
--              mag_scale_z := 1.0 / 760.0;
--           when fs_2_5_gauss =>
--              mag_scale_xy := 1.0 / 670.0;
--              mag_scale_z := 1.0 / 600.0;
--           when fs_4_0_gauss =>
--              mag_scale_xy := 1.0 / 450.0;
--              mag_scale_z := 1.0 / 400.0;
--           when fs_4_7_gauss =>
--              mag_scale_xy := 1.0 / 400.0;
--              mag_scale_z := 1.0 / 355.0;
--           when fs_5_6_gauss =>
--              mag_scale_xy := 1.0 / 330.0;
--              mag_scale_z := 1.0 / 295.0;
--           when fs_8_1_gauss =>
--              mag_scale_xy := 1.0 / 230.0;
--              mag_scale_z := 1.0 / 205.0;
--           when others =>
--              Ada.Text_IO.Put_Line("Unknown value for LSM303DLHC magnetometer full scale deflection");
--              raise Program_Error;
--        end case;
--        --
--        -- Select accelerometer full scale range
--        write(addr_accel, accel_ctrl4, accel_fs, error);
--        --
--        -- 100Hz data rate, X, Y, Z, channels enabled.
--        write(addr_accel, accel_ctrl1, 16#57#, error);
--        --
--        -- 75Hz data rate, temperature enabled.
--       write(addr_mag, mag_cra, 16#98#, error);
--        --
--        -- Select magnetometer full scale range.
--        write(addr_mag, mag_crb, mag_fs, error);
--        write(addr_mag, mag_mr, 16#00#, error);
--     end;
--     --
--     -- A set of utility functions to get measurements from the sensors.  Note
--     -- that for this device, adding 16#80# to the register address causes the
--     -- address to automatically increment when reading multiple bytes.
--     --
--     -- The temperature is a 12 bit value returned in two 8 bit registers.  The
--     -- resolution is 8 LSB to one degree C.  Unfortunately, in testing, this
--     -- does not seem to produce a reasonable value.  The value produced is about
--     -- 20C too low.
--     --
--     function get_temperature(error : out integer) return integer is
--        word : uint16 := readm1(addr_mag, mag_temp_h, error);
--     begin
--        return integer(uint16_to_int16(word)/16);
--     end;
--     --
--     function get_temperature(error : out integer) return float is
--        temp : integer := get_temperature(error);
--     begin
--        return float(temperature_offset + temp)/8.0;
--     end;
--     --
--     function get_temperature(error : out integer) return BBS.units.temp_c is
--        temp : integer := get_temperature(error);
--     begin
--        return BBS.units.temp_c(float(temperature_offset + temp)/8.0);
--     end;
--     --
--     function get_acceleration_x(error : out integer) return integer is
--        word : uint16 := readm2(addr_accel, accel_out_x_h + 16#80#, error);
--     begin
--        return Integer(uint16_to_int16(word));
--     end;
--     --
--     function get_acceleration_y(error : out integer) return integer is
--        word : uint16 := readm2(addr_accel, accel_out_y_h + 16#80#, error);
--     begin
--        return Integer(uint16_to_int16(word));
--     end;
--     --
--     function get_acceleration_z(error : out integer) return integer is
--        word : uint16 := readm2(addr_accel, accel_out_z_h + 16#80#, error);
--     begin
--        return Integer(uint16_to_int16(word));
--     end;
--     --
--     function get_accelerations(error : out integer) return accelerations is
--        accel : accelerations;
--     begin
--        read(addr_accel, accel_out_x_h + 16#80#, buff'access, 6, error);
--        accel.x := Integer(uint16_to_int16(uint16(buff(0)) + uint16(buff(1))*256));
--        accel.y := Integer(uint16_to_int16(uint16(buff(2)) + uint16(buff(3))*256));
--        accel.z := Integer(uint16_to_int16(uint16(buff(4)) + uint16(buff(5))*256));
--        return accel;
--     end;
--     --
--     function get_acceleration_x(error : out integer) return BBS.units.accel_g is
--        accel : integer := get_acceleration_x(error);
--     begin
--        return BBS.units.accel_g(float(accel) * accel_scale);
--     end;
--     --
--     function get_acceleration_y(error : out integer) return BBS.units.accel_g is
--        accel : integer := get_acceleration_y(error);
--     begin
--        return BBS.units.accel_g(float(accel) * accel_scale);
--     end;
--     --
--     function get_acceleration_z(error : out integer) return BBS.units.accel_g is
--        accel : integer := get_acceleration_z(error);
--     begin
--        return BBS.units.accel_g(float(accel) * accel_scale);
--     end;
--     --
--     function get_accelerations(error : out integer) return accelerations_g is
--        accel : accelerations := get_accelerations(error);
--        accel_gs : accelerations_g;
--     begin
--        accel_gs.x := BBS.units.accel_g(float(accel.x) * accel_scale);
--        accel_gs.y := BBS.units.accel_g(float(accel.y) * accel_scale);
--        accel_gs.z := BBS.units.accel_g(float(accel.z) * accel_scale);
--        return accel_gs;
--     end;
--     --
--     function get_accel_status(error : out integer) return uint8 is
--     begin
--        return read(addr_accel, accel_status, error);
--     end;
--     --
--     function accel_data_ready(error : out integer) return boolean is
--        err : integer;
--        byte : uint8 := read(addr_accel, accel_status, err);
--     begin
--        error := err;
--        if ((byte and accel_stat_zyxda) = accel_stat_zyxda) and (err = 0) then
--           return true;
--        else
--           return false;
--        end if;
--     end;
--     --
--     function get_magnet_x(error : out integer) return integer is
--        word : uint16 := readm1(addr_mag, mag_out_x_h, error);
--     begin
--        return Integer(uint16_to_int16(word));
--     end;
--     --
--     function get_magnet_y(error : out integer) return integer is
--        word : uint16 := readm1(addr_mag, mag_out_y_h, error);
--     begin
--        return Integer(uint16_to_int16(word));
--     end;
--     --
--     function get_magnet_z(error : out integer) return integer is
--        word : uint16 := readm1(addr_mag, mag_out_z_h, error);
--     begin
--        return Integer(uint16_to_int16(word));
--     end;
--     --
--     function get_magnetism(error : out integer) return magnetism is
--        mag : magnetism;
--     begin
--        read(addr_mag, mag_out_x_h, buff'access, 6, error);
--        mag.x := Integer(uint16_to_int16(uint16(buff(1)) + uint16(buff(0))*256));
--        mag.z := Integer(uint16_to_int16(uint16(buff(3)) + uint16(buff(2))*256));
--        mag.y := Integer(uint16_to_int16(uint16(buff(5)) + uint16(buff(4))*256));
--        return mag;
--     end;
--     --
--     function get_magnet_x(error : out integer) return BBS.units.mag_g is
--        mag : integer := get_magnet_x(error);
--     begin
--        return BBS.units.mag_g(float(mag) * mag_scale_xy);
--     end;
--     --
--     function get_magnet_y(error : out integer) return BBS.units.mag_g is
--        mag : integer := get_magnet_y(error);
--     begin
--        return BBS.units.mag_g(float(mag) * mag_scale_xy);
--     end;
--     --
--     function get_magnet_z(error : out integer) return BBS.units.mag_g is
--        mag : integer := get_magnet_z(error);
--     begin
--        return BBS.units.mag_g(float(mag) * mag_scale_z);
--     end;
--     --
--     function get_magnetism(error : out integer) return magnetism_gauss is
--        mag : magnetism := get_magnetism(error);
--        mag_g : magnetism_gauss;
--     begin
--        mag_g.x := BBS.units.mag_g(float(mag.x) * mag_scale_xy);
--        mag_g.y := BBS.units.mag_g(float(mag.y) * mag_scale_xy);
--        mag_g.z := BBS.units.mag_g(float(mag.z) * mag_scale_z);
--        return mag_g;
--     end;
--     --
--     function get_mag_status(error : out integer) return uint8 is
--     begin
--        return read(addr_mag, mag_sr, error);
--     end;
--     --
--     function mag_data_ready(error : out integer) return boolean is
--        err : integer;
--        byte : uint8 := read(addr_mag, mag_sr, err);
--     begin
--        error := err;
--        if ((byte and mag_drdy) = mag_drdy) and (err = 0) then
--           return true;
--        else
--           return false;
--        end if;
--     end;
--     --
--     -- Object oriented interface.  This basically emulates the standard interface
--     -- above.
--     --
--     function i2c_new return LSM303DLHC_ptr is
--   begin
--      return new LSM303DLHC_record;
--   end;
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
   function get_temperature(self : LSM303DLHC_record;
                            error : out err_code) return integer is
      word : uint16 := self.hw.readm1(addr_mag, mag_temp_h, error);
   begin
      return integer(uint16_to_int16(word)/16);
   end;
   --
   function get_temperature(self : LSM303DLHC_record;
                            error : out err_code) return float is
      temp : integer := self.get_temperature(error);
   begin
      return float(self.temp_offset + temp)/8.0;
   end;
   --
   function get_temperature(self : LSM303DLHC_record;
                            error : out err_code) return BBS.units.temp_c is
      temp : integer := self.get_temperature(error);
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
