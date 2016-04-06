package body BBS.BBB.i2c.LSM303DLHC is
   --
   -- Do a basic configuration.  Turn on the accelerometers, magnetometers, and
   -- temperature sensor and let most everything else in the default state.
   -- Any application specific setup can be added here.
   --
   procedure configure(error : out integer) is
   begin
      --
      -- 100Hz data rate, X, Y, Z, channels enabled.
      BBS.BBB.i2c.write(addr_accel, accel_ctrl1, 16#57#, error);
      --
      -- 75Hz data rate, temperature enabled.
      BBS.BBB.i2c.write(addr_mag, mag_cra, 16#98#, error);
      --
      -- Full scale range is +/-1.3 gauss.
      BBS.BBB.i2c.write(addr_mag, mag_crb, fs_1_3_gauss, error);
      BBS.BBB.i2c.write(addr_mag, mag_mr, 16#00#, error);
   end;
   --
   -- Configuration that sets full scale ranges
   --
   procedure configure(accel_fs : uint8; mag_fs: uint8; error : out integer) is
   begin
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
            Ada.Text_IO.Put_Line("Unknown value for LSM303DLHC accelerometer full scale deflection");
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
            Ada.Text_IO.Put_Line("Unknown value for LSM303DLHC magnetometer full scale deflection");
            raise Program_Error;
      end case;
      --
      -- Select accelerometer full scale range
      BBS.BBB.i2c.write(addr_accel, accel_ctrl4, accel_fs, error);
      --
      -- 100Hz data rate, X, Y, Z, channels enabled.
      BBS.BBB.i2c.write(addr_accel, accel_ctrl1, 16#57#, error);
      --
      -- 75Hz data rate, temperature enabled.
      BBS.BBB.i2c.write(addr_mag, mag_cra, 16#98#, error);
      --
      -- Select magnetometer full scale range.
      BBS.BBB.i2c.write(addr_mag, mag_crb, mag_fs, error);
      BBS.BBB.i2c.write(addr_mag, mag_mr, 16#00#, error);
   end;
   --
   -- A set of utility functions to get measurements from the sensors.  Note
   -- that for this device, adding 16#80# to the register address causes the
   -- address to automatically increment when reading multiple bytes.
   --
   -- The temperature is a 12 bit value returned in two 8 bit registers.  The
   -- resolution is 8 LSB to one degree C.  Unfortunately, in testing, this
   -- does not seem to produce a reasonable value.  The value produced is about
   -- 20C too low.
   --
   function get_temperature(error : out integer) return integer is
      word : uint16 := BBS.BBB.i2c.readm1(addr_mag, mag_temp_h, error);
   begin
      return integer(BBS.BBB.uint16_to_int16(word)/16);
   end;
   --
   function get_temperature(error : out integer) return float is
      temp : integer := get_temperature(error);
   begin
      return float(temperature_offset + temp)/8.0;
   end;
   --
   function get_temperature(error : out integer) return BBS.units.temp_c is
      temp : integer := get_temperature(error);
   begin
      return BBS.units.temp_c(float(temperature_offset + temp)/8.0);
   end;
   --
   function get_acceleration_x(error : out integer) return integer is
      word : uint16 := BBS.BBB.i2c.readm2(addr_accel, accel_out_x_h + 16#80#, error);
   begin
      return Integer(BBS.BBB.uint16_to_int16(word));
   end;
   --
   function get_acceleration_y(error : out integer) return integer is
      word : uint16 := BBS.BBB.i2c.readm2(addr_accel, accel_out_y_h + 16#80#, error);
   begin
      return Integer(BBS.BBB.uint16_to_int16(word));
   end;
   --
   function get_acceleration_z(error : out integer) return integer is
      word : uint16 := BBS.BBB.i2c.readm2(addr_accel, accel_out_z_h + 16#80#, error);
   begin
      return Integer(BBS.BBB.uint16_to_int16(word));
   end;
   --
   function get_accelerations(error : out integer) return accelerations is
      accel : accelerations;
   begin
      read(addr_accel, accel_out_x_h + 16#80#, buff'access, 6, error);
      accel.x := Integer(uint16_to_int16(uint16(buff(0)) + uint16(buff(1))*256));
      accel.y := Integer(uint16_to_int16(uint16(buff(2)) + uint16(buff(3))*256));
      accel.z := Integer(uint16_to_int16(uint16(buff(4)) + uint16(buff(5))*256));
      return accel;
   end;
   --
   function get_acceleration_x(error : out integer) return BBS.units.accel_g is
      accel : integer := get_acceleration_x(error);
   begin
      return BBS.units.accel_g(float(accel) * accel_scale);
   end;
   --
   function get_acceleration_y(error : out integer) return BBS.units.accel_g is
      accel : integer := get_acceleration_y(error);
   begin
      return BBS.units.accel_g(float(accel) * accel_scale);
   end;
   --
   function get_acceleration_z(error : out integer) return BBS.units.accel_g is
      accel : integer := get_acceleration_z(error);
   begin
      return BBS.units.accel_g(float(accel) * accel_scale);
   end;
   --
   function get_accelerations(error : out integer) return accelerations_g is
      accel : accelerations := get_accelerations(error);
      accel_gs : accelerations_g;
   begin
      accel_gs.x := BBS.units.accel_g(float(accel.x) * accel_scale);
      accel_gs.y := BBS.units.accel_g(float(accel.y) * accel_scale);
      accel_gs.z := BBS.units.accel_g(float(accel.z) * accel_scale);
      return accel_gs;
   end;
   --
   function get_accel_status(error : out integer) return uint8 is
   begin
      return BBS.BBB.i2c.read(addr_accel, accel_status, error);
   end;
   --
   function accel_data_ready(error : out integer) return boolean is
      err : integer;
      byte : uint8 := BBS.BBB.i2c.read(addr_accel, accel_status, err);
   begin
      error := err;
      if ((byte and accel_stat_zyxda) = accel_stat_zyxda) and (err = 0) then
         return true;
      else
         return false;
      end if;
   end;
   --
   function get_magnet_x(error : out integer) return integer is
      word : uint16 := BBS.BBB.i2c.readm1(addr_mag, mag_out_x_h, error);
   begin
      return Integer(BBS.BBB.uint16_to_int16(word));
   end;
   --
   function get_magnet_y(error : out integer) return integer is
      word : uint16 := BBS.BBB.i2c.readm1(addr_mag, mag_out_y_h, error);
   begin
      return Integer(BBS.BBB.uint16_to_int16(word));
   end;
   --
   function get_magnet_z(error : out integer) return integer is
      word : uint16 := BBS.BBB.i2c.readm1(addr_mag, mag_out_z_h, error);
   begin
      return Integer(BBS.BBB.uint16_to_int16(word));
   end;
   --
   function get_magnetism(error : out integer) return magnetism is
      mag : magnetism;
   begin
      read(addr_mag, mag_out_x_h, buff'access, 6, error);
      mag.x := Integer(uint16_to_int16(uint16(buff(1)) + uint16(buff(0))*256));
      mag.z := Integer(uint16_to_int16(uint16(buff(3)) + uint16(buff(2))*256));
      mag.y := Integer(uint16_to_int16(uint16(buff(5)) + uint16(buff(4))*256));
      return mag;
   end;
   --
   function get_magnet_x(error : out integer) return BBS.units.mag_g is
      mag : integer := get_magnet_x(error);
   begin
      return BBS.units.mag_g(float(mag) * mag_scale_xy);
   end;
   --
   function get_magnet_y(error : out integer) return BBS.units.mag_g is
      mag : integer := get_magnet_y(error);
   begin
      return BBS.units.mag_g(float(mag) * mag_scale_xy);
   end;
   --
   function get_magnet_z(error : out integer) return BBS.units.mag_g is
      mag : integer := get_magnet_z(error);
   begin
      return BBS.units.mag_g(float(mag) * mag_scale_z);
   end;
   --
   function get_magnetism(error : out integer) return magnetism_gauss is
      mag : magnetism := get_magnetism(error);
      mag_g : magnetism_gauss;
   begin
      mag_g.x := BBS.units.mag_g(float(mag.x) * mag_scale_xy);
      mag_g.y := BBS.units.mag_g(float(mag.y) * mag_scale_xy);
      mag_g.z := BBS.units.mag_g(float(mag.z) * mag_scale_z);
      return mag_g;
   end;
   --
   function get_mag_status(error : out integer) return uint8 is
   begin
      return BBS.BBB.i2c.read(addr_mag, mag_sr, error);
   end;
   --
   function mag_data_ready(error : out integer) return boolean is
      err : integer;
      byte : uint8 := BBS.BBB.i2c.read(addr_mag, mag_sr, err);
   begin
      error := err;
      if ((byte and mag_drdy) = mag_drdy) and (err = 0) then
         return true;
      else
         return false;
      end if;
   end;
   --
   -- Object oriented interface.  This basically emulates the standard interface
   -- above.
   --
   function i2c_new return LSM303DLHC_ptr is
   begin
      return new LSM303DLHC_record;
   end;
   --
   procedure configure(self : not null access LSM303DLHC_record'class; port : i2c_interface;
                       accel : addr7; mag : addr7; error : out integer) is
   begin
      self.port := port;
      self.addr_accel := accel;
      self.addr_mag := mag;
      --
      -- 100Hz data rate, X, Y, Z, channels enabled.
      self.port.write(self.addr_accel, accel_ctrl1, 16#57#, error);
      --
      -- 75Hz data rate, temperature enabled.
      self.port.write(self.addr_mag, mag_cra, 16#98#, error);
      --
      -- Full scale range is +/-1.3 gauss.
      self.port.write(self.addr_mag, mag_crb, fs_1_3_gauss, error);
      self.port.write(self.addr_mag, mag_mr, 16#00#, error);
   end;
   --
   procedure configure(self : not null access LSM303DLHC_record'class;
                       port : i2c_interface; addr_accel : addr7; addr_mag : addr7;
                       accel_fs : uint8; mag_fs : uint8; error : out integer) is
   begin
      self.port := port;
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
            Ada.Text_IO.Put_Line("Unknown value for LSM303DLHC accelerometer full scale deflection");
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
            Ada.Text_IO.Put_Line("Unknown value for LSM303DLHC magnetometer full scale deflection");
            raise Program_Error;
      end case;
      --
      -- Select accelerometer full scale range
      self.port.write(self.addr_accel, accel_ctrl4, accel_fs, error);
      --
      -- 100Hz data rate, X, Y, Z, channels enabled.
      self.port.write(self.addr_accel, accel_ctrl1, 16#57#, error);
      --
      -- 75Hz data rate, temperature enabled.
      self.port.write(self.addr_mag, mag_cra, 16#98#, error);
      --
      -- Select magnetometer full scale range.
      self.port.write(self.addr_mag, mag_crb, mag_fs, error);
      self.port.write(self.addr_mag, mag_mr, 16#00#, error);
   end;
   --
   procedure calibrate_accel(self : not null access LSM303DLHC_record'class) is
      sum_sq : float := 0.0;
      accel : accelerations_g;
      samples : constant integer := 100;
      err : integer;
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
      Ada.Text_IO.Put("Acceleration calibration is: <");
      Ada.Float_Text_IO.Put(self.accel_calib, 1, 2, 0);
      Ada.Text_IO.Put_Line(">");
   end;
   --
   function get_acceleration_x(self : not null access LSM303DLHC_record'class;
                               error : out integer) return integer is
      word : uint16 := self.port.readm2(self.addr_accel, accel_out_x_h + 16#80#, error);
   begin
      return Integer(BBS.BBB.uint16_to_int16(word));
   end;
   --
   function get_acceleration_y(self : not null access LSM303DLHC_record'class;
                               error : out integer) return integer is
      word : uint16 := self.port.readm2(self.addr_accel, accel_out_y_h + 16#80#, error);
   begin
      return Integer(BBS.BBB.uint16_to_int16(word));
   end;
   --
   function get_acceleration_z(self : not null access LSM303DLHC_record'class;
                               error : out integer) return integer is
      word : uint16 := self.port.readm2(self.addr_accel, accel_out_z_h + 16#80#, error);
   begin
      return Integer(BBS.BBB.uint16_to_int16(word));
   end;
   --
   function get_accelerations(self : not null access LSM303DLHC_record'class;
                              error : out integer) return accelerations is
      accel : accelerations;
   begin
      self.port.read(self.addr_accel, accel_out_x_h + 16#80#, self.buff'access, 6, error);
      accel.x := Integer(uint16_to_int16(uint16(self.buff(0)) + uint16(self.buff(1))*256));
      accel.y := Integer(uint16_to_int16(uint16(self.buff(2)) + uint16(self.buff(3))*256));
      accel.z := Integer(uint16_to_int16(uint16(self.buff(4)) + uint16(self.buff(5))*256));
      return accel;
   end;
   --
   --
   function get_acceleration_x(self : not null access LSM303DLHC_record'class;
                               error : out integer) return BBS.units.accel_g is
      accel : integer := self.get_acceleration_x(error);
   begin
      return BBS.units.accel_g(float(accel) * self.accel_scale * self.accel_calib);
   end;
   --
   function get_acceleration_y(self : not null access LSM303DLHC_record'class;
                               error : out integer) return BBS.units.accel_g is
      accel : integer := self.get_acceleration_y(error);
   begin
      return BBS.units.accel_g(float(accel) * self.accel_scale * self.accel_calib);
   end;
   --
   function get_acceleration_z(self : not null access LSM303DLHC_record'class;
                               error : out integer) return BBS.units.accel_g is
      accel : integer := self.get_acceleration_z(error);
   begin
      return BBS.units.accel_g(float(accel) * self.accel_scale * self.accel_calib);
   end;
   --
   function get_accelerations(self : not null access LSM303DLHC_record'class;
                              error : out integer) return accelerations_g is
      accel : accelerations := self.get_accelerations(error);
      accel_gs : accelerations_g;
   begin
      accel_gs.x := BBS.units.accel_g(float(accel.x) * self.accel_scale * self.accel_calib);
      accel_gs.y := BBS.units.accel_g(float(accel.y) * self.accel_scale * self.accel_calib);
      accel_gs.z := BBS.units.accel_g(float(accel.z) * self.accel_scale * self.accel_calib);
      return accel_gs;
   end;
   --
   function get_accel_status(self : not null access LSM303DLHC_record'class;
                             error : out integer) return uint8 is
   begin
      return self.port.read(addr_accel, accel_status, error);
   end;
   --
   function accel_data_ready(self : not null access LSM303DLHC_record'class;
                             error : out integer) return boolean is
      err : integer;
      byte : uint8 := self.port.read(addr_accel, accel_status, err);
   begin
      error := err;
      if ((byte and accel_stat_zyxda) = accel_stat_zyxda) and (err = 0) then
         return true;
      else
         return false;
      end if;
   end;
   --
   function get_temperature(self : not null access LSM303DLHC_record'class;
                            error : out integer) return integer is
      word : uint16 := self.port.readm1(addr_mag, mag_temp_h, error);
   begin
      return integer(uint16_to_int16(word)/16);
   end;
   --
   function get_temperature(self : not null access LSM303DLHC_record'class;
                            error : out integer) return float is
      temp : integer := self.get_temperature(error);
   begin
      return float(self.temp_offset + temp)/8.0;
   end;
   --
   function get_temperature(self : not null access LSM303DLHC_record'class;
                            error : out integer) return BBS.units.temp_c is
      temp : integer := self.get_temperature(error);
   begin
      return BBS.units.temp_c(float(self.temp_offset + temp)/8.0);
   end;
   --
   function get_magnet_x(self : not null access LSM303DLHC_record'class;
                         error : out integer) return integer is
      word : uint16 := self.port.readm1(self.addr_mag, mag_out_x_h, error);
   begin
      return Integer(uint16_to_int16(word));
   end;
   --
   function get_magnet_y(self : not null access LSM303DLHC_record'class;
                         error : out integer) return integer is
      word : uint16 := self.port.readm1(self.addr_mag, mag_out_y_h, error);
   begin
      return Integer(uint16_to_int16(word));
   end;
   --
   function get_magnet_z(self : not null access LSM303DLHC_record'class;
                         error : out integer) return integer is
      word : uint16 := self.port.readm1(self.addr_mag, mag_out_z_h, error);
   begin
      return Integer(uint16_to_int16(word));
   end;
   --
   function get_magnetism(self : not null access LSM303DLHC_record'class;
                          error : out integer) return magnetism is
      mag : magnetism;
   begin
      self.port.read(self.addr_mag, mag_out_x_h, buff'access, 6, error);
      mag.x := Integer(uint16_to_int16(uint16(buff(1)) + uint16(buff(0))*256));
      mag.z := Integer(uint16_to_int16(uint16(buff(3)) + uint16(buff(2))*256));
      mag.y := Integer(uint16_to_int16(uint16(buff(5)) + uint16(buff(4))*256));
      return mag;
   end;
   --
   function get_magnet_x(self : not null access LSM303DLHC_record'class;
                         error : out integer) return BBS.units.mag_g is
      mag : integer := self.get_magnet_x(error);
   begin
      return BBS.units.mag_g(float(mag) * self.mag_scale_xy);
   end;
   --
   function get_magnet_y(self : not null access LSM303DLHC_record'class;
                         error : out integer) return BBS.units.mag_g is
      mag : integer := self.get_magnet_y(error);
   begin
      return BBS.units.mag_g(float(mag) * self.mag_scale_xy);
   end;
   --
      function get_magnet_z(self : not null access LSM303DLHC_record'class;
                            error : out integer) return BBS.units.mag_g is
      mag : integer := self.get_magnet_z(error);
   begin
      return BBS.units.mag_g(float(mag) * self.mag_scale_z);
   end;
   --
      function get_magnetism(self : not null access LSM303DLHC_record'class;
                             error : out integer) return magnetism_gauss is
      mag : magnetism := self.get_magnetism(error);
      mag_g : magnetism_gauss;
   begin
      mag_g.x := BBS.units.mag_g(float(mag.x) * self.mag_scale_xy);
      mag_g.y := BBS.units.mag_g(float(mag.y) * self.mag_scale_xy);
      mag_g.z := BBS.units.mag_g(float(mag.z) * self.mag_scale_z);
      return mag_g;
   end;
   --
   function get_mag_status(self : not null access LSM303DLHC_record'class;
                           error : out integer) return uint8 is
   begin
      return self.port.read(self.addr_mag, mag_sr, error);
   end;
   --
   function mag_data_ready(self : not null access LSM303DLHC_record'class;
                           error : out integer) return boolean is
      err : integer;
      byte : uint8 := self.port.read(self.addr_mag, mag_sr, err);
   begin
      error := err;
      if ((byte and mag_drdy) = mag_drdy) and (err = 0) then
         return true;
      else
         return false;
      end if;
   end;
   --
end;
