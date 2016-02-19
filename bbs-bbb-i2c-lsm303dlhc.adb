package body BBS.BBB.i2c.LMS303DLHC is
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
            Ada.Text_IO.Put_Line("Unknown value for LMS303DLHC accelerometer full scale deflection");
            raise Program_Error;
      end case;
      case mag_fs is
         when fs_1_3_gauss =>
            mag_scale := 1.3 / 2048.0;
         when fs_1_9_gauss =>
            mag_scale := 1.9 / 2048.0;
         when fs_2_5_gauss =>
            mag_scale := 2.5 / 2048.0;
         when fs_4_0_gauss =>
            mag_scale := 4.0 / 2048.0;
         when fs_4_7_gauss =>
            mag_scale := 4.7 / 2048.0;
         when fs_5_6_gauss =>
            mag_scale := 5.6 / 2048.0;
         when fs_8_1_gauss =>
            mag_scale := 8.1 / 2048.0;
         when others =>
            Ada.Text_IO.Put_Line("Unknown value for LMS303DLHC magnetometer full scale deflection");
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
   --
   -- The temperature is a 12 bit value returned in two 8 bit registers.  The
   -- resolution is 8 LSB to one degree C.  Unfortunately, in testing, this
   -- does not seem to produce a reasonable value.  The value produced is about
   -- 20C too low.
   --
   function get_temperature(error : out integer) return integer is
      byte : uint8;
      word : uint16;
   begin
      byte := BBS.BBB.i2c.read(addr_mag, mag_temp_h, error);
      word := uint16(byte) * 256;
      byte := BBS.BBB.i2c.read(addr_mag, mag_temp_l, error);
      word := word + uint16(byte);
      return integer(BBS.BBB.uint16_to_int16(word)/16);
   end;
   --
   function get_temperature(error : out integer) return float is
      temp : integer;
   begin
      temp := get_temperature(error);
      return float(temperature_offset + temp)/8.0;
   end;
   --
   function get_temperature(error : out integer) return Celsius is
      temp : integer;
   begin
      temp := get_temperature(error);
      return Celsius(float(temperature_offset + temp)/8.0);
   end;
   --
   function get_acceleration_x(error : out integer) return integer is
      word : uint16;
   begin
      word := BBS.BBB.i2c.read(addr_accel, accel_out_x_h + 16#80#, error);
      return Integer(BBS.BBB.uint16_to_int16(word));
   end;
   --
   function get_acceleration_y(error : out integer) return integer is
      word : uint16;
   begin
      word := BBS.BBB.i2c.read(addr_accel, accel_out_y_h + 16#80#, error);
      return Integer(BBS.BBB.uint16_to_int16(word));
   end;
   --
   function get_acceleration_z(error : out integer) return integer is
      word : uint16;
   begin
      word := BBS.BBB.i2c.read(addr_accel, accel_out_z_h + 16#80#, error);
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
   function get_acceleration_x(error : out integer) return accel_g is
      accel : integer;
   begin
      accel := get_acceleration_x(error);
      return accel_g(float(accel) * accel_scale);
   end;
   --
   function get_acceleration_y(error : out integer) return accel_g is
      accel : integer;
   begin
      accel := get_acceleration_y(error);
      return accel_g(float(accel) * accel_scale);
   end;
   --
   function get_acceleration_z(error : out integer) return accel_g is
      accel : integer;
   begin
      accel := get_acceleration_z(error);
      return accel_g(float(accel) * accel_scale);
   end;
   --
   function get_accelerations(error : out integer) return accelerations_g is
      accel : accelerations;
      accel_gs : accelerations_g;
   begin
      accel := get_accelerations(error);
      accel_gs.x := accel_g(float(accel.x) * accel_scale);
      accel_gs.y := accel_g(float(accel.y) * accel_scale);
      accel_gs.z := accel_g(float(accel.z) * accel_scale);
      return accel_gs;
   end;
   --
   function get_accel_status(error : out integer) return uint8 is
   begin
      return BBS.BBB.i2c.read(addr_accel, accel_status, error);
   end;
   --
   function accel_data_ready(error : out integer) return boolean is
      byte : uint8;
      err : integer;
   begin
      byte := BBS.BBB.i2c.read(addr_accel, accel_status, err);
      error := err;
      if ((byte and accel_stat_zyxda) = accel_stat_zyxda) and (err = 0) then
         return true;
      else
         return false;
      end if;
   end;
   --
   function get_magnet_x(error : out integer) return integer is
      word : uint16;
   begin
      word := BBS.BBB.i2c.read(addr_mag, mag_out_x_h + 16#80#, error);
      return Integer(BBS.BBB.uint16_to_int16(word));
   end;
   --
   function get_magnet_y(error : out integer) return integer is
      word : uint16;
   begin
      word := BBS.BBB.i2c.read(addr_mag, mag_out_y_h + 16#80#, error);
      return Integer(BBS.BBB.uint16_to_int16(word));
   end;
   --
   function get_magnet_z(error : out integer) return integer is
      word : uint16;
   begin
      word := BBS.BBB.i2c.read(addr_mag, mag_out_z_h + 16#80#, error);
      return Integer(BBS.BBB.uint16_to_int16(word));
   end;
   --
   function get_magnetism(error : out integer) return magnetism is
      mag : magnetism;
   begin
      read(addr_mag, mag_out_x_h + 16#80#, buff'access, 6, error);
      mag.x := Integer(uint16_to_int16(uint16(buff(0)) + uint16(buff(1))*256));
      mag.z := Integer(uint16_to_int16(uint16(buff(2)) + uint16(buff(3))*256));
      mag.y := Integer(uint16_to_int16(uint16(buff(4)) + uint16(buff(5))*256));
      return mag;
   end;
   --
   function get_magnet_x(error : out integer) return gauss is
      mag : integer;
   begin
      mag := get_magnet_x(error);
      return gauss(float(mag) * mag_scale);
   end;
   --
   function get_magnet_y(error : out integer) return gauss is
      mag : integer;
   begin
      mag := get_magnet_y(error);
      return gauss(float(mag) * mag_scale);
   end;
   --
   function get_magnet_z(error : out integer) return gauss is
      mag : integer;
   begin
      mag := get_magnet_z(error);
      return gauss(float(mag) * mag_scale);
   end;
   --
   function get_magnetism(error : out integer) return magnetism_gauss is
      mag : magnetism;
      mag_g : magnetism_gauss;
   begin
      mag := get_magnetism(error);
      mag_g.x :=gauss(float(mag.x) * mag_scale);
      mag_g.z :=gauss(float(mag.y) * mag_scale);
      mag_g.y :=gauss(float(mag.z) * mag_scale);
      return mag_g;
   end;
   --
   function get_mag_status(error : out integer) return uint8 is
   begin
      return BBS.BBB.i2c.read(addr_mag, mag_sr, error);
   end;
   --
   function mag_data_ready(error : out integer) return boolean is
      byte : uint8;
      err : integer;
   begin
      byte := BBS.BBB.i2c.read(addr_mag, mag_sr, err);
      error := err;
      if ((byte and mag_drdy) = mag_drdy) and (err = 0) then
         return true;
      else
         return false;
      end if;
   end;

end;
