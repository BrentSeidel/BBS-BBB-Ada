package body BBS.BBB.i2c.LMS303DLHC is
   --
   -- Do a basic configuration.  Turn on the accelerometers, magnetometers, and
   -- temperature sensor and let most everything else in the default state.
   -- Any application specific setup can be added here.
   --
   procedure configure(error : out integer) is
   begin
      BBS.BBB.i2c.write(addr_accel, accel_ctrl1, 16#57#, error);
      BBS.BBB.i2c.write(addr_mag, mag_cra, 16#90#, error);
      BBS.BBB.i2c.write(addr_mag, mag_mr, 16#00#, error);
   end;
   --
   -- A set of utility functions to get measurements from the sensors.  Note
   -- that for this device, adding 16#80# to the register address causes the
   -- address to automatically increment when reading multiple bytes.
   --
   function get_temperature(error : out integer) return integer is
      byte : uint8;
      word : uint16;
   begin
      byte := BBS.BBB.i2c.read(addr_mag, mag_temp_h, error);
      word := BBS.BBB.uint16(byte * 256);
      byte := BBS.BBB.i2c.read(addr_mag, mag_temp_l, error);
      return integer(BBS.BBB.uint16_to_int16(word + BBS.BBB.uint16(byte)))/16;
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
   -- Currently this function is unimplemented.  Eventually it will read all
   -- three rotation values in a single i2c transaction and return a structure
   -- containing the values.
   --
   function get_accelerations(error : out integer) return accelerations is
      accel : accelerations;
   begin
      read(addr_accel, accel_out_x_h + 16#80#, buff'access, 6, error);
      accel.x :=Integer(uint16_to_int16(uint16(buff(0)) + uint16(buff(1))*256));
      accel.y :=Integer(uint16_to_int16(uint16(buff(2)) + uint16(buff(3))*256));
      accel.z :=Integer(uint16_to_int16(uint16(buff(4)) + uint16(buff(5))*256));
      return accel;
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
   -- Currently this function is unimplemented.  Eventually it will read all
   -- three rotation values in a single i2c transaction and return a structure
   -- containing the values.
   --
   function get_magnetism(error : out integer) return magnetism is
      mag : magnetism;
   begin
      read(addr_mag, mag_out_x_h + 16#80#, buff'access, 6, error);
      mag.x :=Integer(uint16_to_int16(uint16(buff(0)) + uint16(buff(1))*256));
      mag.z :=Integer(uint16_to_int16(uint16(buff(2)) + uint16(buff(3))*256));
      mag.y :=Integer(uint16_to_int16(uint16(buff(4)) + uint16(buff(5))*256));
      return mag;
   end;
end;
