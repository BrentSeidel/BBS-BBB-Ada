package body BBS.BBB.i2c.L3GD20H is
   --
   -- Do a basic configuration.  Turn on the three axis gyroscopes and let most
   -- everything else in the default state.  Any application specific setup can
   -- be added here.
   --
   procedure configure(error : out integer) is
   begin
      dps_scale := 245.0/32767.0;
      BBS.BBB.i2c.write(addr, ctrl1, 16#ff#, error);
      --
      -- Data rate = 50Hz (or 800Hz if low_odr is 0)
      -- Bandwidth is 16.6Hz or 100Hz
      -- Normal mode
      -- X, Y, Z, axis sensors enabled
      --
   end;
   --
   procedure configure(deflection : uint8; error : out integer) is
   begin
      case deflection is
         when fs_245dps =>
            dps_scale := 245.0/32767.0;
         when fs_500dps =>
            dps_scale := 500.0/32767.0;
         when fs_2000dps =>
            dps_scale := 2000.0/32767.0;
         when others =>
            Ada.Text_IO.Put_Line("Unknown value for L3GD20H full scale deflection");
            raise Program_Error;
      end case;
      BBS.BBB.i2c.write(addr, ctrl4, deflection, error);
      BBS.BBB.i2c.write(addr, ctrl1, 16#ff#, error);
      --
      -- Data rate = 50Hz (or 800Hz if low_odr is 0)
      -- Bandwidth is 16.6Hz or 100Hz
      -- Normal mode
      -- X, Y, Z, axis sensors enabled
      --
   end;

   --
   -- A set of utility functions to get measurements from the sensors.  Note
   -- that for this device, adding 16#80# to the register address causes the
   -- address to automatically increment when reading multiple bytes.
   --
   -- The temperature sensor values actually have a LSB resolution of -1 deg C.
   -- It also appears to be offset by about 40 deg C.  This is determined
   -- emperically as the datasheet isn't really clear.  This function returns the
   -- raw value from the sensor.
   --
   function get_temperature(error : out integer) return integer is
      byte : uint8 := BBS.BBB.i2c.read(addr, out_temp, error);
   begin
      return Integer(BBS.BBB.uint8_to_int8(byte));
   end;
   --
   function get_rotation_x(error : out integer) return integer is
      word : uint16 := BBS.BBB.i2c.read(addr, out_x_l + 16#80#, error);
   begin
      return Integer(BBS.BBB.uint16_to_int16(word));
   end;
   --
   function get_rotation_y(error : out integer) return integer is
      word : uint16 := BBS.BBB.i2c.read(addr, out_y_l + 16#80#, error);
   begin
      return Integer(BBS.BBB.uint16_to_int16(word));
   end;
   --
   function get_rotation_z(error : out integer) return integer is
      word : uint16 := BBS.BBB.i2c.read(addr, out_z_l + 16#80#, error);
   begin
      return Integer(BBS.BBB.uint16_to_int16(word));
   end;
   --
   -- This function reads all three rotation values in a single i2c transaction
   -- and returns a structure containing the values.
   --
   function get_rotations(error : out integer) return rotations is
      rot : rotations;
   begin
      read(addr, out_x_l + 16#80#, buff'access, 6, error);
      rot.x :=Integer(uint16_to_int16(uint16(buff(0)) + uint16(buff(1))*256));
      rot.y :=Integer(uint16_to_int16(uint16(buff(2)) + uint16(buff(3))*256));
      rot.z :=Integer(uint16_to_int16(uint16(buff(4)) + uint16(buff(5))*256));
      return rot;
   end;
   --
   function get_status(error : out integer) return uint8 is
   begin
      return BBS.BBB.i2c.read(addr, status, error);
   end;
   --
   function data_ready(error : out integer) return boolean is
      err : integer;
      byte : uint8 := BBS.BBB.i2c.read(addr, status, err);
   begin
      error := err;
      if ((byte and zyxda) = zyxda) and (err = 0) then
         return true;
      else
         return false;
      end if;
   end;
   --
   function get_temperature(error : out integer) return Celsius is
      raw : integer := get_temperature(error);
   begin
      return Celsius(temperature_offset - raw);
   end;
   --
   function get_rotation_x(error : out integer) return rate_dps is
      raw : integer := get_rotation_x(error);
   begin
      return rate_dps(float(raw) * dps_scale);
   end;
   --
   function get_rotation_y(error : out integer) return rate_dps is
      raw : integer := get_rotation_y(error);
   begin
      return rate_dps(float(raw) * dps_scale);
   end;
   --
   function get_rotation_z(error : out integer) return rate_dps is
      raw : integer := get_rotation_z(error);
   begin
      return rate_dps(float(raw) * dps_scale);
   end;
   --
   function get_rotations(error : out integer) return rotations_dps is
      raw : rotations := get_rotations(error);
      rot : rotations_dps;
   begin
      rot.x := rate_dps(float(raw.x) * dps_scale);
      rot.y := rate_dps(float(raw.y) * dps_scale);
      rot.z := rate_dps(float(raw.z) * dps_scale);
      return rot;
   end;
   --
   -- Object oriented interface.  This basically emulates the standard interface
   -- above.
   --
   function i2c_new return L3GD20H_ptr is
   begin
      return new L3GD20H_record;
   end;
   --
   procedure configure(self : not null access L3GD20H_record'class; port : i2c_interface;
                       addr : addr7; error : out integer) is
   begin
      self.port := port;
      self.address := addr;
      self.scale := 245.0/32767.0;
      self.port.write(addr, ctrl1, 16#ff#, error);
      --
      -- Data rate = 50Hz (or 800Hz if low_odr is 0)
      -- Bandwidth is 16.6Hz or 100Hz
      -- Normal mode
      -- X, Y, Z, axis sensors enabled
      --
   end;
   --
   procedure configure(self : not null access L3GD20H_record'class; port : i2c_interface;
                       addr : addr7; deflection : uint8; error : out integer) is

   begin
      case deflection is
         when fs_245dps =>
            self.scale := 245.0/32767.0;
         when fs_500dps =>
            self.scale := 500.0/32767.0;
         when fs_2000dps =>
            self.scale := 2000.0/32767.0;
         when others =>
            Ada.Text_IO.Put_Line("Unknown value for L3GD20H full scale deflection");
            raise Program_Error;
      end case;
      self.port := port;
      self.address := addr;
      self.port.write(addr, ctrl4, deflection, error);
      self.port.write(addr, ctrl1, 16#ff#, error);
      --
      -- Data rate = 50Hz (or 800Hz if low_odr is 0)
      -- Bandwidth is 16.6Hz or 100Hz
      -- Normal mode
      -- X, Y, Z, axis sensors enabled
      --
   end;
   --
   function get_temperature(self : not null access L3GD20H_record'class;
                            error : out integer) return integer is
      byte : uint8 := self.port.read(self.address, out_temp, error);
   begin
      return Integer(BBS.BBB.uint8_to_int8(byte));
   end;
   --
   function get_rotation_x(self : not null access L3GD20H_record'class;
                           error : out integer) return integer is
      word : uint16 := self.port.read(self.address, out_x_l + 16#80#, error);
   begin
      return Integer(BBS.BBB.uint16_to_int16(word));
   end;
   --
   function get_rotation_y(self : not null access L3GD20H_record'class;
                           error : out integer) return integer is
      word : uint16 := self.port.read(self.address, out_y_l + 16#80#, error);
   begin
      return Integer(BBS.BBB.uint16_to_int16(word));
   end;
   --
   function get_rotation_z(self : not null access L3GD20H_record'class;
                           error : out integer) return integer is
      word : uint16 := self.port.read(self.address, out_z_l + 16#80#, error);
   begin
      return Integer(BBS.BBB.uint16_to_int16(word));
   end;
   --
   function get_rotations(self : not null access L3GD20H_record'class;
                          error : out integer) return rotations is
      rot : rotations;
   begin
      self.port.read(self.address, out_x_l + 16#80#, buff'access, 6, error);
      rot.x :=Integer(uint16_to_int16(uint16(buff(0)) + uint16(buff(1))*256));
      rot.y :=Integer(uint16_to_int16(uint16(buff(2)) + uint16(buff(3))*256));
      rot.z :=Integer(uint16_to_int16(uint16(buff(4)) + uint16(buff(5))*256));
      return rot;
   end;
   --
   --
   function get_temperature(self : not null access L3GD20H_record'class;
                            error : out integer) return Celsius is
      raw : integer := self.get_temperature(error);
   begin
      return Celsius(self.temp_offset - raw);
   end;
   --
   function get_rotation_x(self : not null access L3GD20H_record'class;
                           error : out integer) return rate_dps is
      raw : integer := self.get_rotation_x(error);
   begin
      return rate_dps(float(raw) * self.scale);
   end;
   --
   function get_rotation_y(self : not null access L3GD20H_record'class;
                           error : out integer) return rate_dps is
      raw : integer := self.get_rotation_y(error);
   begin
      return rate_dps(float(raw) * self.scale);
   end;
   --
   function get_rotation_z(self : not null access L3GD20H_record'class;
                           error : out integer) return rate_dps is
      raw : integer := self.get_rotation_z(error);
   begin
      return rate_dps(float(raw) * self.scale);
   end;
   --
   function get_rotations(self : not null access L3GD20H_record'class;
                          error : out integer) return rotations_dps is
      raw : rotations := self.get_rotations(error);
      rot : rotations_dps;
   begin
      rot.x := rate_dps(float(raw.x - self.offset_x) * self.scale);
      rot.y := rate_dps(float(raw.y - self.offset_y) * self.scale);
      rot.z := rate_dps(float(raw.z - self.offset_z) * self.scale);
      return rot;
   end;
   --
   function get_status(self : not null access L3GD20H_record'class;
                       error : out integer) return uint8 is
   begin
      return self.port.read(self.address, status, error);
   end;
   --
   function data_ready(self : not null access L3GD20H_record'class;
                       error : out integer) return boolean is
      byte : uint8;
      err : integer;
   begin
      byte := self.port.read(self.address, status, err);
      error := err;
      if ((byte and zyxda) = zyxda) and (err = 0) then
         return true;
      else
         return false;
      end if;
   end;
   --
   procedure measure_offsets(self : not null access L3GD20H_record'class) is
      sum_x : integer := 0;
      sum_y : integer := 0;
      sum_z : integer := 0;
      rot : rotations;
      samples : constant integer := 50;
      err : integer;
   begin
      for i in 1 .. samples loop
         loop
            exit when self.data_ready(err);
         end loop;
         rot := self.get_rotations(err);
         sum_x := sum_x + rot.x;
         sum_y := sum_y + rot.y;
         sum_z := sum_z + rot.z;
      end loop;
      self.offset_x := sum_x / samples;
      self.offset_y := sum_y / samples;
      self.offset_z := sum_z / samples;
      Ada.Text_IO.Put_Line("Rotation offsets are: <" & integer'Image(self.offset_x) & ", " &
                    integer'Image(self.offset_y) & ", " &
                    integer'Image(self.offset_z) & ">");
   end;
   --
end;
