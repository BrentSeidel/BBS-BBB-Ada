package body BBS.BBB.i2c.L3GD20H is
   --
   -- Do a basic configuration.  Turn on the three axis gyroscopes and let most
   -- everything else in the default state.  Any application specific setup can
   -- be added here.
   --
   procedure configure(error : out integer) is
   begin
      BBS.BBB.i2c.write(addr, ctrl1, 16#0f#, error);
   end;
   --
   -- A set of utility functions to get measurements from the sensors.  Note
   -- that for this device, adding 16#80# to the register address causes the
   -- address to automatically increment when reading multiple bytes.
   --
   function get_temperature(error : out integer) return integer is
      byte : uint8;
   begin
      byte := BBS.BBB.i2c.read(addr, out_temp, error);
      return Integer(BBS.BBB.uint8_to_int8(byte));
   end;
   --
   function get_rotation_x(error : out integer) return integer is
      word : uint16;
   begin
      word := BBS.BBB.i2c.read(addr, out_x_l + 16#80#, error);
      return Integer(BBS.BBB.uint16_to_int16(word));
   end;
   --
   function get_rotation_y(error : out integer) return integer is
      word : uint16;
   begin
      word := BBS.BBB.i2c.read(addr, out_y_l + 16#80#, error);
      return Integer(BBS.BBB.uint16_to_int16(word));
   end;
   --
   function get_rotation_z(error : out integer) return integer is
      word : uint16;
   begin
      word := BBS.BBB.i2c.read(addr, out_z_l + 16#80#, error);
      return Integer(BBS.BBB.uint16_to_int16(word));
   end;
   --
   -- Currently this function is unimplemented.  Eventually it will read all
   -- three rotation values in a single i2c transaction and return a structure
   -- containing the values.
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
      byte : uint8;
      err : integer;
   begin
      byte := BBS.BBB.i2c.read(addr, status, err);
      error := err;
      if ((byte and zyxda) = zyxda) and (err = 0) then
         return true;
      else
         return false;
      end if;
   end;

end;
