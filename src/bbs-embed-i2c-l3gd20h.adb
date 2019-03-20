with BBS.embed.log;
--
-- Author: Brent Seidel
-- Date:   15-Mar-2016
--
--  See spec header for more details.
--
package body BBS.embed.i2c.L3GD20H is
   --
   procedure configure(self : in out L3GD20H_record; port : i2c_interface;
                       addr : addr7; error : out err_code) is
   begin
      self.hw := port;
      self.address := addr;
      self.scale := 245.0/32767.0;
      self.hw.write(addr, ctrl1, uint8(16#ff#), error);
      --
      -- Data rate = 50Hz (or 800Hz if low_odr is 0)
      -- Bandwidth is 16.6Hz or 100Hz
      -- Normal mode
      -- X, Y, Z, axis sensors enabled
      --
   end;
   --
   procedure configure(self : in out L3GD20H_record; port : i2c_interface;
                       addr : addr7; deflection : uint8; error : out err_code) is

   begin
      case deflection is
         when fs_245dps =>
            self.scale := 245.0/32767.0;
         when fs_500dps =>
            self.scale := 500.0/32767.0;
         when fs_2000dps =>
            self.scale := 2000.0/32767.0;
         when others =>
            BBS.embed.log.error.Put_Line("Unknown value for L3GD20H full scale deflection");
            raise Program_Error;
      end case;
      self.hw := port;
      self.address := addr;
      self.hw.write(addr, ctrl4, deflection, error);
      self.hw.write(addr, ctrl1, uint8(16#ff#), error);
      --
      -- Data rate = 50Hz (or 800Hz if low_odr is 0)
      -- Bandwidth is 16.6Hz or 100Hz
      -- Normal mode
      -- X, Y, Z, axis sensors enabled
      --
   end;
   --
   function get_temperature(self : L3GD20H_record;
                            error : out err_code) return integer is
      byte : uint8 := self.hw.read(self.address, out_temp, error);
   begin
      return Integer(uint8_to_int8(byte));
   end;
   --
   function get_rotation_x(self : L3GD20H_record;
                           error : out err_code) return integer is
      word : uint16 := self.hw.readm2(self.address, out_x_l + 16#80#, error);
   begin
      return Integer(uint16_to_int16(word));
   end;
   --
   function get_rotation_y(self : L3GD20H_record;
                           error : out err_code) return integer is
      word : uint16 := self.hw.readm2(self.address, out_y_l + 16#80#, error);
   begin
      return Integer(uint16_to_int16(word));
   end;
   --
   function get_rotation_z(self : L3GD20H_record;
                           error : out err_code) return integer is
      word : uint16 := self.hw.readm2(self.address, out_z_l + 16#80#, error);
   begin
      return Integer(uint16_to_int16(word));
   end;
   --
   function get_rotations(self : L3GD20H_record;
                          error : out err_code) return rotations is
      rot : rotations;
   begin
      self.hw.read(self.address, out_x_l + 16#80#, buff_index(6), error);
      rot.x :=Integer(uint16_to_int16(uint16(self.hw.b(0)) + uint16(self.hw.b(1))*256));
      rot.y :=Integer(uint16_to_int16(uint16(self.hw.b(2)) + uint16(self.hw.b(3))*256));
      rot.z :=Integer(uint16_to_int16(uint16(self.hw.b(4)) + uint16(self.hw.b(5))*256));
      return rot;
   end;
   --
   --
   function get_temperature(self : L3GD20H_record;
                            error : out err_code) return BBS.units.temp_c is
      raw : integer := self.get_temperature(error);
   begin
      return BBS.units.temp_c(self.temp_offset - raw);
   end;
   --
   function get_rotation_x(self : L3GD20H_record;
                           error : out err_code) return BBS.units.rot_d_s is
      raw : integer := self.get_rotation_x(error);
   begin
      return BBS.units.rot_d_s(float(raw) * self.scale);
   end;
   --
   function get_rotation_y(self : L3GD20H_record;
                           error : out err_code) return BBS.units.rot_d_s is
      raw : integer := self.get_rotation_y(error);
   begin
      return BBS.units.rot_d_s(float(raw) * self.scale);
   end;
   --
   function get_rotation_z(self : L3GD20H_record;
                           error : out err_code) return BBS.units.rot_d_s is
      raw : integer := self.get_rotation_z(error);
   begin
      return BBS.units.rot_d_s(float(raw) * self.scale);
   end;
   --
   function get_rotations(self : L3GD20H_record;
                          error : out err_code) return rotations_dps is
      raw : rotations := self.get_rotations(error);
      rot : rotations_dps;
   begin
      rot.x := BBS.units.rot_d_s(float(raw.x - self.offset_x) * self.scale);
      rot.y := BBS.units.rot_d_s(float(raw.y - self.offset_y) * self.scale);
      rot.z := BBS.units.rot_d_s(float(raw.z - self.offset_z) * self.scale);
      return rot;
   end;
   --
   function get_status(self : L3GD20H_record;
                       error : out err_code) return uint8 is
   begin
      return self.hw.read(self.address, status, error);
   end;
   --
   function data_ready(self : L3GD20H_record;
                       error : out err_code) return boolean is
      byte : uint8;
      err : err_code;
   begin
      byte := self.hw.read(self.address, status, err);
      error := err;
      if ((byte and zyxda) = zyxda) and (err = none) then
         return true;
      else
         return false;
      end if;
   end;
   --
   -- From emperical measurements, if the variance is less than 100_000 for each
   -- of the three axis, then the offsets should be good.  If not, try again to
   -- get good results.
   --
   -- In order to prevent infinite loops, this only checks a maximum of five
   -- times.
   --
   function measure_offsets(self : in out L3GD20H_record) return boolean is
      threshold : constant float := 400_000.0;
      sum_x : float := 0.0;
      sum_y : float := 0.0;
      sum_z : float := 0.0;
      sum_x2 : float := 0.0;
      sum_y2 : float := 0.0;
      sum_z2 : float := 0.0;
      var_x : float;
      var_y : float;
      var_z : float;
      rot : rotations;
      samples : constant integer := 50;
      err : err_code;
      loop_counter : integer := 0;
   begin
      loop
         for i in 1 .. samples loop
            loop
               exit when self.data_ready(err);
            end loop;
            rot := self.get_rotations(err);
            sum_x := sum_x + float(rot.x);
            sum_y := sum_y + float(rot.y);
            sum_z := sum_z + float(rot.z);
            sum_x2 := sum_x2 + float(rot.x)*float(rot.x);
            sum_y2 := sum_y2 + float(rot.y)*float(rot.y);
            sum_z2 := sum_z2 + float(rot.z)*float(rot.z);
         end loop;
         self.offset_x := integer(sum_x / float(samples));
         self.offset_y := integer(sum_y / float(samples));
         self.offset_z := integer(sum_z / float(samples));
         var_x := (sum_x2 - float(self.offset_x)*float(self.offset_x))/(float(samples - 1));
         var_y := (sum_y2 - float(self.offset_y)*float(self.offset_y))/(float(samples - 1));
         var_z := (sum_z2 - float(self.offset_z)*float(self.offset_z))/(float(samples - 1));
         if debug then
            BBS.embed.log.debug.Put_Line("Rotation offsets are: <" & integer'Image(self.offset_x) & ", " &
                                   integer'Image(self.offset_y) & ", " &
                                   integer'Image(self.offset_z) & ">");
            BBS.embed.log.debug.Put_Line("Rotation variances are: <" & integer'Image(integer(var_x)) & ", " &
                                   integer'Image(integer(var_y)) & ", " &
                                   integer'Image(integer(var_z)) & ">");
         end if;
         exit when loop_counter > 5;
         exit when (var_x < threshold) and (var_y < threshold) and (var_z < threshold);
         loop_counter := loop_counter + 1;
      end loop;
      if (var_x < threshold) and (var_y < threshold) and (var_z < threshold) then
         return true;
      else
         return false;
      end if;
   end;
   --
end;
