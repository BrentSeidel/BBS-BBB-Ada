package body BBS.BBB.i2c.BME280 is
   --
   -- Get elementary math functions for floating point numbers
   --
   package float_functions is new Ada.Numerics.Generic_Elementary_Functions(float);
   function "**"(Left, Right : float) return float
                 renames float_functions."**";
   --
   --
   -- Object oriented interface
   --
   function i2c_new return BME280_ptr is
   begin
      return new BME280_record;
   end;
   --
   procedure configure(self : not null access BME280_record'class; port : i2c_interface;
                       addr : addr7; error : out integer) is
      temp_1 : uint8;
      temp_2 : uint8;
      temp_3 : uint8;
      temp_a : uint12;
      temp_b : uint12;
   begin
      self.port := port;
      self.address := addr;
      --
      -- Calibration parameters.  Most of these are either two byte with LSB
      -- first or a single byte.  The two exceptions are H4 and H5.
      --
      self.T1 := self.port.readm2(self.address, dig_T1, error);
      self.T2 := uint16_to_int16(self.port.readm2(self.address, dig_T2, error));
      self.T3 := uint16_to_int16(self.port.readm2(self.address, dig_T3, error));
      self.P1 := self.port.readm2(self.address, dig_P1, error);
      self.P2 := uint16_to_int16(self.port.readm2(self.address, dig_P2, error));
      self.P3 := uint16_to_int16(self.port.readm2(self.address, dig_P3, error));
      self.P4 := uint16_to_int16(self.port.readm2(self.address, dig_P4, error));
      self.P5 := uint16_to_int16(self.port.readm2(self.address, dig_P5, error));
      self.P6 := uint16_to_int16(self.port.readm2(self.address, dig_P6, error));
      self.P7 := uint16_to_int16(self.port.readm2(self.address, dig_P7, error));
      self.P8 := uint16_to_int16(self.port.readm2(self.address, dig_P8, error));
      self.P9 := uint16_to_int16(self.port.readm2(self.address, dig_P9, error));
      self.H1 := self.port.read(self.address, dig_H1, error);
      self.H2 := uint16_to_int16(self.port.readm2(self.address, dig_H2, error));
      self.H3 := self.port.read(self.address, dig_H3, error);
      --
      -- Specification of H4 is given as 0xE4/0xE5[3:0] => dig_H4[11:4]/[3:0]
      -- Specification of H5 is given as 0xE5[7:4]/0xE6 => dig_H5[3:0]/[11:4]
      -- These are actually 12 bit integers packed into three bytes.
      --
      temp_1 := self.port.read(self.address, dig_H4, error);
      temp_2 := self.port.read(self.address, dig_H45, error);
      temp_3 := self.port.read(self.address, dig_H5, error);
      temp_a := uint12(temp_1)*16 + uint12(temp_2 mod 16);
      temp_b := uint12(temp_3)*16 + uint12(temp_2/16);
      self.H4 := int16(uint12_to_int12(temp_a));
      self.H5 := int16(uint12_to_int12(temp_b));
      self.H6 := self.port.read(self.address, dig_H6, error);
      if debug then
         Ada.Text_IO.Put_Line("BME280 Calibration parameters");
         Ada.Text_IO.Put("T1 = ");
         Ada.Integer_Text_IO.Put(integer(self.T1), width => 9, base => 16);
         Ada.Text_IO.New_Line;
         Ada.Text_IO.Put("T2 = ");
         Ada.Integer_Text_IO.Put(integer(self.T2), width => 9, base => 16);
         Ada.Text_IO.New_Line;
         Ada.Text_IO.Put("T3 = ");
         Ada.Integer_Text_IO.Put(integer(self.T3), width => 9, base => 16);
         Ada.Text_IO.New_Line;
         Ada.Text_IO.Put("P1 = ");
         Ada.Integer_Text_IO.Put(integer(self.P1), width => 9, base => 16);
         Ada.Text_IO.New_Line;
         Ada.Text_IO.Put("P2 = ");
         Ada.Integer_Text_IO.Put(integer(self.P2), width => 9, base => 16);
         Ada.Text_IO.New_Line;
         Ada.Text_IO.Put("P3 = ");
         Ada.Integer_Text_IO.Put(integer(self.P3), width => 9, base => 16);
         Ada.Text_IO.New_Line;
         Ada.Text_IO.Put("P4 = ");
         Ada.Integer_Text_IO.Put(integer(self.P4), width => 9, base => 16);
         Ada.Text_IO.New_Line;
         Ada.Text_IO.Put("P5 = ");
         Ada.Integer_Text_IO.Put(integer(self.P5), width => 9, base => 16);
         Ada.Text_IO.New_Line;
         Ada.Text_IO.Put("P6 = ");
         Ada.Integer_Text_IO.Put(integer(self.P6), width => 9, base => 16);
         Ada.Text_IO.New_Line;
         Ada.Text_IO.Put("P7 = ");
         Ada.Integer_Text_IO.Put(integer(self.P7), width => 9, base => 16);
         Ada.Text_IO.New_Line;
         Ada.Text_IO.Put("P8 = ");
         Ada.Integer_Text_IO.Put(integer(self.P8), width => 9, base => 16);
         Ada.Text_IO.New_Line;
         Ada.Text_IO.Put("P9 = ");
         Ada.Integer_Text_IO.Put(integer(self.P9), width => 9, base => 16);
         Ada.Text_IO.New_Line;
         Ada.Text_IO.Put("H1 = ");
         Ada.Integer_Text_IO.Put(integer(self.H1), width => 9, base => 16);
         Ada.Text_IO.New_Line;
         Ada.Text_IO.Put("H2 = ");
         Ada.Integer_Text_IO.Put(integer(self.H2), width => 9, base => 16);
         Ada.Text_IO.New_Line;
         Ada.Text_IO.Put("H3 = ");
         Ada.Integer_Text_IO.Put(integer(self.H3), width => 9, base => 16);
         Ada.Text_IO.New_Line;
         Ada.Text_IO.Put("H4 = ");
         Ada.Integer_Text_IO.Put(integer(self.H4), width => 9, base => 16);
         Ada.Text_IO.New_Line;
         Ada.Text_IO.Put("H5 = ");
         Ada.Integer_Text_IO.Put(integer(self.H5), width => 9, base => 16);
         Ada.Text_IO.New_Line;
         Ada.Text_IO.Put("H6 = ");
         Ada.Integer_Text_IO.Put(integer(self.H6), width => 9, base => 16);
         Ada.Text_IO.New_Line;
      end if;
      --
      -- Now set the mode.  Use forced mode to keep the interface similar to
      -- the BMP180.
      --
      -- First put into sleep more so configuration can be set.  Oversampling
      -- is set to 1 for each parameter.
      --
      self.port.write(self.address, ctrl_meas, mode_sleep, error);
      --
      -- Set humidity oversampling
      --
      self.port.write(self.address, ctrl_hum, hum_over_1, error);
      --
      -- Temperature, pressure, and mode are in the same register so set them
      -- all at once.
      --
      self.port.write(self.address, ctrl_meas, temp_over_1 + press_over_1 +
                        mode_force, error);
   end;
   --
   procedure start_conversion(self : not null access BME280_record'class;
                              error : out integer) is
   begin
      self.port.write(self.address, ctrl_meas, temp_over_1 + press_over_1 +
                        mode_force, error);
   end;
   --
   function data_ready(self : not null access BME280_record'class;
                       error : out integer) return boolean is
      byte : uint8;
      err : integer;
   begin
      byte := self.port.read(self.address, status, err);
      error := err;
      if ((byte and stat_measuring) /= stat_measuring) and (err = 0) then
         return true;
      else
         return false;
      end if;
   end;
   --
   -- Read 3 bytes for pressure and temperature and 2 bytes for humidity. 8 bytes
   -- total
   --
   procedure read_data(self : not null access BME280_record'class; error : out integer) is
      var1 : int32;
      var2 : int32;
   begin
      self.port.read(addr, data_start, buff'access, 8, error);
      self.raw_press := (uint32(buff(0))*2**16 + uint32(buff(1))*2**8 + uint32(buff(2)))/16;
      self.raw_temp  := (uint32(buff(3))*2**16 + uint32(buff(4))*2**8 + uint32(buff(5)))/16;
      self.raw_hum   := uint32(buff(6))*2**8  + uint32(buff(7));
      --
      -- Compute the calibrated values based on the algorithms in the datasheet.
      --
      -- Temperature conversion
      --
      var1 := (((int32(self.raw_temp/2**3) - (int32(self.T1)*2)) * int32(T2))*2**11);
      var2 := ((((int32(self.raw_temp/2**4) - (int32(self.T1)) * (int32(self.raw_temp/2**4)
                    - int32(self.T1)))/2**12 * int32(self.T3))/2**14));
      self.t_fine := var1 + var2;
   end;
   --
   function get_temp(self : not null access BME280_record'class) return integer is
   begin
      return integer((self.t_fine*5 + 128)/2**8);
   end;
   --
   function get_temp(self : not null access BME280_record'class) return BBS.units.temp_c is
      int_temp : integer := self.get_temp;
   begin
      return BBS.units.temp_c(float(int_temp) / 100.0);
   end;
   --
   function get_temp(self : not null access BME280_record'class) return BBS.units.temp_f is
      int_temp : integer := self.get_temp;
   begin
      return BBS.units.to_Farenheit(BBS.units.temp_c(float(int_temp) / 100.0));
   end;
   --
   function get_temp(self : not null access BME280_record'class) return BBS.units.temp_k is
      int_temp : integer := self.get_temp;
   begin
      return BBS.units.to_Kelvin(BBS.units.temp_c(float(int_temp) / 100.0));
   end;
   --
   function get_press(self : not null access BME280_record'class) return integer is
   begin
      return integer(self.raw_press);
   end;
   --
   function get_press(self : not null access BME280_record'class) return BBS.units.press_p is
      int_press : integer := self.get_press;
   begin
      return BBS.units.press_p(int_press);
   end;
   --
   function get_press(self : not null access BME280_record'class) return BBS.units.press_mb is
      int_press : integer := self.get_press;
   begin
      return BBS.units.to_milliBar(BBS.units.press_p(int_press));
   end;
   --
   function get_press(self : not null access BME280_record'class) return BBS.units.press_atm is
      int_press : integer := self.get_press;
   begin
      return BBS.units.to_Atmosphere(BBS.units.press_p(int_press));
   end;
   --
   function get_press(self : not null access BME280_record'class) return BBS.units.press_inHg is
      int_press : integer := self.get_press;
   begin
      return BBS.units.to_inHg(BBS.units.press_p(int_press));
   end;
   --
   function get_hum(self : not null access BME280_record'class) return integer is
   begin
      return integer(self.raw_hum);
   end;

end;
