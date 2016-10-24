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
         Ada.Text_IO.Put("temp_1 = ");
         Ada.Integer_Text_IO.Put(integer(temp_1), width => 9, base => 16);
         Ada.Text_IO.New_Line;
         Ada.Text_IO.Put("temp_2 = ");
         Ada.Integer_Text_IO.Put(integer(temp_2), width => 9, base => 16);
         Ada.Text_IO.New_Line;
         Ada.Text_IO.Put("temp_3 = ");
         Ada.Integer_Text_IO.Put(integer(temp_3), width => 9, base => 16);
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
      --
      -- Nested functions to do conversion of the temperature, pressure, and
      -- humidity values.  These are based on the example C code in the datasheet
      -- and are not the clearest code.
      --
      -- Temperature conversion (t_fine needs a little more processing before
      -- generating the final temperature, but it is used in other processing)
      --
      function cal_temp(self : not null access BME280_record'class) return int32 is
         var1 : int32;
         var2 : int32;
      begin
         var1 := (int32(self.raw_temp)/2**3 - int32(self.T1)*2)*int32(self.T2)/2**11;
         var2 := (int32(self.raw_temp)/2**4 - int32(self.T1))*
           (int32(self.raw_temp)/2**4 - int32(self.T1))/2**12*int32(self.T3)/2**14;
         return var1 + var2;
      end;
      --
      -- Pressure conversion.  The result is in Pascals * 256.
      --
      function cal_press(self : not null access BME280_record'class) return uint32 is
         var1 : int64;
         var2 : int64;
         p : int64;
      begin
         var1 := int64(self.t_fine) - 128000;
         var2 := var1*var1*int64(self.P6);
         var2 := var2 + var1*int64(self.P5)*2**17;
         var2 := var2 + int64(self.P4)*2**35;
         var1 := var1*var1*int64(self.P3)/2**8 + var1*int64(self.P2)*2**12;
         var1 := (2**47 + var1)*int64(self.P1)/2**33;
         if (var1 = 0) then
            return 0;
         end if;
         p := 1_048_576 - int64(self.raw_press);
         p := (p*2**31 - var2)*3125/var1;
         var1 := int64(self.P9)*(p/2**13)*(p/2**13)/2**25;
         var2 := int64(self.P8)*p/2**19;
         p := (p + var1 + var2)/2**8 + int64(self.P7)*2**4;
         return uint32(p);
      end;
      --
      -- Humidity conversion.  The result is in % * 1024.
      --
      function cal_hum(self : not null access BME280_record'class) return uint32 is
         v_x1 : int32;
      begin
         v_x1 := self.t_fine - 76_800;
         v_x1 := ((int32(self.raw_hum)*2**14 - int32(self.H4)*2**20 - int32(self.H5)*v_x1 + 16_384)/2**15)*
           ((((v_x1*int32(self.H6)/2**10)*
            (v_x1*int32(self.H3)/2**11 + 32_768)/2**10 + 2_097_152)*int32(self.H2) + 8192)/2**14);
         v_x1 := v_x1 - (v_x1/2**15)*(v_x1/2**15)/2**7*int32(self.H1)/2**4;
         if (v_x1 < 0) then
            v_x1 := 0;
         elsif (v_x1 > 419_430_400) then
            v_x1 := 419_430_400;
         end if;
         return uint32(v_x1/2**12);
      end;
      --
   begin
      self.port.read(addr, data_start, buff'access, 8, error);
      self.raw_press := (uint32(buff(0))*2**16 + uint32(buff(1))*2**8 + uint32(buff(2)))/16;
      self.raw_temp  := (uint32(buff(3))*2**16 + uint32(buff(4))*2**8 + uint32(buff(5)))/16;
      self.raw_hum   := uint32(buff(6))*2**8  + uint32(buff(7));
      if (debug) then
         Ada.Text_IO.Put("p_raw: ");
         Ada.Integer_Text_IO.Put(integer(self.raw_press), width => 9, base => 16);
         Ada.Text_IO.New_Line;
         Ada.Text_IO.Put("t_raw: ");
         Ada.Integer_Text_IO.Put(integer(self.raw_temp), width => 9, base => 16);
         Ada.Text_IO.New_Line;
         Ada.Text_IO.Put("h_raw: ");
         Ada.Integer_Text_IO.Put(integer(self.raw_hum), width => 9, base => 16);
         Ada.Text_IO.New_Line;
      end if;
      --
      -- Compute the calibrated values based on the algorithms in the datasheet.
      --
      self.t_fine := cal_temp(self);
      self.p_cal := cal_press(self);
      self.h_cal := cal_hum(self);
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
      return integer(self.p_cal);
   end;
   --
   function get_press(self : not null access BME280_record'class) return BBS.units.press_p is
      int_press : integer := self.get_press;
   begin
      return BBS.units.press_p(int_press)/256.0;
   end;
   --
   function get_press(self : not null access BME280_record'class) return BBS.units.press_mb is
      int_press : integer := self.get_press;
   begin
      return BBS.units.to_milliBar(BBS.units.press_p(int_press)/256.0);
   end;
   --
   function get_press(self : not null access BME280_record'class) return BBS.units.press_atm is
      int_press : integer := self.get_press;
   begin
      return BBS.units.to_Atmosphere(BBS.units.press_p(int_press)/256.0);
   end;
   --
   function get_press(self : not null access BME280_record'class) return BBS.units.press_inHg is
      int_press : integer := self.get_press;
   begin
      return BBS.units.to_inHg(BBS.units.press_p(int_press)/256.0);
   end;
   --
   function get_hum(self : not null access BME280_record'class) return integer is
   begin
      return integer(self.h_cal);
   end get_hum;
   --
   function get_hum(self : not null access BME280_record'class) return float is
      int_value : integer := self.get_hum;
   begin
      return float(int_value)/1024.0;
   end get_hum;

end;
