package body BBS.BBB.i2c.BME280 is
   --
   -- Get elementary math functions for floating point numbers
   --
   package float_functions is new Ada.Numerics.Generic_Elementary_Functions(float);
   function "**"(Left, Right : float) return float
                 renames float_functions."**";
   --
   -- Given local pressure and altimeter setting, determine the pressure
   -- altitude.  Given local pressure and altitude, determine the altimeter
   -- setting.
   --
   function pressure_altitude(pressure : BBS.units.press_p; altm : BBS.units.press_p)
                              return BBS.units.len_m is
   begin
      return BBS.units.len_m(44330.0 * (1.0 - (float(pressure)/float(altm))**float(1.0/5.255)));
   end;
   --
   function altimeter(pressure : BBS.units.press_p; altitude : BBS.units.len_m) return
     BBS.units.press_p is
   begin
      return BBS.units.press_p(float(pressure)/(1.0 - (float(altitude)/44330.0)**float(5.255)));
   end;
   --
   -- Procedures to work with the BME280 pressure and temperature sensor
   --
   --
   -- This procedure reads the calibration data from the sensor and uses it to
   -- generate a number of calibration constants.
   --
   procedure configure(error : out integer) is
   begin
      T1 := readm2(addr, dig_T1, error);
      T2 := uint16_to_int16(readm2(addr, dig_T2, error));
      T3 := uint16_to_int16(readm2(addr, dig_T3, error));
      P1 := readm2(addr, dig_P1, error);
      P2 := uint16_to_int16(readm2(addr, dig_P2, error));
      P3 := uint16_to_int16(readm2(addr, dig_P3, error));
      P4 := uint16_to_int16(readm2(addr, dig_P4, error));
      P5 := uint16_to_int16(readm2(addr, dig_P5, error));
      P6 := uint16_to_int16(readm2(addr, dig_P6, error));
      P7 := uint16_to_int16(readm2(addr, dig_P7, error));
      P8 := uint16_to_int16(readm2(addr, dig_P8, error));
      P9 := uint16_to_int16(readm2(addr, dig_P9, error));
      H1 := read(addr, dig_H1, error);
      H2 := uint16_to_int16(readm2(addr, dig_H2, error));
      H3 := read(addr, dig_H3, error);
      H4 := uint16_to_int16(readm2(addr, dig_H4, error));
      H5 := uint16_to_int16(readm2(addr, dig_H5, error));
      H6 := read(addr, dig_H6, error);
   end;
   --
--   procedure start_conversion(kind : uint8; error : out integer) is
--   begin
--      BBS.BBB.i2c.write(addr, ctrl, kind, error);
--      last_cvt := kind;
--   end;
   --
--   function data_ready(error : out integer) return boolean is
--      byte : uint8;
--      err : integer;
--   begin
--      byte := BBS.BBB.i2c.read(addr, ctrl, err);
--      error := err;
--      if ((byte and start_cvt) /= start_cvt) and (err = 0) then
--         return true;
--      else
--         return false;
--      end if;
--   end;
   --
   -- The calculations to get calibrated temperature and pressure are based on
   -- the algorithm in the datasheet.  It does not explain the meaning of the
   -- various constants or steps in the process.  It is also not unlikely that
   -- some errors may have crept in in the process of translating it into Ada.
   --
   -- I think that some of the arithmatic depends on the details of integer math
   -- dropping bits on underflow or overflow.
   --
   function get_temp(error : out integer) return integer is
      msb_value : uint8;
      lsb_value : uint8;
      temp : int16;
   begin
      if (last_cvt /= cvt_temp) then
         Ada.Text_IO.Put_Line("Last conversion request was not for temperature");
         raise Program_Error;
      end if;
      msb_value := BBS.BBB.i2c.read(addr, msb, error);
      lsb_value := BBS.BBB.i2c.read(addr, lsb, error);
      temp := uint16_to_int16(uint16(msb_value) * 256 + uint16(lsb_value));
      x1 := ((integer(temp) - integer(ac6)) * integer(ac5)) / 32768;
      x2 := integer(mc) * 2048 / (x1 + integer(md));
      b5 := x1 + x2;
      return (b5 + 8)/16;
   end;
   --
   function get_temp(error : out integer) return float is
      int_temp : integer := get_temp(error);
   begin
      return float(int_temp) / 10.0;
   end;
   --
   function get_temp(error : out integer) return BBS.units.temp_c is
      int_temp : integer := get_temp(error);
   begin
      return BBS.units.temp_c(float(int_temp) / 10.0);
   end;
   --
   function get_temp(error : out integer) return BBS.units.temp_f is
      int_temp : integer := get_temp(error);
   begin
      return BBS.units.to_Farenheit(BBS.units.temp_c(float(int_temp) / 10.0));
   end;
   --
   function get_temp(error : out integer) return BBS.units.temp_k is
      int_temp : integer := get_temp(error);
   begin
      return BBS.units.to_Kelvin(BBS.units.temp_c(float(int_temp) / 10.0));
   end;
   --
   function get_press(error : out integer) return integer is
      msb_value : uint8;
      lsb_value : uint8;
      xlsb_value : uint8;
      oss : uint8;
      oss_2 : integer;
      press : integer;
      b6 : integer;
      x1a : integer;
      x2a : integer;
      x3 : integer;
      b3 : integer;
      b4 : uint32;
      b7 : uint32;
   begin
      if (last_cvt /= cvt_press0) and (last_cvt /= cvt_press1) and
        (last_cvt /= cvt_press2) and (last_cvt /= cvt_press3) then
         Ada.Text_IO.Put_Line("Last conversion request was not for pressure");
         raise Program_Error;
      end if;
      msb_value := BBS.BBB.i2c.read(addr, msb, error);
      lsb_value := BBS.BBB.i2c.read(addr, lsb, error);
      xlsb_value := BBS.BBB.i2c.read(addr, xlsb, error);
      press := uint32_to_int(uint32(msb_value) * 65536 + uint32(lsb_value) * 256 + uint32(xlsb_value));
      oss := (last_cvt / 64) and 3;
      case oss is
      when 0 =>
         press := press / 2 ** 8;
         oss_2 := 1;
      when 1 =>
         press := press / 2 ** 7;
         oss_2 := 2;
      when 2 =>
         press := press / 2 ** 6;
         oss_2 := 4;
      when 3 =>
         press := press / 2 ** 5;
         oss_2 := 8;
      when others =>
         Ada.Text_IO.Put_Line("OSS value out of range " & integer'Image(integer(oss)));
         raise Program_Error;
      end case;
      b6 := b5 - 4000;
      x1a := (integer(b2) * (b6*b6/2**12))/2**11;
      x2a := integer(ac2)*b6/2**11;
      x3 := x1a + x2a;
      b3 := (((integer(ac1)*4 + x3)*oss_2) + 2)/4;
      x1a := integer(ac3)*b6/2**13;
      x2a := (integer(b1) * (b6*b6/2**12))/2**16;
      x3 := (x1a + x2a + 2)/2**2;
      b4 := uint32(ac4) * uint32(x3 + 32768)/2**15;
      b7 := (int_to_uint32(press) - uint32(b3))*(50000/uint32(oss_2));
      if (b7 < 16#80000000#) then
         press := integer((b7*2)/b4);
      else
         press := integer((b7/b4)*2);
      end if;
      x1a := (press/2**8)*(press/2**8);
      x1a := (x1a*3038)/2**16;
      x2a := (-7357*press)/2**16;
      press := press + (x1a + x2a + 3791)/2**4;
      return press;
   end;
   --
   function get_press(error : out integer) return BBS.units.press_p is
      int_press : integer;
   begin
      int_press := get_press(error);
      return BBS.units.press_p(int_press);
   end;
   --
   function get_press(error : out integer) return BBS.units.press_mb is
      int_press : integer;
   begin
      int_press := get_press(error);
      return BBS.units.to_milliBar(BBS.units.press_p(int_press));
   end;
   --
   function get_press(error : out integer) return BBS.units.press_atm is
      int_press : integer;
   begin
      int_press := get_press(error);
      return BBS.units.to_Atmosphere(BBS.units.press_p(int_press));
   end;
   --
   function get_press(error : out integer) return BBS.units.press_inHg is
      int_press : integer;
   begin
      int_press := get_press(error);
      return BBS.units.to_inHg(BBS.units.press_p(int_press));
   end;
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
   begin
      self.port := port;
      self.address := addr;
      --
      -- offsets into the buffer do not match the addresses.  Offset zero is
      -- equal to address 16#aa#.
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
      self.H4 := uint16_to_int16(self.port.readm2(self.address, dig_H4, error));
      self.H5 := uint16_to_int16(self.port.readm2(self.address, dig_H5, error));
      self.H6 := self.port.read(self.address, dig_H6, error);
   end;
   --
   procedure start_conversion(self : not null access BME280_record'class;
                              kind : uint8; error : out integer) is
   begin
      self.port.write(self.address, ctrl, kind, error);
      self.last_cvt := kind;
   end;
   --
   function data_ready(self : not null access BME280_record'class;
                       error : out integer) return boolean is
      byte : uint8;
      err : integer;
   begin
      byte := self.port.read(self.address, ctrl, err);
      error := err;
      if ((byte and start_cvt) /= start_cvt) and (err = 0) then
         return true;
      else
         return false;
      end if;
   end;
   --
   function get_temp(self : not null access BME280_record'class;
                     error : out integer) return integer is
      msb_value : uint8;
      lsb_value : uint8;
      temp : int16;
   begin
      if (self.last_cvt /= cvt_temp) then
         Ada.Text_IO.Put_Line("Last conversion request was not for temperature");
         raise Program_Error;
      end if;
      msb_value := self.port.read(self.address, msb, error);
      lsb_value := self.port.read(self.address, lsb, error);
      temp := uint16_to_int16(uint16(msb_value) * 256 + uint16(lsb_value));
      self.x1 := ((integer(temp) - integer(self.ac6)) * integer(self.ac5)) / 32768;
      self.x2 := integer(self.mc) * 2048 / (self.x1 + integer(self.md));
      self.b5 := self.x1 + self.x2;
      return (self.b5 + 8)/16;
   end;
   --
   function get_temp(self : not null access BME280_record'class;
                     error : out integer) return float is
      int_temp : integer := self.get_temp(error);
   begin
      return float(int_temp) / 10.0;
   end;
   --
   function get_temp(self : not null access BME280_record'class;
                     error : out integer) return BBS.units.temp_c is
      int_temp : integer := self.get_temp(error);
   begin
      return BBS.units.temp_c(float(int_temp) / 10.0);
   end;
   --
   function get_temp(self : not null access BME280_record'class;
                     error : out integer) return BBS.units.temp_f is
      int_temp : integer := self.get_temp(error);
   begin
      return BBS.units.to_Farenheit(BBS.units.temp_c(float(int_temp) / 10.0));
   end;
   --
   function get_temp(self : not null access BME280_record'class;
                     error : out integer) return BBS.units.temp_k is
      int_temp : integer := self.get_temp(error);
   begin
      return BBS.units.to_Kelvin(BBS.units.temp_c(float(int_temp) / 10.0));
   end;
   --
   function get_press(self : not null access BME280_record'class;
                      error : out integer) return integer is
      msb_value : uint8;
      lsb_value : uint8;
      xlsb_value : uint8;
      oss : uint8;
      oss_2 : integer;
      press : integer;
      b6a : integer;
      x1a : integer;
      x2a : integer;
      x3a : integer;
      b3a : integer;
      b4a : uint32;
      b7a : uint32;
   begin
      if (self.last_cvt /= cvt_press0) and (self.last_cvt /= cvt_press1) and
        (self.last_cvt /= cvt_press2) and (self.last_cvt /= cvt_press3) then
         Ada.Text_IO.Put_Line("Last conversion request was not for pressure");
         raise Program_Error;
      end if;
      msb_value := self.port.read(self.address, msb, error);
      lsb_value := self.port.read(self.address, lsb, error);
      xlsb_value := self.port.read(self.address, xlsb, error);
      press := uint32_to_int(uint32(msb_value) * 65536 + uint32(lsb_value) * 256 + uint32(xlsb_value));
      oss := (self.last_cvt / 64) and 3;
      case oss is
      when 0 =>
         press := press / 2 ** 8;
         oss_2 := 1;
      when 1 =>
         press := press / 2 ** 7;
         oss_2 := 2;
      when 2 =>
         press := press / 2 ** 6;
         oss_2 := 4;
      when 3 =>
         press := press / 2 ** 5;
         oss_2 := 8;
      when others =>
         Ada.Text_IO.Put_Line("OSS value out of range " & integer'Image(integer(oss)));
         raise Program_Error;
      end case;
      b6a := self.b5 - 4000;
      x1a := (integer(self.b2) * (b6a*b6a/2**12))/2**11;
      x2a := integer(self.ac2)*b6a/2**11;
      x3a := x1a + x2a;
      b3a := (((integer(self.ac1)*4 + x3a)*oss_2) + 2)/4;
      x1a := integer(self.ac3)*b6a/2**13;
      x2a := (integer(self.b1) * (b6a*b6a/2**12))/2**16;
      x3a := (x1a + x2a + 2)/2**2;
      b4a := uint32(self.ac4) * uint32(x3a + 32768)/2**15;
      b7a := (int_to_uint32(press) - uint32(b3a))*(50000/uint32(oss_2));
      if (b7a < 16#80000000#) then
         press := integer((b7a*2)/b4a);
      else
         press := integer((b7a/b4a)*2);
      end if;
      x1a := (press/2**8)*(press/2**8);
      x1a := (x1a*3038)/2**16;
      x2a := (-7357*press)/2**16;
      press := press + (x1a + x2a + 3791)/2**4;
      return press;
   end;
   --
   function get_press(self : not null access BME280_record'class;
                      error : out integer) return BBS.units.press_p is
      int_press : integer := self.get_press(error);
   begin
      return BBS.units.press_p(int_press);
   end;
   --
   function get_press(self : not null access BME280_record'class;
                      error : out integer) return BBS.units.press_mb is
      int_press : integer := self.get_press(error);
   begin
      return BBS.units.to_milliBar(BBS.units.press_p(int_press));
   end;
   --
   function get_press(self : not null access BME280_record'class;
                      error : out integer) return BBS.units.press_atm is
      int_press : integer := self.get_press(error);
   begin
      return BBS.units.to_Atmosphere(BBS.units.press_p(int_press));
   end;
   --
   function get_press(self : not null access BME280_record'class;
                      error : out integer) return BBS.units.press_inHg is
      int_press : integer := self.get_press(error);
   begin
      return BBS.units.to_inHg(BBS.units.press_p(int_press));
   end;

end;
