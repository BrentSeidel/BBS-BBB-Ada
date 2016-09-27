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
   begin
      self.port.read(addr, data_start, buff'access, 8, error);
      self.press_msb  := buff(0);
      self.press_lsb  := buff(1);
      self.press_xlsb := buff(2);
      self.temp_msb  := buff(3);
      self.temp_lsb  := buff(4);
      self.temp_xlsb := buff(5);
      self.hum_msb := buff(6);
      self.hum_lsb := buff(7);

   end;
   --
   function get_temp(self : not null access BME280_record'class;
                     error : out integer) return integer is
      msb_value : uint8;
      lsb_value : uint8;
      temp : int16;
   begin
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
