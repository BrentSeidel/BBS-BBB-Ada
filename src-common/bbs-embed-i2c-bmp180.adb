with BBS.embed.log;
package body BBS.embed.i2c.BMP180 is
   --
   procedure configure(self : in out BMP180_record; port : i2c_interface;
                       addr : addr7; error : out err_code) is
   begin
      self.hw := port;
      self.address := addr;
      BBS.embed.log.info.put_line("BMP180 Info: Configuring and reading calibration values.");
      self.hw.read(self.address, cal_start, buff_index(cal_end - cal_start + 1), error);
      --
      -- offsets into the buffer do not match the addresses.  Offset zero is
      -- equal to address 16#aa#.
      --
      self.ac1 := uint16_to_int16(uint16(self.hw.b(0)) * 256 + uint16(self.hw.b(1)));
      self.ac2 := uint16_to_int16(uint16(self.hw.b(2)) * 256 + uint16(self.hw.b(3)));
      self.ac3 := uint16_to_int16(uint16(self.hw.b(4)) * 256 + uint16(self.hw.b(5)));
      self.ac4 := uint16(self.hw.b(6)) * 256 + uint16(self.hw.b(7));
      self.ac5 := uint16(self.hw.b(8)) * 256 + uint16(self.hw.b(9));
      self.ac6 := uint16(self.hw.b(10)) * 256 + uint16(self.hw.b(11));
      self.b1 := uint16_to_int16(uint16(self.hw.b(12)) * 256 + uint16(self.hw.b(13)));
      self.b2 := uint16_to_int16(uint16(self.hw.b(14)) * 256 + uint16(self.hw.b(15)));
      self.mb := uint16_to_int16(uint16(self.hw.b(16)) * 256 + uint16(self.hw.b(17)));
      self.mc := uint16_to_int16(uint16(self.hw.b(18)) * 256 + uint16(self.hw.b(19)));
      self.md := uint16_to_int16(uint16(self.hw.b(20)) * 256 + uint16(self.hw.b(21)));
   end;
   --
   procedure start_conversion(self : in out BMP180_record;
                              kind : uint8; error : out err_code) is
   begin
--      BBS.embed.log.info.put_line("BMP180 Info: Starting Conversion.");
      self.hw.write(self.address, ctrl, kind, error);
      self.last_cvt := kind;
   end;
   --
   function data_ready(self : BMP180_record;
                       error : out err_code) return boolean is
      byte : uint8;
      err : err_code;
   begin
--      BBS.embed.log.info.put_line("BMP180 Info: Checking for data ready.");
      byte := self.hw.read(self.address, ctrl, err);
      error := err;
      if ((byte and start_cvt) /= start_cvt) and (err = none) then
         return true;
      else
         return false;
      end if;
   end;
   --
   function get_temp(self : in out BMP180_record;
                     error : out err_code) return integer is
      msb_value : uint8;
      lsb_value : uint8;
      temp : int16;
   begin
      if (self.last_cvt /= cvt_temp) then
         BBS.embed.log.error.Put_Line("BMP180 Error: Last conversion request was not for temperature");
         raise Program_Error;
      end if;
      msb_value := self.hw.read(self.address, msb, error);
      lsb_value := self.hw.read(self.address, lsb, error);
      temp := uint16_to_int16(uint16(msb_value) * 256 + uint16(lsb_value));
      self.x1 := ((integer(temp) - integer(self.ac6)) * integer(self.ac5)) / 32768;
      self.x2 := integer(self.mc) * 2048 / (self.x1 + integer(self.md));
      self.b5 := self.x1 + self.x2;
      return (self.b5 + 8)/16;
   end;
   --
   function get_temp(self : in out BMP180_record;
                     error : out err_code) return float is
      int_temp : integer := self.get_temp(error);
   begin
      return float(int_temp) / 10.0;
   end;
   --
   function get_temp(self : in out BMP180_record;
                     error : out err_code) return BBS.units.temp_c is
      int_temp : integer := self.get_temp(error);
   begin
      return BBS.units.temp_c(float(int_temp) / 10.0);
   end;
   --
   function get_temp(self : in out BMP180_record;
                     error : out err_code) return BBS.units.temp_f is
      int_temp : integer := self.get_temp(error);
   begin
      return BBS.units.to_Farenheit(BBS.units.temp_c(float(int_temp) / 10.0));
   end;
   --
   function get_temp(self : in out BMP180_record;
                     error : out err_code) return BBS.units.temp_k is
      int_temp : integer := self.get_temp(error);
   begin
      return BBS.units.to_Kelvin(BBS.units.temp_c(float(int_temp) / 10.0));
   end;
   --
   function get_press(self : BMP180_record;
                      error : out err_code) return integer is
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
         BBS.embed.log.error.Put_Line("BMP180 Error: Last conversion request was not for pressure");
         raise Program_Error;
      end if;
      msb_value := self.hw.read(self.address, msb, error);
      lsb_value := self.hw.read(self.address, lsb, error);
      xlsb_value := self.hw.read(self.address, xlsb, error);
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
         BBS.embed.log.error.Put_Line("BMP180 Error: OSS value out of range " & integer'Image(integer(oss)));
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
   function get_press(self : BMP180_record;
                      error : out err_code) return BBS.units.press_p is
      int_press : integer := self.get_press(error);
   begin
      return BBS.units.press_p(int_press);
   end;
   --
   function get_press(self : BMP180_record;
                      error : out err_code) return BBS.units.press_mb is
      int_press : integer := self.get_press(error);
   begin
      return BBS.units.to_milliBar(BBS.units.press_p(int_press));
   end;
   --
   function get_press(self : BMP180_record;
                      error : out err_code) return BBS.units.press_atm is
      int_press : integer := self.get_press(error);
   begin
      return BBS.units.to_Atmosphere(BBS.units.press_p(int_press));
   end;
   --
   function get_press(self : BMP180_record;
                      error : out err_code) return BBS.units.press_inHg is
      int_press : integer := self.get_press(error);
   begin
      return BBS.units.to_inHg(BBS.units.press_p(int_press));
   end;

end;
