package body BBS.BBB.i2c.BMP180 is
   --
   -- Procedures to work with the BMP180 pressure and temperature sensor
   --
   --
   -- This procedure reads the calibration data from the sensor and uses it to
   -- generate a number of calibration constants.
   --
   procedure configure(error : out integer) is
   begin
      read(addr, cal_start, buff'access, uint16(cal_end - cal_start + 1), error);
      --
      -- offsets into the buffer do not match the addresses.  Offset zero is
      -- equal to address 16#aa#.
      --
      ac1 := uint16_to_int16(uint16(buff(0)) * 256 + uint16(buff(1)));
      ac2 := uint16_to_int16(uint16(buff(2)) * 256 + uint16(buff(3)));
      ac3 := uint16_to_int16(uint16(buff(4)) * 256 + uint16(buff(5)));
      ac4 := uint16(buff(6)) * 256 + uint16(buff(7));
      ac5 := uint16(buff(8)) * 256 + uint16(buff(9));
      ac6 := uint16(buff(10)) * 256 + uint16(buff(11));
      b1 := uint16_to_int16(uint16(buff(12)) * 256 + uint16(buff(13)));
      b2 := uint16_to_int16(uint16(buff(14)) * 256 + uint16(buff(15)));
      mb := uint16_to_int16(uint16(buff(16)) * 256 + uint16(buff(17)));
      mc := uint16_to_int16(uint16(buff(18)) * 256 + uint16(buff(19)));
      md := uint16_to_int16(uint16(buff(20)) * 256 + uint16(buff(21)));
      if (dump_values) then
         Ada.Text_IO.Put("Constant AC1 has a value of " & integer'Image(integer(ac1)) & " (");
         Ada.Integer_Text_IO.Put(integer(ac1), 10, 16);
         Ada.Text_IO.Put_Line(")");
         Ada.Text_IO.Put("Constant AC2 has a value of " & integer'Image(integer(ac2)) & " (");
         Ada.Integer_Text_IO.Put(integer(ac2), 10, 16);
         Ada.Text_IO.Put_Line(")");
         Ada.Text_IO.Put("Constant AC3 has a value of " & integer'Image(integer(ac3)) & " (");
         Ada.Integer_Text_IO.Put(integer(ac3), 10, 16);
         Ada.Text_IO.Put_Line(")");
         Ada.Text_IO.Put("Constant AC4 has a value of " & integer'Image(integer(ac4)) & " (");
         Ada.Integer_Text_IO.Put(integer(ac4), 10, 16);
         Ada.Text_IO.Put_Line(")");
         Ada.Text_IO.Put("Constant AC5 has a value of " & integer'Image(integer(ac5)) & " (");
         Ada.Integer_Text_IO.Put(integer(ac5), 10, 16);
         Ada.Text_IO.Put_Line(")");
         Ada.Text_IO.Put("Constant AC6 has a value of " & integer'Image(integer(ac6)) & " (");
         Ada.Integer_Text_IO.Put(integer(ac6), 10, 16);
         Ada.Text_IO.Put_Line(")");
         Ada.Text_IO.Put("Constant B1 has a value of " & integer'Image(integer(b1)) & " (");
         Ada.Integer_Text_IO.Put(integer(b1), 10, 16);
         Ada.Text_IO.Put_Line(")");
         Ada.Text_IO.Put("Constant B2 has a value of " & integer'Image(integer(b2)) & " (");
         Ada.Integer_Text_IO.Put(integer(b2), 10, 16);
         Ada.Text_IO.Put_Line(")");
         Ada.Text_IO.Put("Constant MB has a value of " & integer'Image(integer(mb)) & " (");
         Ada.Integer_Text_IO.Put(integer(mb), 10, 16);
         Ada.Text_IO.Put_Line(")");
         Ada.Text_IO.Put("Constant MC has a value of " & integer'Image(integer(mc)) & " (");
         Ada.Integer_Text_IO.Put(integer(mc), 10, 16);
         Ada.Text_IO.Put_Line(")");
         Ada.Text_IO.Put("Constant MD has a value of " & integer'Image(integer(md)) & " (");
         Ada.Integer_Text_IO.Put(integer(md), 10, 16);
         Ada.Text_IO.Put_Line(")");
      end if;
   end;
   --
   procedure start_conversion(kind : uint8; error : out integer) is
   begin
      BBS.BBB.i2c.write(addr, ctrl, kind, error);
      last_cvt := kind;
   end;
   --
   function data_ready(error : out integer) return boolean is
      byte : uint8;
      err : integer;
   begin
      byte := BBS.BBB.i2c.read(addr, ctrl, err);
      error := err;
      if ((byte and start_cvt) /= start_cvt) and (err = 0) then
         return true;
      else
         return false;
      end if;
   end;
   --
   -- The calculations to get calibrated temperature and pressure are based on
   -- the algorithm in the datasheet.  It does not explain the meaning of the
   -- various constants or steps in the process.  It is also not unlikely that
   -- some errors may have crept in in the process of translating it into Ada.
   --
   -- I think that some of the arithmatic depends on the details of integer math
   -- dropping bits on underflow or overflow.
   --
   function get_temp(error : out integer) return float is
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
      cal_temp := (b5 + 8)/16;
      return float(cal_temp) / 10.0;
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
end;
