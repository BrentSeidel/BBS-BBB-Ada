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
      BBS.BBB.i2c.write(BBS.BBB.i2c.BMP180.addr, BBS.BBB.i2c.BMP180.ctrl, kind, error);
      last_cvt := kind;
   end;
   --
   function get_temp(error : out integer) return float is
      msb_value : uint8;
      lsb_value : uint8;
      temp : int16;
   begin
      msb_value := BBS.BBB.i2c.read(addr, msb, error);
      lsb_value := BBS.BBB.i2c.read(addr, lsb, error);
      temp := uint16_to_int16(uint16(msb_value) * 256 + uint16(lsb_value));
      x1 := ((integer(temp) - integer(ac6)) * integer(ac5)) / 32768;
      x2 := integer(mc) * 2048 / (x1 + integer(md));
      b5 := x1 + x2;
      cal_temp := (b5 + 8)/16;
      return float(cal_temp) / 10.0;
   end;

end;
