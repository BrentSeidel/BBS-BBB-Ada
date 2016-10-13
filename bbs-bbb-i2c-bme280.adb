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
      self.raw_press := uint32(buff(0))*2**16 + uint32(buff(1))*2**8 + uint32(buff(2));
      self.raw_temp  := uint32(buff(3))*2**16 + uint32(buff(4))*2**8 + uint32(buff(5));
      self.raw_hum   := uint32(buff(6))*2**8  + uint32(buff(7));

   end;
   --
   function get_temp(self : not null access BME280_record'class) return integer is
   begin
      return integer(self.raw_temp);
   end;
   --
   function get_temp(self : not null access BME280_record'class) return BBS.units.temp_c is
      int_temp : integer := self.get_temp;
   begin
      return BBS.units.temp_c(float(int_temp) / 10.0);
   end;
   --
   function get_temp(self : not null access BME280_record'class) return BBS.units.temp_f is
      int_temp : integer := self.get_temp;
   begin
      return BBS.units.to_Farenheit(BBS.units.temp_c(float(int_temp) / 10.0));
   end;
   --
   function get_temp(self : not null access BME280_record'class) return BBS.units.temp_k is
      int_temp : integer := self.get_temp;
   begin
      return BBS.units.to_Kelvin(BBS.units.temp_c(float(int_temp) / 10.0));
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

end;
