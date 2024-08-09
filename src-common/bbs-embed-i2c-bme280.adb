--
--  Author: Brent Seidel
--  Date: 9-Aug-2024
--
--  This file is part of bbs_embed.
--  Bbs_embed is free software: you can redistribute it and/or modify it
--  under the terms of the GNU General Public License as published by the
--  Free Software Foundation, either version 3 of the License, or (at your
--  option) any later version.
--
--  bbs_embed is distributed in the hope that it will be useful, but
--  WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
--  Public License for more details.
--
--  You should have received a copy of the GNU General Public License along
--  with bbs_embed. If not, see <https://www.gnu.org/licenses/>.--
--
with BBS.embed.log;
package body BBS.embed.i2c.BME280 is
   --
   -- Object oriented interface
   --
   procedure configure(self : in out BME280_record; port : i2c_interface;
                       addr : addr7; error : out err_code) is
      temp_1 : uint8;
      temp_2 : uint8;
      temp_3 : uint8;
      temp_a : uint12;
      temp_b : uint12;
   begin
      self.hw := port;
      self.address := addr;
      --
      -- Calibration parameters.  Most of these are either two byte with LSB
      -- first or a single byte.  The two exceptions are H4 and H5.
      --
--      BBS.embed.log.info.put_line("BME280: Getting parameter T1");
      self.T1 := self.hw.readm2(self.address, dig_T1, error);
      if error /= BBS.embed.i2c.none then
         return;
      end if;
--      BBS.embed.log.info.put_line("BME280: Getting parameter T2");
      self.T2 := uint16_to_int16(self.hw.readm2(self.address, dig_T2, error));
      if error /= BBS.embed.i2c.none then
         return;
      end if;
--      BBS.embed.log.info.put_line("BME280: Getting parameter T3");
      self.T3 := uint16_to_int16(self.hw.readm2(self.address, dig_T3, error));
      if error /= BBS.embed.i2c.none then
         return;
      end if;
--      BBS.embed.log.info.put_line("BME280: Getting parameter P1");
      self.P1 := self.hw.readm2(self.address, dig_P1, error);
      if error /= BBS.embed.i2c.none then
         return;
      end if;
--      BBS.embed.log.info.put_line("BME280: Getting parameter P2");
      self.P2 := uint16_to_int16(self.hw.readm2(self.address, dig_P2, error));
      if error /= BBS.embed.i2c.none then
         return;
      end if;
--      BBS.embed.log.info.put_line("BME280: Getting parameter P3");
      self.P3 := uint16_to_int16(self.hw.readm2(self.address, dig_P3, error));
      if error /= BBS.embed.i2c.none then
         return;
      end if;
--      BBS.embed.log.info.put_line("BME280: Getting parameter P4");
      self.P4 := uint16_to_int16(self.hw.readm2(self.address, dig_P4, error));
      if error /= BBS.embed.i2c.none then
         return;
      end if;
--      BBS.embed.log.info.put_line("BME280: Getting parameter P5");
      self.P5 := uint16_to_int16(self.hw.readm2(self.address, dig_P5, error));
      if error /= BBS.embed.i2c.none then
         return;
      end if;
--      BBS.embed.log.info.put_line("BME280: Getting parameter P6");
      self.P6 := uint16_to_int16(self.hw.readm2(self.address, dig_P6, error));
      if error /= BBS.embed.i2c.none then
         return;
      end if;
--      BBS.embed.log.info.put_line("BME280: Getting parameter P7");
      self.P7 := uint16_to_int16(self.hw.readm2(self.address, dig_P7, error));
      if error /= BBS.embed.i2c.none then
         return;
      end if;
--      BBS.embed.log.info.put_line("BME280: Getting parameter P8");
      self.P8 := uint16_to_int16(self.hw.readm2(self.address, dig_P8, error));
      if error /= BBS.embed.i2c.none then
         return;
      end if;
--      BBS.embed.log.info.put_line("BME280: Getting parameter P9");
      self.P9 := uint16_to_int16(self.hw.readm2(self.address, dig_P9, error));
      if error /= BBS.embed.i2c.none then
         return;
      end if;
--      BBS.embed.log.info.put_line("BME280: Getting parameter H1");
      self.H1 := self.hw.read(self.address, dig_H1, error);
      if error /= BBS.embed.i2c.none then
         return;
      end if;
--      BBS.embed.log.info.put_line("BME280: Getting parameter H2");
      self.H2 := uint16_to_int16(self.hw.readm2(self.address, dig_H2, error));
      if error /= BBS.embed.i2c.none then
         return;
      end if;
--      BBS.embed.log.info.put_line("BME280: Getting parameter H3");
      self.H3 := self.hw.read(self.address, dig_H3, error);
      if error /= BBS.embed.i2c.none then
         return;
      end if;
      --
      -- Specification of H4 is given as 0xE4/0xE5[3:0] => dig_H4[11:4]/[3:0]
      -- Specification of H5 is given as 0xE5[7:4]/0xE6 => dig_H5[3:0]/[11:4]
      -- These are actually 12 bit integers packed into three bytes.
      --
      BBS.embed.log.info.put_line("BME280: Getting parameter H4");
      temp_1 := self.hw.read(self.address, dig_H4, error);
      if error /= BBS.embed.i2c.none then
         return;
      end if;
      BBS.embed.log.info.put_line("BME280: Getting parameter H4/H5");
      temp_2 := self.hw.read(self.address, dig_H45, error);
      if error /= BBS.embed.i2c.none then
         return;
      end if;
      BBS.embed.log.info.put_line("BME280: Getting parameter H5");
      temp_3 := self.hw.read(self.address, dig_H5, error);
      if error /= BBS.embed.i2c.none then
         return;
      end if;
      temp_a := uint12(temp_1)*16 + uint12(temp_2 mod 16);
      temp_b := uint12(temp_3)*16 + uint12(temp_2/16);
      self.H4 := int16(uint12_to_int12(temp_a));
      self.H5 := int16(uint12_to_int12(temp_b));
      self.H6 := self.hw.read(self.address, dig_H6, error);
      if debug then
         BBS.embed.log.debug.Put_Line("BME280 Calibration parameters");
         BBS.embed.log.debug.put_line("BME280: T1 = " & Integer'Image(integer(self.T1)));
         BBS.embed.log.debug.put_line("BME280: T2 = " & Integer'Image(integer(self.T2)));
         BBS.embed.log.debug.put_line("BME280: T3 = " & Integer'Image(integer(self.T3)));
         BBS.embed.log.debug.put_line("BME280: P1 = " & Integer'Image(integer(self.P1)));
         BBS.embed.log.debug.put_line("BME280: P2 = " & Integer'Image(integer(self.P2)));
         BBS.embed.log.debug.put_line("BME280: P3 = " & Integer'Image(integer(self.P3)));
         BBS.embed.log.debug.put_line("BME280: P4 = " & Integer'Image(integer(self.P4)));
         BBS.embed.log.debug.put_line("BME280: P5 = " & Integer'Image(integer(self.P5)));
         BBS.embed.log.debug.put_line("BME280: P6 = " & Integer'Image(integer(self.P6)));
         BBS.embed.log.debug.put_line("BME280: P7 = " & Integer'Image(integer(self.P7)));
         BBS.embed.log.debug.put_line("BME280: P8 = " & Integer'Image(integer(self.P8)));
         BBS.embed.log.debug.put_line("BME280: P9 = " & Integer'Image(integer(self.P9)));
         BBS.embed.log.debug.put_line("BME280: H1 = " & Integer'Image(integer(self.H1)));
         BBS.embed.log.debug.put_line("BME280: H2 = " & Integer'Image(integer(self.H2)));
         BBS.embed.log.debug.put_line("BME280: H3 = " & Integer'Image(integer(self.H3)));
         BBS.embed.log.debug.put_line("BME280: temp_1 = " & Integer'Image(integer(temp_1)));
         BBS.embed.log.debug.put_line("BME280: temp_2 = " & Integer'Image(integer(temp_2)));
         BBS.embed.log.debug.put_line("BME280: temp_3 = " & Integer'Image(integer(temp_3)));
         BBS.embed.log.debug.put_line("BME280: H4 = " & Integer'Image(integer(self.H4)));
         BBS.embed.log.debug.put_line("BME280: H5 = " & Integer'Image(integer(self.H5)));
         BBS.embed.log.debug.put_line("BME280: H6 = " & Integer'Image(integer(self.H6)));
      end if;
      --
      -- Now set the mode.  Use forced mode to keep the interface similar to
      -- the BMP180.
      --
      -- First put into sleep more so configuration can be set.  Oversampling
      -- is set to 1 for each parameter.
      --
      self.hw.write(self.address, ctrl_meas, mode_sleep, error);
      --
      -- Set humidity oversampling
      --
      self.hw.write(self.address, ctrl_hum, hum_over_1, error);
      --
      -- Temperature, pressure, and mode are in the same register so set them
      -- all at once.
      --
      self.hw.write(self.address, ctrl_meas, temp_over_1 + press_over_1 +
                        mode_force, error);
   end;
   --
   procedure start_conversion(self : BME280_record; error : out err_code) is
   begin
      self.hw.write(self.address, ctrl_meas, temp_over_1 + press_over_1 +
                        mode_force, error);
   end;
   --
   function data_ready(self : BME280_record;
                       error : out err_code) return boolean is
      byte : uint8;
      err : err_code;
   begin
      byte := self.hw.read(self.address, status, err);
      error := err;
      if ((byte and stat_measuring) /= stat_measuring) and (err = none) then
         return true;
      else
         return false;
      end if;
   end;
   --
   -- Read 3 bytes for pressure and temperature and 2 bytes for humidity. 8 bytes
   -- total
   --
   procedure read_data(self : in out BME280_record; error : out err_code) is
      --
      -- Nested functions to do conversion of the temperature, pressure, and
      -- humidity values.  These are based on the example C code in the datasheet
      -- and are not the clearest code.
      --
      -- Temperature conversion (t_fine needs a little more processing before
      -- generating the final temperature, but it is used in other processing)
      --
      function cal_temp(self : BME280_record) return int32 is
         var1 : int32;
         var2 : int32;
      begin
         var1 := (int32(self.raw_temp)/2**3 - int32(self.T1)*2)*int32(self.T2)/2**11;
         var2 := (int32(self.raw_temp)/2**4 - int32(self.T1))*
           ((int32(self.raw_temp)/2**4 - int32(self.T1))/2**12)*(int32(self.T3)/2**14);
         return var1 + var2;
      end;
      --
      -- Pressure conversion.  The result is in Pascals * 256.
      --
      function cal_press(self : BME280_record) return uint32 is
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
      function cal_hum(self : BME280_record) return uint32 is
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
      self.hw.read(addr, data_start, 8, error);
      self.raw_press := (uint32(self.hw.b(0))*2**16 + uint32(self.hw.b(1))*2**8 + uint32(self.hw.b(2)))/16;
      self.raw_temp  := (uint32(self.hw.b(3))*2**16 + uint32(self.hw.b(4))*2**8 + uint32(self.hw.b(5)))/16;
      self.raw_hum   := uint32(self.hw.b(6))*2**8  + uint32(self.hw.b(7));
      if (debug) then
         BBS.embed.log.debug.put_line("p_raw: " & uint32'Image(self.raw_press));
         BBS.embed.log.debug.put_line("t_raw: " & uint32'Image(self.raw_temp));
         BBS.embed.log.debug.put_line("h_raw: " & uint32'Image(self.raw_hum));
      end if;
      --
      -- Compute the calibrated values based on the algorithms in the datasheet.
      --
      self.t_fine := cal_temp(self);
      self.p_cal := cal_press(self);
      self.h_cal := cal_hum(self);
   end;
   --
   procedure get_raw(self : BME280_record; raw_temp : out uint32;
                     raw_press : out uint32; raw_hum : out uint32) is
   begin
      raw_temp := self.raw_temp;
      raw_press := self.raw_press;
      raw_hum := self.raw_hum;
   end;
   --
   function get_t_fine(self : BME280_record) return int32 is
   begin
      return self.t_fine;
   end;
   --
   function get_temp(self : BME280_record) return integer is
   begin
      return integer((self.t_fine*5 + 128)/2**8);
   end;
   --
   function get_temp(self : BME280_record) return BBS.units.temp_c is
      int_temp : integer := self.get_temp;
   begin
      return BBS.units.temp_c(float(int_temp) / 100.0);
   end;
   --
   function get_temp(self : BME280_record) return BBS.units.temp_f is
      int_temp : integer := self.get_temp;
   begin
      return BBS.units.to_Farenheit(BBS.units.temp_c(float(int_temp) / 100.0));
   end;
   --
   function get_temp(self : BME280_record) return BBS.units.temp_k is
      int_temp : integer := self.get_temp;
   begin
      return BBS.units.to_Kelvin(BBS.units.temp_c(float(int_temp) / 100.0));
   end;
   --
   function get_press(self : BME280_record) return integer is
   begin
      return integer(self.p_cal);
   end;
   --
   function get_press(self : BME280_record) return BBS.units.press_p is
      int_press : integer := self.get_press;
   begin
      return BBS.units.press_p(int_press)/256.0;
   end;
   --
   function get_press(self : BME280_record) return BBS.units.press_mb is
      int_press : integer := self.get_press;
   begin
      return BBS.units.to_milliBar(BBS.units.press_p(int_press)/256.0);
   end;
   --
   function get_press(self : BME280_record) return BBS.units.press_atm is
      int_press : integer := self.get_press;
   begin
      return BBS.units.to_Atmosphere(BBS.units.press_p(int_press)/256.0);
   end;
   --
   function get_press(self : BME280_record) return BBS.units.press_inHg is
      int_press : integer := self.get_press;
   begin
      return BBS.units.to_inHg(BBS.units.press_p(int_press)/256.0);
   end;
   --
   function get_hum(self : BME280_record) return integer is
   begin
      return integer(self.h_cal);
   end get_hum;
   --
   function get_hum(self : BME280_record) return float is
      int_value : integer := self.get_hum;
   begin
      return float(int_value)/1024.0;
   end get_hum;

end;
