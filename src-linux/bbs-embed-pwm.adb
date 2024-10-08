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
package body BBS.embed.PWM is
	--
   function pwm_new return PWM is
   begin
      return new PWM_record;
   end;
   --
   procedure configure(self : not null access PWM_record'class;
                       pin : string; index : pwm_range) is
      temp : Ada.Text_IO.File_Type;
   begin
      --
      -- Set pin function
      --
      Ada.Text_IO.Open(temp, Ada.Text_IO.Out_File, pin);
      Ada.Text_IO.Put_Line(temp, "pwm");
      Ada.Text_IO.Close(temp);
      --
      -- Setup the record
      --
      self.PWM_index := index;
      self.period := 0;
      self.high_time := 0;
   end;
   --
   procedure enable(self : not null access PWM_record'class; state : boolean) is
      temp : Ada.Text_IO.File_Type;
   begin
      Ada.Text_IO.Open(temp, Ada.Text_IO.Out_File, pwm_ctrl(self.pwm_index).all & "/enable");
      if state then
         Ada.Text_IO.Put_Line(temp, "1");
      else
         Ada.Text_IO.Put_Line(temp, "0");
      end if;
      Ada.Text_IO.Close(temp);
   end;
   --
   procedure set_period(self : not null access PWM_record'class; period : nanoseconds) is
      temp : Ada.Text_IO.File_Type;
      value : constant string := Ada.Strings.Fixed.Trim(integer'Image(integer(period)),
                                                          Ada.Strings.Left);
   begin
      Ada.Text_IO.Open(temp, Ada.Text_IO.Out_File, pwm_ctrl(self.pwm_index).all & "/period");
      Ada.Text_IO.Put_Line(temp, value);
      self.period := period;
      Ada.Text_IO.Close(temp);
   end;
   --
   procedure set_period(self : not null access PWM_record'class; period : Duration) is
      temp : Ada.Text_IO.File_Type;
      value : constant string := Ada.Strings.Fixed.Trim(integer'Image(integer(period*1_000_000_000.0)),
                                                          Ada.Strings.Left);
   begin
      Ada.Text_IO.Open(temp, Ada.Text_IO.Out_File, pwm_ctrl(self.pwm_index).all & "/period");
      Ada.Text_IO.Put_Line(temp, value);
      self.period := nanoseconds(period*1_000_000_000.0);
      Ada.Text_IO.Close(temp);
   end;
   --
   procedure set_high(self : not null access PWM_record'class; high : nanoseconds) is
      temp : Ada.Text_IO.File_Type;
      value : constant string := Ada.Strings.Fixed.Trim(integer'Image(integer(high)),
                                                          Ada.Strings.Left);
   begin
      Ada.Text_IO.Open(temp, Ada.Text_IO.Out_File, pwm_ctrl(self.pwm_index).all & "/duty_cycle");
      Ada.Text_IO.Put_Line(temp, value);
      self.high_time := high;
      Ada.Text_IO.Close(temp);
   end;
   --
   procedure set_high(self : not null access PWM_record'class; high : Duration) is
      temp : Ada.Text_IO.File_Type;
      value : constant string := Ada.Strings.Fixed.Trim(integer'Image(integer(high*1_000_000_000.0)),
                                                          Ada.Strings.Left);
   begin
      Ada.Text_IO.Open(temp, Ada.Text_IO.Out_File, pwm_ctrl(self.pwm_index).all & "/duty_cycle");
      Ada.Text_IO.Put_Line(temp, value);
      self.high_time := nanoseconds(high*1_000_000_000.0);
      Ada.Text_IO.Close(temp);
   end;
   --
   procedure set_rate(self : not null access PWM_record'class; rate : BBS.units.freq_hz) is
      temp : Ada.Text_IO.File_Type;
      period : constant nanoseconds := nanoseconds(1_000_000_000.0/float(rate));
      value : constant string := Ada.Strings.Fixed.Trim(integer'Image(integer(period)),
                                                          Ada.Strings.Left);
   begin
      Ada.Text_IO.Open(temp, Ada.Text_IO.Out_File, pwm_ctrl(self.pwm_index).all & "/period");
      Ada.Text_IO.Put_Line(temp, value);
      self.period := period;
      Ada.Text_IO.Close(temp);
   end;
   --
   procedure set_duty(self : not null access PWM_record'class; duty : float) is
      temp : Ada.Text_IO.File_Type;
      high : constant nanoseconds := nanoseconds(float(self.period)*duty/100.0);
      value : constant string := Ada.Strings.Fixed.Trim(integer'Image(integer(high)),
                                                          Ada.Strings.Left);
   begin
      Ada.Text_IO.Open(temp, Ada.Text_IO.Out_File, pwm_ctrl(self.pwm_index).all & "/duty_cycle");
      Ada.Text_IO.Put_Line(temp, value);
      self.high_time := high;
      Ada.Text_IO.Close(temp);
   end;
   --
end;
