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
with BBS.embed.BBB;
with BBS.units;
with Ada.Text_IO;
with Ada.Strings.Fixed;
package BBS.embed.PWM is
   --
   -- Mapping of PWMs to pins.  Note that there is currently no pinmux directory
   -- for pin P8_28, so the function for this pin cannot be changed (easily.  With
   -- enough effort, most anything can be changed).
   --
   --   ECAPPWM0 := P8_42;
   --   ECAPPWM2 := P8_28; -- No pinmux for this pin
   --   EHRPWM0A := P9_22; -- or P9_31
   --   EHRPWM0B := P9_21; -- or P9_29
   --   EHRPWM1A := P9_14; -- or P8_34
   --   EHRPWM1B := P9_16; -- or P8_36
   --   EHRPWM2A := P8_19; -- or P8_45
   --   EHRPWM2B := P8_13; -- or P8_46
   --
   -- The Linux PWM driver uses period and duty_cycle (actually time high) expressed
   -- in nanoseconds.  By the time that this is translated to the device, the
   -- actual resolution may be something different.  It is also not clear what
   -- the maximum value of period may be.  The maximum value may also depend on
   -- the specific PWM device being accessed.  Some experimentation would be in
   -- order to determine what works for your application.  If I can find out
   -- any more information, I will update this comment.
   --
   type nanoseconds is range 0 .. integer'Last;
   --
   -- There are actually 8 PWMs, but ECAPPWM2 does not seem to be easily accessible.
   --
   type pwm_range is range 0 .. 6;
   --
   -- To simplify things, the following array is provided for the accessible
   -- PWMs.
   --
   pwm_ctrl : constant array (pwm_range) of access constant String :=
     (BBS.embed.BBB.EHRPWM0A'Access, BBS.embed.BBB.EHRPWM0B'Access,
      BBS.embed.BBB.EHRPWM1A'Access, BBS.embed.BBB.EHRPWM1B'Access,
      BBS.embed.BBB.EHRPWM2A'Access, BBS.embed.BBB.EHRPWM2B'Access,
      BBS.embed.BBB.ECAPPWM0'Access);
   --
   -- The PWM object.  Note that while you can create multiple objects pointing
   -- to the same physical PWM, this will probably not be a good idea.
   --
   type PWM_record is tagged limited private;
   type PWM is access PWM_record;
   --
   -- Create a new PWM object.
   --
   function PWM_new return PWM;
   --
   -- Configure a new GPIO object.  The pin control file and GPIO directory
   -- must correspond, otherwise things will not work correctly.  Pin should
   -- be one of the pin constants and port should be one of
   -- the gpio constants from BBS.BBB.pins package.
   --
   procedure configure(self : not null access PWM_record'class;
                       pin : string; index : pwm_range);
   --
   -- Enable or disable the PWM
   --
   procedure enable(self : not null access PWM_record'class; state : boolean);
   --
   -- Set the period of the PWM.  This is the value used by Linux in Nanoseconds.
   -- Must be zero or positive.  Some of the PWM devices may have additional
   -- restrictions on the range.
   --
   procedure set_period(self : not null access PWM_record'class; period : nanoseconds);
   --
   -- Set the period of the PWM.  This is the value used by Linux in Nanoseconds.
   -- Must be zero or positive.  Some of the PWM devices may have additional
   -- restrictions on the range.
   --
   procedure set_period(self : not null access PWM_record'class; period : Duration);
   --
   -- Set the time that the output is in the high state.  This must be between
   -- zero and the period.
   --
   procedure set_high(self : not null access PWM_record'class; high : nanoseconds);
   --
   -- Set the time that the output is in the high state.  This must be between
   -- zero and the period.
   --
   procedure set_high(self : not null access PWM_record'class; high : Duration);
   --
   -- Sets the rate in Hz.  This is essentially the inverse of set_period.  The
   -- rate is given as a floating point number and is converted to an integer
   -- number of nanoseconds.
   --
   procedure set_rate(self : not null access PWM_record'class; rate : BBS.units.freq_hz);
   --
   -- Sets the duty cycle.  The duty cycle is a percentage of the period that
   -- the output is high.  The input is a floating point number and is converted
   -- to the appropriate number of nanoseconds.
   --
   procedure set_duty(self : not null access PWM_record'class; duty : float)
     with
     pre => (duty >= 0.0) and (duty <= 100.0);

private
   type PWM_record is tagged limited
      record
         pwm_index : pwm_range;
         period : nanoseconds;
         high_time : nanoseconds;
      end record;

end;
