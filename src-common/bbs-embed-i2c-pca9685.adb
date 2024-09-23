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
--with Ada.Text_IO;
with BBS.embed.log;
package body BBS.embed.i2c.PCA9685 is
   --
   -- The PCA9685 has a default clock frequency of 25MHz.  This is divided by
   -- 4096 steps per pulse which gives a maximum pulse rate of 6.1kHz.  The
   -- prescale value is set to 30 which gives a pulse rate of 203Hz.  Each
   -- step then has a length of 1.2uS.
   --
   scale : constant uint8 := 30;
   --
   -- MODE1 bits are:
   --  7 - Restart
   --  6 - External Clk
   --  5 - Auto increment
   --  4 - Sleep
   --  3 - Sub1
   --  2 - Sub2
   --  1 - Sub3
   --  0 - All call
   --
   --  Since only constants are being used for MODE1, don't bother with
   --  defining a record type with all the bits broken out.  Just define
   --  some constants.
   --
   MODE1_Sleep : constant uint8 := 16#10#;  --  Put device to sleep
   MODE1_AIncr : constant uint8 := 16#20#;  --  Enable auto-increment mode
   --
   procedure configure(self : in out PS9685_record; port : i2c_interface;
                       addr : addr7; error : out err_code) is
   begin
      self.hw := port;
      self.address := addr;
      self.servo_min := (others => 0);
      self.servo_max := (others => 0);
      self.servo_set := (others => false);
      port.write(addr, MODE1, MODE1_Sleep, error);  -- Put PCA9685 to sleep
      port.write(addr, PRESCALE, scale, error);     -- Set prescale to 30
      port.write(addr, MODE1, MODE1_AIncr, error);  -- Wake up with auto increment
   end;
   --
   --  Sets the ON high and low, and the OFF high and low bytes.  This
   --  is done as a single 4 byte write starting at the address for the
   --  LEDx_ON_L register (on this device with auto-increment, this also
   --  writes the next three regsiters).
   --
   procedure set(self : PS9685_record; chan : channel;
                 on : uint12; off : uint12; error : out err_code) is
   begin
      self.hw.b(0) := uint8(on and 16#ff#);   --  LEDx_ON_L value
      self.hw.b(1) := uint8(on / 16#100#);    --  LEDx_ON_H value
      self.hw.b(2) := uint8(off and 16#ff#);  --  LEDx_OFF_L value
      self.hw.b(3) := uint8(off / 16#100#);   --  LEDx_OFF_H value
      self.hw.write(self.address, LED_ON_L(chan), buff_index(4), error);
   end;
   --
   function get_on(self : PS9685_record; chan : channel; error : out err_code) return uint16 is
   begin
      return self.hw.readm2(self.address, LED_ON_L(chan), error);
   end;
   --
   function get_off(self : PS9685_record; chan : channel; error : out err_code) return uint16 is
   begin
      return self.hw.readm2(self.address, LED_OFF_L(chan), error);
   end;
   --
   procedure set_full_on(self : PS9685_record; chan : channel;
                         error : out err_code) is
   begin
      self.hw.write(self.address, LED_ON_H(chan), uint8(16#10#), error);
      self.hw.write(self.address, LED_OFF_H(chan), uint8(16#0#), error);
   end;
   --
   procedure set_full_off(self : PS9685_record; chan : channel;
                          error : out err_code) is
   begin
      self.hw.write(self.address, LED_OFF_H(chan), uint8(16#10#), error);
      self.hw.write(self.address, LED_ON_H(chan), uint8(16#0#), error);
   end;
   --
   procedure sleep(self : PS9685_record; state : boolean;
                  error : out err_code) is
   begin
      if state then
         self.hw.write(self.address, MODE1, MODE1_Sleep, error); -- Put PCA9685 to sleep
      else
         self.hw.write(self.address, MODE1, MODE1_AIncr, error); -- Wake up with auto increment
      end if;
   end;
   --
   procedure set_servo_range(self : in out PS9685_record; chan : channel;
                         min : uint12; max : uint12) is
   begin
      self.servo_min(chan) := min;
      self.servo_max(chan) := max;
      self.servo_set(chan) := true;
   end;
   --
   -- Once the servo range has been set, a servo can be controlled by set_servo.
   --
   procedure set_servo(self : PS9685_record; chan : channel;
                       position : servo_range; error : out err_code) is
      temp : uint12;
      scale : float := float(self.servo_max(chan) - self.servo_min(chan)) / 2.0;
   begin
      if (self.servo_set(chan)) then
         temp := uint12((float(position) + 1.0)*scale) + self.servo_min(chan);
         self.set(chan, 0, temp, error);
      else
         raise program_error;
      end if;
   end;
   --
end;
