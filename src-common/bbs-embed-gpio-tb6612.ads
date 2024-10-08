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
--
--  This package contains operations for the TB6612 dual H-Bridge driver.  This
--  can be used to drive a single stepper motor or two DC motors.
--
package BBS.embed.gpio.tb6612 is
   --
   type TB6612_record is tagged private;
   type TB6612_ptr is access TB6612_record;

   --
   --  Initialize the TB6612 device record.
   --
   procedure init(self : in out TB6612_record; pin_a : BBS.embed.GPIO.GPIO;
                  pin_b : BBS.embed.GPIO.GPIO; pin_c : BBS.embed.GPIO.GPIO;
                  pin_d : BBS.embed.GPIO.GPIO);
   --
   --  Set the delay time in mS between steps.  This can be set to 0 if the
   --  delay is being handled elsewhere.
   --
   procedure set_delay(self : in out TB6612_record; wait_time : Natural);
   --
   --  Step a stepper motor the specified number of steps.
   --
   procedure step(self : in out TB6612_record; steps : Integer);
   --
   --  De-energize the coils for a stepper motor.
   --
   procedure stepper_off(self : in out TB6612_record);
   --
   --  For controlling DC motors, each H-Bridge can be controlled separately.
   --  A positive value sets the polarity in one direction, a negative value
   --  reverses the polarity, and a zero value turns it off.
   --
   procedure set_bridge_a(self : in out TB6612_record; value : Integer);
   procedure set_bridge_b(self : in out TB6612_record; value : Integer);

private
   type TB6612_record is tagged record
      initialized : Boolean := False;
      pin_a : BBS.embed.GPIO.GPIO;
      pin_b : BBS.embed.GPIO.GPIO;
      pin_c : BBS.embed.GPIO.GPIO;
      pin_d : BBS.embed.GPIO.GPIO;
      phase : Integer;
      time  : Standard.Duration := 0.005;
   end record;
   --
   --  Phase sequence for stepper motor.
   --
   step_phase : constant array (1 .. 8, 1 .. 4) of bit :=
     ((1, 0, 1, 0),  --  1. +A +B
      (1, 0, 0, 0),  --  2. +A 0B
      (1, 0, 0, 1),  --  3. +A -B
      (0, 0, 0, 1),  --  4. 0A -B
      (0, 1, 0, 1),  --  5. -A -B
      (0, 1, 0, 0),  --  6. -A 0B
      (0, 1, 1, 0),  --  7. -A +B
      (0, 0, 1, 0)); --  8. 0A +B

end;
