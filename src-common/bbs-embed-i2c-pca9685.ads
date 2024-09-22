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
with BBS.embed.i2c;

package BBS.embed.i2c.PCA9685 is
   --
   -- Define constants and types used by the PCA9685 Pulse Width Modulation LED
   -- controller (16-channels, 12-bit PWM, I2C Bus).
   --
   -- In addition to controlling LEDs, it can control other PWM devices such as
   -- servo motors.  Note that LED brightness is controlled by the duty cycle and
   -- any duty cycle is valid, servos are controlled by the pulse width which
   -- should range from 1.5 to 2.5 mS.
   --
   -- Measured values for some servos in my testing are:
   -- Servo       Min   Max
   -- SG90        500  2100
   -- SG92        450  2050
   -- SG5010      500  2100
   --
   -- For a continuous rotation servo from Parallax, I got the following:
   -- Null:  1290 - 1350
   -- CW:   <1290
   -- CCW:  >1350
   --
   -- Note that all measured numbers are approximate.  There are probably a few
   -- counts left before hitting full scale movement.  It's also entirely possible
   -- that these values may vary with time, temperature, or other factors.
   --
   -- There are two things to keep in mind:
   -- 1. Test your own servos to determine their appropriate values.
   -- 2. If you want any sort of precision, you need some sort of position feed-
   --    back to the program.
   -- 3. The documentation that says that the pulse width for servos should range
   --    from 1.5 to 2.5 mS may not be accurate.
   --
   -- PWM channels are 0 to 15.  Channel 16 is the all call channel.
   --
   type channel is new integer range 0 .. 16;
   --
   -- The servo range type maps the servo position into a floating point number
   -- in the range -1.0 to 1.0.
   --
   type servo_range is new float range -1.0 .. 1.0;
   --
   ALL_CHAN : constant channel := 16;
   --
   -- For each channel there is a 12 bit counter and two thresholds: the on and
   -- the off threshold.  When the counter is equal to the on threshold, the
   -- output turns on.  When the counter is equal to the off threshold, the
   -- output turns off.  This allows the pulses to be staggered between the
   -- channels, if needed.
   -- --------------------------------------------------------
   --
   type PS9685_record is new i2c_device_record with private;
   type PS9685_ptr is access PS9685_record;
   --
   procedure configure(self : in out PS9685_record; port : i2c_interface;
                       addr : addr7; error : out err_code);
   --
   procedure set(self : PS9685_record; chan : channel;
                 on : uint12; off : uint12; error : out err_code);
   --
   procedure set_full_on(self : PS9685_record; chan : channel;
                  error : out err_code);
   --
   procedure set_full_off(self : PS9685_record; chan : channel;
                  error : out err_code);
   --
   -- State = true for sleep or false for wake.
   --
   procedure sleep(self : PS9685_record; state : boolean;
                  error : out err_code);
   --
   -- Methods for servos
   --
   procedure set_servo_range(self : in out PS9685_record; chan : channel;
                         min : uint12; max : uint12);
   --
   -- Once the servo range has been set, a servo can be controlled by set_servo.
   --
   procedure set_servo(self : PS9685_record; chan : channel;
                       position : servo_range; error : out err_code);
   --
private
   --
   --  Device register addresses
   --
   MODE1 : constant uint8 := 16#00#;
   MODE2 : constant uint8 := 16#01#;
   SUBADR1 : constant uint8 := 16#02#;
   SUBADR2 : constant uint8 := 16#03#;
   SUBADR3 : constant uint8 := 16#04#;
   ALLCALLADDR : constant uint8 := 16#05#;
   --
   --  Arrays containing register addresses for LED ON (High & Low byte)
   --  and LED OFF (High & Low byte) times.  The index is the channel number.
   --
   LED_ON_L : constant array (channel) of uint8 := (16#06#, 16#0A#, 16#0E#, 16#12#,
                                                    16#16#, 16#1A#, 16#1E#, 16#22#,
                                                    16#26#, 16#2A#, 16#2E#, 16#32#,
                                                    16#36#, 16#3A#, 16#3E#, 16#42#,
                                                    16#FA#);
   LED_ON_H : constant array (channel) of uint8 := (16#07#, 16#0B#, 16#0F#, 16#13#,
                                                    16#17#, 16#1B#, 16#1F#, 16#23#,
                                                    16#27#, 16#2B#, 16#2F#, 16#33#,
                                                    16#37#, 16#3B#, 16#3F#, 16#43#,
                                                    16#FB#);
   LED_OFF_L : constant array (channel) of uint8 := (16#08#, 16#0C#, 16#00#, 16#14#,
                                                     16#18#, 16#1C#, 16#10#, 16#24#,
                                                     16#28#, 16#2C#, 16#20#, 16#34#,
                                                     16#38#, 16#3C#, 16#30#, 16#44#,
                                                     16#FC#);
   LED_OFF_H : constant array (channel) of uint8 := (16#09#, 16#0D#, 16#01#, 16#15#,
                                                     16#19#, 16#1D#, 16#11#, 16#25#,
                                                     16#29#, 16#2D#, 16#21#, 16#35#,
                                                     16#39#, 16#3D#, 16#31#, 16#45#,
                                                     16#FD#);

   --
   PRESCALE : constant uint8 := 16#FE#;
   RESERVED : constant uint8 := 16#FF#;  --  Test mode, don't write to this register.
   --
   -- Array type definitions
   --
   type servo_array is array (channel) of uint12;
   type servo_set_array is array (channel) of boolean;
   --
   -- Object definition
   --
   type PS9685_record is new i2c_device_record with record
      servo_min : servo_array;
      servo_max : servo_array;
      servo_set : servo_set_array;
   end record;
   --
   -- Buffer for writes
   --
   i2c_buff : aliased buffer;
   --
end;
