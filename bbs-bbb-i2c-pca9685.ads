with BBS.BBB.i2c;

package BBS.BBB.i2c.PCA9685 is
   --
   -- Define constants and types used by the PCA9685 Pulse Width Modulation LED
   -- controller (16-channels, 12-bit PWM, I2C Bus).
   --
   -- In addition to controlling LEDs, it can control other PWM devices such as
   -- servo motors.  Note that LED brightness is controlled by the duty cycle and
   -- any duty cycle is valid, servos are controlled by the pulse width which
   -- should range from 1.5 to 2.5 mS.
   --
   -- Note that after playing with some servos, it appears that each servo may
   -- have a different range.  Thus, one will have to do some experimenting to
   -- find the correct upper and lower range for each servo.  Good starting points
   -- are 1275 for the lower limit and 2125 for the upper limit of counts to the
   -- pulse high time.
   --
   -- PWM channels are 0 to 15.  Channel 16 is the all call channel.
   --
   type channel is new integer range 0 .. 16;
   --
   ALL_CHAN : constant channel := 16;
   --
   addr_0 : constant BBS.BBB.addr7 := 16#40#;
   addr_1 : constant BBS.BBB.addr7 := 16#41#;
   addr_2 : constant BBS.BBB.addr7 := 16#42#;
   addr_3 : constant BBS.BBB.addr7 := 16#43#;
   --
   MODE1 : constant BBS.BBB.uint8 := 16#00#;
   MODE2 : constant BBS.BBB.uint8 := 16#01#;
   SUBADR1 : constant BBS.BBB.uint8 := 16#02#;
   SUBADR2 : constant BBS.BBB.uint8 := 16#03#;
   SUBADR3 : constant BBS.BBB.uint8 := 16#04#;
   ALLCALLADDR : constant BBS.BBB.uint8 := 16#05#;
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
   PRESCALE : constant BBS.BBB.uint8 := 16#FE#;
   RESERVED : constant BBS.BBB.uint8 := 16#FF#;
   --
   -- For each channel there is a 12 bit counter and two thresholds: the on and
   -- the off threshold.  When the counter is equal to the on threshold, the
   -- output turns on.  When the counter is equal to the off threshold, the
   -- output turns off.  This allows the pulses to be staggered between the
   -- channels, if needed.
   --
   -- Simple non-object oriented interface to get started.
   --
   procedure setup(port : BBS.BBB.i2c.i2c_interface; addr : addr7);
   procedure set(port : BBS.BBB.i2c.i2c_interface; addr : addr7; chan : channel;
                 on : uint12; off : uint12);
   -- --------------------------------------------------------
   -- Stuff for object oriented interface.  These basically emulate the function
   -- of the conventional routines above.
   --
   type PS9685_record is new i2c_device_record with private;
   type PS9685_ptr is access PS9685_record;
   --
   function i2c_new return PS9685_ptr;
   procedure configure(self : not null access PS9685_record'class; port : i2c_interface;
                       addr : addr7; error : out integer);
   --
   procedure set(self : not null access PS9685_record'class; chan : channel;
                 on : uint12; off : uint12; error : out integer);
   --
   procedure set_full_on(self : not null access PS9685_record'class; chan : channel;
                  error : out integer);
   --
   procedure set_full_off(self : not null access PS9685_record'class; chan : channel;
                  error : out integer);
   --
   -- State = true for sleep or false for wake.
   --
   procedure sleep(self : not null access PS9685_record'class; state : boolean;
                  error : out integer);

private
   --
   -- Object definition
   --
   type PS9685_record is new i2c_device_record with record
      addr : addr7;
   end record;
   --
   -- Buffer for writes
   --
   i2c_buff : aliased buffer;
   --
end;
