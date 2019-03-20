package body BBS.embed.i2c.PCA9685 is
   --
   -- Object oriented interface
   --
   procedure configure(self : in out PS9685_record; port : i2c_interface;
                       addr : addr7; error : out err_code) is
      --
      -- The PCA9685 has a default clock frequency of 25MHz.  This is divided by
      -- 4096 steps per pulse which gives a maximum pulse rate of 6.1kHz.  The
      -- prescale value is set to 30 which gives a pulse rate of 203Hz.  Each
      -- step then has a lenght of 1.2uS.
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
   begin
      self.hw := port;
      self.address := addr;
      self.servo_min := (others => 0);
      self.servo_max := (others => 0);
      self.servo_set := (others => false);
      port.write(addr, MODE1, uint8(16#10#), error); -- Put PCA9685 to sleep
      port.write(addr, PRESCALE, uint8(16#1E#), error); -- Set prescale to 30
      port.write(addr, MODE1, uint8(16#20#), error); -- Wake up with auto increment
   end;

   procedure set(self : PS9685_record; chan : channel;
                 on : uint12; off : uint12; error : out err_code) is
   begin
      self.hw.b(0) := uint8(on and 16#ff#);
      self.hw.b(1) := uint8(on / 16#100#);
      self.hw.b(2) := uint8(off and 16#ff#);
      self.hw.b(3) := uint8(off / 16#100#);
      self.hw.write(self.address, LED_ON_L(chan), buff_index(4), error);
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
         self.hw.write(self.address, MODE1, uint8(16#10#), error); -- Put PCA9685 to sleep
      else
         self.hw.write(self.address, MODE1, uint8(16#20#), error); -- Wake up with auto increment
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
         null;
      else
         raise program_error;
      end if;
   end;
   --
end;
