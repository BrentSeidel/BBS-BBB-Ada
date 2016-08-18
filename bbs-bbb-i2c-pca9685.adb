package body BBS.BBB.i2c.PCA9685 is

   --
   -- Simple object oriented interface
   --
   procedure setup(port : BBS.BBB.i2c.i2c_interface; addr : addr7) is
      error : integer;
   begin
      port.write(addr_0, MODE1, 16#10#, error);
      port.write(addr_0, PRESCALE, 16#1E#, error);
      port.write(addr_0, MODE1, 16#00#, error);
   end;
   --
   procedure set(port : BBS.BBB.i2c.i2c_interface; addr : addr7; chan : channel;
                  on : uint12; off : uint12) is
      error : integer;
      t_low : uint8;
      t_high : uint8;
   begin
      t_low := uint8(on and 16#ff#);
      t_high := uint8(on / 16#100#);
      port.write(addr_0, LED_ON_L(chan), t_low, error);
      port.write(addr_0, LED_ON_H(chan), t_high, error);
      t_low := uint8(off and 16#ff#);
      t_high := uint8(off / 16#100#);
      port.write(addr_0, LED_OFF_L(chan), t_low, error);
      port.write(addr_0, LED_OFF_H(chan), t_high, error);
   end;
   --
   -- Object oriented interface
   --
   function i2c_new return PS9685_ptr is
   begin
      return new PS9685_record;
   end;
   --
   procedure configure(self : not null access PS9685_record'class; port : i2c_interface;
                       addr : addr7; error : out integer) is
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
      self.port := port;
      self.address := addr;
      port.write(addr, MODE1, 16#10#, error); -- Put PCA9685 to sleep
      port.write(addr, PRESCALE, 16#1E#, error); -- Set prescale to 30
      port.write(addr, MODE1, 16#20#, error); -- Wake up with auto increment
   end;

   procedure set(self : not null access PS9685_record'class; chan : channel;
                 on : uint12; off : uint12) is
      error : integer;
   begin
      i2c_buff(0) := uint8(on and 16#ff#);
      i2c_buff(1) := uint8(on / 16#100#);
      i2c_buff(2) := uint8(off and 16#ff#);
      i2c_buff(3) := uint8(off / 16#100#);
--      self.port.write(self.address, LED_ON_L(chan), i2c_buff(0), error);
--      self.port.write(self.address, LED_ON_H(chan), i2c_buff(1), error);
--      self.port.write(self.address, LED_OFF_L(chan), i2c_buff(2), error);
--      self.port.write(self.address, LED_OFF_H(chan), i2c_buff(3), error);
      self.port.write(self.address, LED_ON_L(chan), i2c_buff'access, 4, error);
   end;
end;
