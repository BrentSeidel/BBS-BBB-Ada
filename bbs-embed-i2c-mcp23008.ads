package BBS.embed.i2c.MCP23008 is
   --
   -- Addresses for MCP23008 - 8 Bit I/O Expander
   --
   -- The MCP23008 has 3 address select pins so it can have one of 8 addresses.
   -- This allows up to 8 of the devices on a single I2C bus.  The MCP23S08 has
   -- 2 address select pins allowing only 4 addresses.  These are the same as
   -- the first 4 addresses of the MCP23008.
   --
   addr_0 : constant addr7 := 16#20#;
   addr_1 : constant addr7 := 16#21#;
   addr_2 : constant addr7 := 16#22#;
   addr_3 : constant addr7 := 16#23#;
   addr_4 : constant addr7 := 16#24#;
   addr_5 : constant addr7 := 16#25#;
   addr_6 : constant addr7 := 16#26#;
   addr_7 : constant addr7 := 16#27#;
   --
   IODIR : constant uint8 := 16#00#; -- I/O Direction
   IPOL : constant uint8 := 16#01#; -- Input polarity
   GPINTEN : constant uint8 := 16#02#; -- Interrupt-on-change control
   DEFVAL : constant uint8 := 16#03#; --Default compare
   INTCON : constant uint8 := 16#04#; -- Interrupt control
   IOCON : constant uint8 := 16#05#; --Configuration
   GPPU : constant uint8 := 16#06#; -- Pull-up resistor configuration
   INTF : constant uint8 := 16#07#; -- Interrupt flag
   INTCAP : constant uint8 := 16#08#; -- Interrupt capture
   GPIO : constant uint8 := 16#09#; -- Port register
   OLAT : constant uint8 := 16#0a#; -- Output latch
   --
   all_write : constant uint8 := 16#00#;
   all_read : constant uint8 := 16#FF#;
   --
   -- Note: If you are using this chip in the AdaFruit i2c LCD backpack, the
   -- output databits are connected as follows (header pin numbers in parenthesis):
   -- GP0 - not connected
   -- GP1 - RS (4)
   -- GP2 - E (6)
   -- GP3 - DB4 (11)
   -- GP4 - DB5 (12)
   -- GP5 - DB6 (13)
   -- GP6 - DB7 (14)
   -- GP7 - LITE (16 - switches a transistor to GND)
   --
   --
   -- Stuff for object oriented interface.  A non-object oriented interface
   -- is not provided for this device.  If you need one, it should be fairly
   -- easy to write one.
   --
   -- Note that only the basic functions are covered.  The main purpose for this
   -- is to support its use as the LCD backpack controller.  If you need additional
   -- features, it should be easy enough to add them.
   --
   type MCB23008_record is new i2c_device_record with private;
   type MCB23008_ptr is access MCB23008_record;
   --
   function i2c_new return MCB23008_ptr;
   --
   -- The configure procedure needs to be called first to initialize the
   -- calibration constants from the device.
   --
   procedure configure(self : not null access MCB23008_record'class; port : i2c_interface;
                       addr : addr7; error : out integer);
   --
   -- Set the direction (read(0)/write(1)) for each of the output bits.  The
   -- direction bits are packed into a uint8.
   --
   procedure set_dir(self : not null access MCB23008_record'class; dir : uint8;
                     error : out integer);
   --
   -- Sets the output bits.  Bits are packed into a uint8.
   --
   procedure set_data(self : not null access MCB23008_record'class; data : uint8;
                      error : out integer);
   --
   -- Read the port.  Bits are packed into a uint8.
   --
   function read_data(self : not null access MCB23008_record'class; error : out integer)
                      return uint8;
private
   buff : aliased buffer;
   --
   type MCB23008_record is new i2c_device_record with record
      buff : aliased buffer;
   end record;

end;
