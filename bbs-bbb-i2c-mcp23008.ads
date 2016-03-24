package BBS.BBB.i2c.MCP23008 is
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
end;
