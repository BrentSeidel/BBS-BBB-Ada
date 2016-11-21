with BBS.embed.i2c;
--
-- Constants for various devices on the I2C bus.
-- Device addresses and register addresses within the device are provided.
-- Refer to the data sheets for contents of these registers.
--
-- Once a specific device gets used often enough, it will likely get split off
-- into its own package.  This is especially true if some special logic of data
-- structures are used in the interface.
--
package BBS.embed.i2c.devices is
   --
   -- List of addresses for devices in separate packages.  These are listed here
   -- in address order to help check for duplicate addresses.
   --
   addr_LMS303DLHC_accel : constant addr7 := 16#19#; -- Accelerometer
   addr_LMS303DLHC_mag : constant addr7 := 16#1e#; -- Magnetometer
   --
   -- Addresses for MCP23008 - 8 Bit I/O Expander
   --
   -- The MCP23008 has 3 address select pins so it can have one of 8 addresses.
   -- This allows up to 8 of the devices on a single I2C bus.  The MCP23S08 has
   -- 2 address select pins allowing only 4 addresses.  These are the same as
   -- the first 4 addresses of the MCP23008.
   --
   addr_MCP23008_1 : constant addr7 := 16#20#;
   addr_MCP23008_2 : constant addr7 := 16#21#;
   addr_MCP23008_3 : constant addr7 := 16#22#;
   addr_MCP23008_4 : constant addr7 := 16#23#;
   addr_MCP23008_5 : constant addr7 := 16#24#;
   addr_MCP23008_6 : constant addr7 := 16#25#;
   addr_MCP23008_7 : constant addr7 := 16#26#;
   addr_MCP23008_8 : constant addr7 := 16#27#;
   --
   -- Addresses for PCA9685 PWM Controllers.  This has 6 address select pins so
   -- it can have 64 different addresses (apparently only 62 are usable.
   --
   addr_PCA9685_00 : constant addr7 := 16#40#;
   --
   addr_L3GD20H : constant addr7 := 16#6b#; -- Gyroscope
   addr_BMP180 : constant addr7 := 16#77#;
   --
   -- Define some useful constants.
   --
end;
