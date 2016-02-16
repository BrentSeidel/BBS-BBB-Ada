with BBS.BBB.i2c;
--
-- Constants for various devices on the I2C bus.
-- Device addresses and register addresses within the device are provided.
-- Refer to the data sheets for contents of these registers.
--
-- Once a specific device gets used often enough, it will likely get split off
-- into its own package.  This is especially true if some special logic of data
-- structures are used in the interface.
--
package BBS.BBB.i2c.devices is
   --
   -- List of addresses for devices in separate packages.  These are listed here
   -- in address order to help check for duplicate addresses.
   --
   addr_LMS303DLHC_accel : constant addr7 := 16#19#; -- Accelerometer
   addr_LMS303DLHC_mag : constant addr7 := 16#1e#; -- Magnetometer
   addr_L3GD20H : constant addr7 := 16#6b#; -- Gyroscope
   addr_BMP180 : constant addr7 := 16#77#;
   --
   -- Define some useful constants.
   --
end;
