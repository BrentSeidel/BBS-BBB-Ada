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
   -- Define some useful constants.
   --
   --
   -- Addresses for BMP180 - Digital Pressure Sensor
   --
   addr_press : constant addr7 := 16#77#;
   press_xlsb : constant uint8 := 16#f8#;
   press_lsb : constant uint8 := 16#f7#;
   press_msb : constant uint8 := 16#f6#;
   press_ctrl : constant uint8 := 16#f4#;
   press_reset : constant uint8 := 16#e0#;
   press_id : constant uint8 := 16#d0#;
end;
