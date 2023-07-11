--
--  Package body for driving the MCP4725 DAC.
--
package body BBS.embed.i2c.MCP4725 is
   --
   procedure configure(self : in out MCP4725_record; port : i2c_interface;
                       addr : addr7; error : out err_code) is
   begin
      self.hw := port;
      self.address := addr;
      error := none;
   end;
--
--  MCP4725 data
--
--  Commands (3 bits)
--  0 - Fast write
--  1 - Fast write
--  2 - Write Command for DAC Input
--  3 - Write Command for DAC Input and EEPROM Write
--  4 - <Reserved>
--  5 - <Reserved>
--  6 - <Reserved>
--  7 - <Reserved>
--
--  Power-Down select
--  0 - Normal Mode
--  1 - 1k resistance to ground
--  2 - 100k resistance to ground
--  3 - 500k resistance to ground
--
   procedure set(self : in out MCP4725_record; value : uint12; err : out err_code) is
   begin
     --
     --  Fast write in normal mode.
     --
     self.hw.write(self.address, BBS.embed.uint8((value/16#100#) and 16#0F#),
      BBS.embed.uint8(value and 16#FF#), err);
   end;
   --
end;
