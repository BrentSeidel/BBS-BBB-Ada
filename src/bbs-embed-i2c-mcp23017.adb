package body BBS.embed.i2c.MCP23017 is
   --
   --
   -- Object oriented interface
   --
   --
   --  Configure the device to work as a single 16 bit I/O port.  The default
   --  Power-on/Reset values should work.
   --
   procedure configure(self : in out MCP23017_record; port : i2c_interface;
                       addr : addr7; error : out err_code) is
      pragma Unreferenced(error);
   begin
      self.hw := port;
      self.address := addr;
   end;
   --
   -- Set the direction (read(0)/write(1)) for each of the output bits.  The
   -- direction bits are packed into a uint16.
   --
   procedure set_dir(self : MCP23017_record; dir : uint16;
                     error : out err_code) is
   begin
--      self.hw.writem2(self.address, IODIR, dir, error);
      self.hw.b(0) := uint8(dir and 16#ff#);
      self.hw.b(1) := uint8(dir / 16#100#);
      self.hw.write(self.address, IODIR, buff_index(2), error);
   end;
   --
   function get_dir(self : MCP23017_record;
                    error : out err_code) return uint16 is
   begin
      return self.hw.readm2(self.address, IODIR, error);
   end;
   --
   --  Enable/Disable weak pullup resistors (disable(0)/enable(1)) for each
   --  of the output bits.  The bits are packed into a uint16.
   --
   procedure set_pullup(self : MCP23017_record; dir : uint16;
                        error : out err_code) is
   begin
      self.hw.writem2(self.address, GPPU, dir, error);
   end;
   --
   function get_pullup(self : MCP23017_record;
                     error : out err_code) return uint16 is
   begin
      return self.hw.readm2(self.address, GPPU, error);
   end;
   --
   -- Set the polarity (normal(0)/inverted(1)) for each of the input bits.  The
   -- direction bits are packed into a uint16.
   --
   procedure set_polarity(self : MCP23017_record; dir : uint16;
                     error : out err_code) is
   begin
      self.hw.writem2(self.address, IPOL, dir, error);
   end;
   --
   function get_polarity(self : MCP23017_record;
                     error : out err_code) return uint16 is
   begin
      return self.hw.readm2(self.address, IPOL, error);
   end;
   --
   --  Sets the output bits.  Bits are packed into a uint16.
   --
   procedure set_data(self : MCP23017_record; data : uint16;
                      error : out err_code) is
   begin
      self.hw.writem2(self.address, OLAT, data, error);
   end;
   --
   --  Read the port.  Bits are packed into a uint16.
   --
   function get_data(self : MCP23017_record; error : out err_code)
                      return uint16 is
   begin
      return self.hw.readm2(self.address, GPIO, error);
   end;

end;
