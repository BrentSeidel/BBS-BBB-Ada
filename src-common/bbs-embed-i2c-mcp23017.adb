--
--  Author: Brent Seidel
--  Date: 9-Aug-2024
--
--  This file is part of bbs_embed.
--  Bbs_embed is free software: you can redistribute it and/or modify it
--  under the terms of the GNU General Public License as published by the
--  Free Software Foundation, either version 3 of the License, or (at your
--  option) any later version.
--
--  bbs_embed is distributed in the hope that it will be useful, but
--  WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
--  Public License for more details.
--
--  You should have received a copy of the GNU General Public License along
--  with bbs_embed. If not, see <https://www.gnu.org/licenses/>.--
--
with BBS.embed.i2c.devices;
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
   --  Check to see if the configured device is present.
   --
   function present(port : i2c_interface;
                    addr : addr7) return boolean is
      err  : err_code;
      temp : uint8;
      pragma unreferenced (temp);  --  Needed for the read, but value is ignored
   begin
      --
      --  First check to see if address is in range, then check if a device
      --  responds at that address.
      --
      if (addr < BBS.embed.i2c.devices.addr_MCP23008_1) or
         (addr > BBS.embed.i2c.devices.addr_MCP23008_8) then
         return False;
      end if;
      temp := port.read(addr, IOCON, err);
      return err = NONE;
   end;
   --
   -- Set the direction (read(0)/write(1)) for each of the output bits.  The
   -- direction bits are packed into a uint16.
   --
   procedure set_dir(self : MCP23017_record; dir : uint16;
                     error : out err_code) is
   begin
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
      self.hw.b(0) := uint8(dir and 16#ff#);
      self.hw.b(1) := uint8(dir / 16#100#);
      self.hw.write(self.address, GPPU, buff_index(2), error);
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
      self.hw.b(0) := uint8(dir and 16#ff#);
      self.hw.b(1) := uint8(dir / 16#100#);
      self.hw.write(self.address, IPOL, buff_index(2), error);
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
      self.hw.b(0) := uint8(data and 16#ff#);
      self.hw.b(1) := uint8(data / 16#100#);
      self.hw.write(self.address, GPIO, buff_index(2), error);
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
