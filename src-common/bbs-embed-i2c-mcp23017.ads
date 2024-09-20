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
package BBS.embed.i2c.MCP23017 is
   --
   all_write : constant uint8 := 16#00#;
   all_read  : constant uint8 := 16#FF#;
   --
   type MCP23017_record is new i2c_device_record with private;
   type MCP23017_ptr is access MCP23017_record;
   --
   --  The configure procedure needs to be called first to setup the object.
   --
   procedure configure(self : in out MCP23017_record; port : i2c_interface;
                       addr : addr7; error : out err_code);
   --
   --  Check to see if the configured device is present.
   --
   function present(port : i2c_interface;
                    addr : addr7) return boolean;
   --
   -- Set the direction (read(0)/write(1)) for each of the output bits.  The
   -- direction bits are packed into a uint16.
   --
   procedure set_dir(self : MCP23017_record; dir : uint16;
                     error : out err_code);
   function get_dir(self : MCP23017_record;
                     error : out err_code) return uint16;
   --
   -- Set the polarity (normal(0)/inverted(1)) for each of the input bits.  The
   -- direction bits are packed into a uint16.
   --
   procedure set_polarity(self : MCP23017_record; dir : uint16;
                     error : out err_code);
   function get_polarity(self : MCP23017_record;
                     error : out err_code) return uint16;
   --
   --  Enable/Disable weak pullup resistors (disable(0)/enable(1)) for each
   --  of the output bits.  The bits are packed into a uint16.
   --
   procedure set_pullup(self : MCP23017_record; dir : uint16;
                     error : out err_code);
   function get_pullup(self : MCP23017_record;
                     error : out err_code) return uint16;
   --
   --  Sets the output bits.  Bits are packed into a uint16.
   --
   procedure set_data(self : MCP23017_record; data : uint16;
                      error : out err_code);
   --
   --  Read the port.  Bits are packed into a uint16.
   --
   function get_data(self : MCP23017_record; error : out err_code)
                      return uint16;
private
   --
   type MCP23017_record is new i2c_device_record with
      record
         null;
   end record;
   --
   --  Device registers.  The MCP23017 can be configured to work as two separate
   --  8 bit ports or one 16 bit port.  Depending on the setting of the BANK bit
   --  in the IOCON register, the _A and _B registers are used or the register
   --  with no suffix is used.
   --
   --  This application will configure the device to work as a single 16 bit I/O
   --  port.
   --
   IODIR   : constant uint8 := 16#00#; -- I/O Direction
   IODIR_A : constant uint8 := 16#00#; -- I/O Direction
   IODIR_B : constant uint8 := 16#10#; -- I/O Direction
   IPOL    : constant uint8 := 16#02#; -- Input polarity
   IPOL_A  : constant uint8 := 16#01#; -- Input polarity
   IPOL_B  : constant uint8 := 16#11#; -- Input polarity
   GPINTEN : constant uint8 := 16#04#; -- Interrupt-on-change control
   GPINTEN_A : constant uint8 := 16#02#; -- Interrupt-on-change control
   GPINTEN_B : constant uint8 := 16#12#; -- Interrupt-on-change control
   DEFVAL   : constant uint8 := 16#06#; --Default compare
   DEFVAL_A : constant uint8 := 16#03#; --Default compare
   DEFVAL_B : constant uint8 := 16#13#; --Default compare
   INTCON   : constant uint8 := 16#08#; -- Interrupt control
   INTCON_A : constant uint8 := 16#04#; -- Interrupt control
   INTCON_B : constant uint8 := 16#14#; -- Interrupt control
   IOCON   : constant uint8 := 16#0A#; -- Control register
   IOCON_A : constant uint8 := 16#05#; -- Control register
   IOCON_B : constant uint8 := 16#15#; -- Control register
   GPPU   : constant uint8 := 16#0C#; -- Pull-up resistor configuration
   GPPU_A : constant uint8 := 16#06#; -- Pull-up resistor configuration
   GPPU_B : constant uint8 := 16#16#; -- Pull-up resistor configuration
   INTF   : constant uint8 := 16#0E#; -- Interrupt flag
   INTF_A : constant uint8 := 16#07#; -- Interrupt flag
   INTF_B : constant uint8 := 16#17#; -- Interrupt flag
   INTCAP   : constant uint8 := 16#10#; -- Interrupt capture
   INTCAP_A : constant uint8 := 16#08#; -- Interrupt capture
   INTCAP_B : constant uint8 := 16#18#; -- Interrupt capture
   GPIO   : constant uint8 := 16#12#; -- Port register
   GPIO_A : constant uint8 := 16#09#; -- Port register
   GPIO_B : constant uint8 := 16#19#; -- Port register
   OLAT   : constant uint8 := 16#14#; -- Output latch
   OLAT_A : constant uint8 := 16#0A#; -- Output latch
   OLAT_B : constant uint8 := 16#1A#; -- Output latch

end;
