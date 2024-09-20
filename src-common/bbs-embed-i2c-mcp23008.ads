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
package BBS.embed.i2c.MCP23008 is
   pragma Preelaborate;
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
   -- Note that only the basic functions are covered.  The main purpose for this
   -- is to support its use as the LCD backpack controller.  If you need additional
   -- features, it should be easy enough to add them.
   --
   type MCB23008_record is new i2c_device_record with private;
   type MCB23008_ptr is access MCB23008_record;
   --
   -- The configure procedure needs to be called first to initialize the
   -- calibration constants from the device.
   --
   procedure configure(self : in out MCB23008_record; port : i2c_interface;
                       addr : addr7; error : out err_code);
   --
   --  Check to see if the configured device is present.
   --
   function present(port : i2c_interface;
                    addr : addr7) return boolean;
   --
   -- Set the direction (read(0)/write(1)) for each of the output bits.  The
   -- direction bits are packed into a uint8.
   --
   procedure set_dir(self : MCB23008_record; dir : uint8;
                     error : out err_code);
   --
   -- Sets the output bits.  Bits are packed into a uint8.
   --
   procedure set_data(self : MCB23008_record; data : uint8;
                      error : out err_code);
   --
   -- Read the port.  Bits are packed into a uint8.
   --
   function read_data(self : MCB23008_record; error : out err_code)
                      return uint8;
private
   --
   IODIR   : constant uint8 := 16#00#; -- I/O Direction
   IPOL    : constant uint8 := 16#01#; -- Input polarity
   GPINTEN : constant uint8 := 16#02#; -- Interrupt-on-change control
   DEFVAL  : constant uint8 := 16#03#; --Default compare
   INTCON  : constant uint8 := 16#04#; -- Interrupt control
   IOCON   : constant uint8 := 16#05#; --Configuration
   GPPU    : constant uint8 := 16#06#; -- Pull-up resistor configuration
   INTF    : constant uint8 := 16#07#; -- Interrupt flag
   INTCAP  : constant uint8 := 16#08#; -- Interrupt capture
   GPIO    : constant uint8 := 16#09#; -- Port register
   OLAT    : constant uint8 := 16#0a#; -- Output latch
   --
   type MCB23008_record is new i2c_device_record with record
      null;
   end record;

end;
