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
--
--  Package for driving the MCP4725 DAC.
--
--  On a Rasperry PI 400 under Ubuntu, this could generate a square wave
--  at a little over 800Hz.
--
package BBS.embed.i2c.MCP4725 is
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
   --  Note that the Fast Write command only requires two bytes to be sent
   --  to the DAC.  The other commands require three bytes.
   --
   type CMD_type is (Fast_Write, Write_CMD, Write_EEPROM);
   for CMD_type use (Fast_Write => 0, Write_CMD => 2, Write_EEPROM => 3);
   for CMD_type'Size use 3;
--   type CMD_type is new Integer range 0 .. 7;
--   Fast_Write   : constant CMD_type := 0;
--   Write_CMD    : constant CMD_type := 2;
--   Write_EEPROM : constant CMD_type := 3;
   --
   --  Power-Down select
   --  0 - Normal Mode
   --  1 - 1k resistance to ground
   --  2 - 100k resistance to ground
   --  3 - 500k resistance to ground
   --
   type Mode_type is (PD_Normal, PD_1k, PD_100k, PD_500k);
   for Mode_type use (PD_Normal => 0, PD_1k => 1, PD_100k => 2, PD_500k => 3);
   for Mode_type'Size use 2;
--   type Mode_type is new Integer range 0 .. 3;
--   PD_Normal : constant Mode_type := 0;
--   PD_1k     : constant Mode_type := 1;
--   PD_100k   : constant Mode_type := 2;
--   PD_500k   : constant Mode_type := 3;
   --
   --  Command byte
   --
   type cmd_byte is record
      unused0 : Boolean := False;
      mode    : Mode_type;
      unused3 : Boolean := False;
      unused4 : Boolean := False;
      cmd     : CMD_type;
   end record;
   for cmd_byte use record
      unused0 at 0 range 0 .. 0;
      mode    at 0 range 1 .. 2;
      unused3 at 0 range 3 .. 3;
      unused4 at 0 range 4 .. 4;
      cmd     at 0 range 5 .. 7;
   end record;
   for cmd_byte'Size use 8;
   --
   -- Define object.
   --
   type MCP4725_record is new i2c_device_record with private;
   type MCP4725_ptr is access MCP4725_record;
   --
   procedure configure(self : in out MCP4725_record; port : i2c_interface;
                       addr : addr7; error : out err_code);
   --
   --  The DAC is only single channel, so this just sets the value using
   --  fast write and PD normal mode.
   --
   procedure set(self : in out MCP4725_record; value : uint12; err : out err_code);
   --
   --  General set command.  Use the defined constants for the command
   --  and mode.  Other values may cause unexpected behavior.
   --
   procedure set(self : in out MCP4725_record; cmd : CMD_type; mode : Mode_type;
                 value : uint12; err : out err_code);
   --
private
   --
   type MCP4725_record is new i2c_device_record with record
      null;
   end record;
end;
