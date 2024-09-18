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
--  Package body for driving the MCP4725 DAC.
--
with Ada.Unchecked_Conversion;
with BBS.embed.log;
package body BBS.embed.i2c.MCP4725 is
   --
   --  Unchecked conversions for configuration
   --
   function cmd_to_uint8 is new Ada.Unchecked_Conversion
      (source => cmd_byte, Target => uint8);
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
     --  Fast write in normal mode.  This just sends two bytes to the
     --  DAC.  The write register command is used for this with the
     --  register address being the high byte and the data being the
     --  low byte.
     --
     self.hw.write(self.address, uint8((value/16#100#) and 16#0F#),
                   uint8(value and 16#FF#), err);
   end;
   --
   --
   --  General set command.  Use the defined constants for the command
   --  and mode.  Other values may cause unexpected behavior.
   --
   procedure set(self : in out MCP4725_record; cmd : CMD_type; mode : Mode_type;
                 value : uint12; err : out err_code) is
     msb : cmd_byte;
   begin
     if cmd = Fast_Write then
       --
       --  Fast write command.  This just sends two bytes to the  DAC
       --  The write register command is used for this with the register
       --  address being the high byte and the data being the low byte.
       --
       self.hw.write(self.address, uint8((value/16#100#) and 16#0F#),
                     uint8(value and 16#FF#), err);
     else
       msb.mode := mode;
       msb.cmd := cmd;
       msb.unused0 := False;
       msb.unused3 := False;
       msb.unused4 := False;
       --
       --  Other commands.  This sends three bytes to the DAC.  The writem1
       --  command is used for this.  The register address is the first
       --  byte and the value is the two following bytes.
       --
       self.hw.writem1(self.address, cmd_to_uint8(msb), uint16(value)*16#10#, err);
     end if;
   end;
end;
