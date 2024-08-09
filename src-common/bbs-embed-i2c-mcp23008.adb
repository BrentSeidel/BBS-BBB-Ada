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
package body BBS.embed.i2c.MCP23008 is
   --
   --
   -- Object oriented interface
   --
   procedure configure(self : in out MCB23008_record; port : i2c_interface;
                       addr : addr7; error : out err_code) is
      pragma Unreferenced(error);
   begin
      self.hw := port;
      self.address := addr;
   end;
   --
   procedure set_dir(self : MCB23008_record; dir : uint8;
                     error : out err_code) is
   begin
      self.hw.write(self.address, IODIR, dir, error);
   end;
   --
   procedure set_data(self : MCB23008_record; data : uint8;
                      error : out err_code) is
   begin
      self.hw.write(self.address, GPIO, data, error);
   end;
   --
   function read_data(self : MCB23008_record; error : out err_code)
                      return uint8 is
   begin
      return self.hw.read(self.address, GPIO, error);
   end;

end;
