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
package body BBS.embed.SPI.Linux is
   --
   function SPI_new return SPI_ptr is
   begin
      return new Linux_SPI_record;
   end;
   --
   -- Configure the SPI interface on a BeagleBone Black or other systems that
   -- have multiple functions on the SPI pins.  This configureation procedure
   -- sets the pins to the SPI function.
   --
   procedure configure(self : in out Linux_SPI_record; SPI_file : string;
                       SCL : string; SDA : string) is
   begin
      null;
   end;
   --
   -- Configure the SPI interface on a Raspberry PI or other systems that have
   -- dedicated pins for the SPI interface.  This would also work on a system
   -- with shared pins if the pins had already been set to the SPI function.
   --
   procedure configure(self : in out Linux_SPI_record; SPI_file : string) is
   begin
      self.port := BBS.embed.Linux.C_open(SPI_file, BBS.embed.Linux.O_RDWR, 8#666#);
      if self.port = -1 then
         raise spi_fault with "SPI file open failed.";
      end if;
   end;
   --
   procedure set(self : Linux_SPI_record; value : uint8) is
      temp : BBS.embed.Linux.size_t;
   begin
      buff(0) := value;
      temp := BBS.embed.Linux.C_write(self.port, buff, 1);
   end;
   --
   function get(self : Linux_SPI_record) return uint8 is
      dummy2 : BBS.embed.Linux.ssize_t;
   begin
      dummy2 := BBS.embed.Linux.C_read(self.port, buff, 1);
      return buff(0);
   end;
   --
   --  Close the I2C interface when done.  Once this is called, the
   --  I2C object will need to be re-configured.
   --
   procedure close(self : in out Linux_SPI_record) is
      temp : Integer;
      pragma unreferenced(temp);
   begin
      temp := BBS.embed.Linux.c_close(self.port);
   end;
   --
end;
