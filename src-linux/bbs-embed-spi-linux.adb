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
   -- Write a byte to the SPI
   --
   procedure set(self : Linux_SPI_record; value : uint8) is
      temp : BBS.embed.Linux.size_t;
   begin
      buff(0) := value;
      temp := BBS.embed.Linux.C_write(self.port, buff, 1);
   end;
   --
   --  Write a buffer to the SPI
   --
   procedure set(self : Linux_SPI_record; value : buffer; size : buff_index) is
      temp : BBS.embed.Linux.size_t;
      buff : buffer := value;
   begin
      temp := BBS.embed.Linux.C_write(self.port, buff, BBS.embed.Linux.size_t(size));
   end;
   --
   -- Read a byte from the SPI
   --
   function get(self : Linux_SPI_record) return uint8 is
      temp : BBS.embed.Linux.ssize_t;
   begin
      temp := BBS.embed.Linux.C_read(self.port, buff, 1);
      return buff(0);
   end;
   --
   --  Read a buffer from the SPI
   --
   procedure get(self : Linux_SPI_record; value : out buffer; size : buff_index) is
      temp : BBS.embed.Linux.ssize_t;
   begin
      temp := BBS.embed.Linux.C_read(self.port, value, BBS.embed.Linux.size_t(size));
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
   --  Print the SPI configuration
   --
   procedure print_config(self : in out Linux_SPI_record) is
      temp : Interfaces.C.Int;
      byte : uint8  := 0;
      word : uint32 := 0;
   begin
      temp := mode_ioctl(self.port, SPI_IOC_RD_MODE, byte);
      if temp = -1 then
         Ada.Text_IO.Put_Line("SPI Get mode ioctl() failed");
      end if;
      Ada.Text_IO.Put_Line("SPI port mode is " & uint8'Image(byte));
      temp := lsb_ioctl(self.port, SPI_IOC_RD_LSB_FIRST, byte);
      if temp = -1 then
         Ada.Text_IO.Put_Line("SPI Get LSB ioctl() failed");
      end if;
      Ada.Text_IO.Put_Line("SPI port LSB mode is " & uint8'Image(byte));
      temp := bits_ioctl(self.port, SPI_IOC_RD_BITS_PER_WORD, byte);
      if temp = -1 then
         Ada.Text_IO.Put_Line("SPI Get bits ioctl() failed");
      end if;
      Ada.Text_IO.Put_Line("SPI port bits per word is " & uint8'Image(byte));
      temp := speed_ioctl(self.port, SPI_IOC_RD_MAX_SPEED_HZ, word);
      if temp = -1 then
         Ada.Text_IO.Put_Line("SPI Get speed ioctl() failed");
      end if;
      Ada.Text_IO.Put_Line("SPI port bits per second is " & uint32'Image(word));
      temp := mode32_ioctl(self.port, SPI_IOC_RD_MODE32, word);
      if temp = -1 then
         Ada.Text_IO.Put_Line("SPI Get mode 32 ioctl() failed");
      end if;
      Ada.Text_IO.Put_Line("SPI port mode 32 is " & uint32'Image(word));
   end;
   --
end;
