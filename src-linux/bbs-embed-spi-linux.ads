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
with Ada.Text_IO;
with Interfaces.C;
use type Interfaces.C.unsigned_long;
with BBS.embed.Linux;
with BBS.embed.SPI;

package BBS.embed.SPI.Linux is

   --
   -- The SPI  object
   --
   type Linux_SPI_record is new BBS.embed.SPI.SPI_record with private;
   --
   function SPI_new return SPI_ptr;
   --
   -- Configure the SPI interface on a BeagleBone Black or other systems that
   -- have multiple functions on the SPI pins.  This configuration procedure
   -- sets the pins to the SPI function.
   --
   procedure configure(self : in out Linux_SPI_record; SPI_file : string;
                       SCL : string; SDA : string);
   --
   -- Configure the SPI interface on a Raspberry PI or other systems that have
   -- dedicated pins for the SPI interface.  This would also work on a system
   -- with shared pins if the pins had already been set to the SPI function.
   --
   procedure configure(self : in out Linux_SPI_record; SPI_file : string);
   --
   -- Write a value to the SPI
   --
   overriding
   procedure set(self : Linux_SPI_record; value : uint8);
   --
   -- Read a value from the SPI
   --
   overriding
   function get(self : Linux_SPI_record) return uint8;
   --
   --  Close the SPI interface when done.  Once this is called, the
   --  SPI object will need to be re-configured.
   --
   procedure close(self : in out Linux_SPI_record);
private
   --
   -- Some of the more advanced features of the SPI port require the use of
   -- IOCTL calls.  So, to be prepared, the use of C/Unix files is needed.  This
   -- is similar to the approach taken in the BBS.embed.i2c package.
   --
   -- First, declare some bindings to the C library.
   --
   -- Since the basic C file and ioctl calls use a file descriptor, define a
   -- type for it and declare bindings for the C open, read, and write functions.
   --
   type size_t is new long_integer;
   subtype ssize_t is size_t;
   --
   --
   function C_read(file : BBS.embed.Linux.file_id; buff : in out uint8; length : size_t) return ssize_t;
   pragma import(C, C_read, "read");
   --
   function C_write(file : BBS.embed.Linux.file_id; buff : in out uint8; length : size_t) return ssize_t;
   pragma import(C, C_write, "write");
   --
   type Linux_SPI_record is new SPI_record with record
      port   : BBS.embed.Linux.file_id;
   end record;

end;
