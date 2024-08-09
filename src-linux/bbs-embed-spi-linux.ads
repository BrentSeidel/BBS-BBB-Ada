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
   -- have multiple functions on the SPI pins.  This configureation procedure
   -- sets the pins to the SPI function.
   --
--   overriding
   procedure configure(self : in out Linux_SPI_record; SPI_file : string;
                       SCL : string; SDA : string);
   --
   -- Configure the SPI interface on a Raspberry PI or other systems that have
   -- dedicated pins for the SPI interface.  This would also work on a system
   -- with shared pins if the pins had already been set to the SPI function.
   --
--   overriding
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
private
   --
   -- Some of the more advanced features of the SPI port require the use of
   -- IOCTL calls.  So, to be prepared, the use of C/Unix files is needed.  This
   -- is similar to the approach taken in the BBS.embed.i2c package.
   --
   --
   -- First, declare some bindings to the C library.
   --
   -- Since the basic C file and ioctl calls use a file descriptor, define a
   -- type for it and declare bindings for the C open, read, and write functions.
   --
   type file_id is new interfaces.C.int;
   type mode_t is new integer;
   type size_t is new long_integer;
   subtype ssize_t is size_t;
   --
   -- File flags for opening a file read/write.  This is the only one used here
   -- so don't bother to define others.
   --
   O_RDWR : integer := 16#02#;
   --
   function C_open(name : string; flags : integer; mode : mode_t := 8#666#) return file_id;
   pragma import(C, C_open, "open");
   --
   function C_close(file : file_id) return integer;
   pragma import(C, C_close, "close");
   --
   function C_read(file : file_id; buff : in out uint8; length : size_t) return ssize_t;
   pragma import(C, C_read, "read");
   --
   function C_write(file : file_id; buff : in out uint8; length : size_t) return ssize_t;
   pragma import(C, C_write, "write");
   --
   -- Now some C functions for getting errno and error messages
   --
   function get_errno return integer;
   pragma Import(C, get_errno, "get_errno");
   --
   procedure reset_errno;
   pragma Import(C, reset_errno, "reset_errno");
   --
   type err_msg is new string(1 .. 255);
   type err_msg_ptr is access err_msg;
   --
   procedure perror(msg : string);
   pragma Import(C, perror, "perror");
   --
   function strerror(err_num : integer) return err_msg_ptr;
   pragma Import(C, strerror, "strerror");
   --
   type Linux_SPI_record is new SPI_record with
      record
         port   : file_id;
      end record;

end;
