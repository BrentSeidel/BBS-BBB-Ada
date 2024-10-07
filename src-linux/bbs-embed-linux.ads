--
--  Author: Brent Seidel
--  Date: 4-Oct-2024
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
with Ada.Direct_IO;
with Ada.Strings.Fixed;
with Ada.Text_IO;
with Ada.Unchecked_Conversion;
with BBS.embed.GPIO;
with Interfaces.C;
use type Interfaces.C.Int;
--
--  This package is intended to consolidate the Linux constants and
--  function definitions used in multiple other packages.  This was
--  generally in the private sections of other packages such as I2C,
--  GPIO, and SPI.  Collecting it here will reduce duplication and help
--  improve consistency.  Definitions specific to a particular package
--  will remain in that package.
--
package BBS.embed.Linux is
   --
   --  File ID returned from a Linux open() or creat() system call.
   --
   type file_id is new interfaces.C.int;
   --
   -- File flags for opening a file.
   --
   type file_flg is new Interfaces.C.Int;
   O_RDONLY : file_flg := 0;
   O_WRONLY : file_flg := 1;
   O_RDWR   : file_flg := 2;
   --
   type mode_t is new Interfaces.C.Int;
   --
   type cbuff_ptr is new buff_ptr;
   pragma Convention(C, cbuff_ptr);
   --
   --  Declare C file functions.
   --
   function C_open(name : string; flags : file_flg; mode : mode_t := 8#666#)
      return file_id;
   pragma import(C, C_open, "open");
   --
   function C_close(file : file_id) return integer;
   pragma import(C, C_close, "close");
   --
   -- The range is used on size_t so that it is forced to be within the size of
   -- buffer.
   --
   type size_t is new long_integer
      range long_integer(buffer'First) .. long_integer(buffer'Last);
   subtype ssize_t is size_t;
   --
   function C_read(file : BBS.embed.Linux.file_id; buff : in out buffer; length : size_t) return ssize_t;
   pragma import(C, C_read, "read");
   --
   function C_write(file : BBS.embed.Linux.file_id; buff : in out buffer; length : size_t) return ssize_t;
   pragma import(C, C_write, "write");
   --
   -- Now some C functions for getting errno and error messages
   --
   type err_msg is new string(1 .. 255);
   type err_msg_ptr is access err_msg;
   --
   function get_errno return integer;
   pragma Import(C, get_errno, "get_errno");
   --
   procedure reset_errno;
   pragma Import(C, reset_errno, "reset_errno");
   --
   procedure perror(msg : string);
   pragma Import(C, perror, "perror");
   --
   function strerror(err_num : integer) return err_msg_ptr;
   pragma Import(C, strerror, "strerror");
   --
   function cvt_cstr_adastr(str_ptr : err_msg_ptr) return string;

end BBS.embed.Linux;
