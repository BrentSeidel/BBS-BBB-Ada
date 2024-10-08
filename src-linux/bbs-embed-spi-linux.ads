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
use type Interfaces.C.Int;
use type Interfaces.C.unsigned_long;
with BBS.embed.Linux;
use type BBS.embed.Linux.File_ID;
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
   -- Write a byte to the SPI
   --
   overriding
   procedure set(self : Linux_SPI_record; value : uint8);
   --
   --  Write a buffer to the SPI
   --
   overriding
   procedure set(self : Linux_SPI_record; value : buffer; size : buff_index);
   --
   -- Read a byte from the SPI
   --
   overriding
   function get(self : Linux_SPI_record) return uint8;
   --
   --  Read a buffer from the SPI
   --
   overriding
   procedure get(self : Linux_SPI_record; value : out buffer; size : buff_index);
   --
   --  Close the SPI interface when done.  Once this is called, the
   --  SPI object will need to be re-configured.
   --
   procedure close(self : in out Linux_SPI_record);
   --
   --  Print the SPI configuration.  This is mainly intended for debugging
   --  and development purposes.
   --
   procedure print_config(self : in out Linux_SPI_record);
   --
private
   --
   --  SPI exception raised for errors calling Linux.
   --
   spi_fault : Exception;
   --
   --  Stuff for SPI ioctl() calls
   --
   --  #define _IOC(dir,type,nr,size) \
   --    (((dir)  << _IOC_DIRSHIFT) | \
   --    ((type) << _IOC_TYPESHIFT) | \
   --    ((nr)   << _IOC_NRSHIFT) | \
   --    ((size) << _IOC_SIZESHIFT))
   --
   --  #define _IOC_TYPECHECK(t) (sizeof(t))
   --
   --  Used to create ioctl command numbers.
   --
   --  NOTE: _IOW means userland is writing and kernel is reading. _IOR
   --  means userland is reading and kernel is writing.
   --
   --  #define _IO(type,nr)            _IOC(_IOC_NONE,(type),(nr),0)
   --  #define _IOR(type,nr,size)      _IOC(_IOC_READ,(type),(nr),(_IOC_TYPECHECK(size)))
   --  #define _IOW(type,nr,size)      _IOC(_IOC_WRITE,(type),(nr),(_IOC_TYPECHECK(size)))
   --  #define _IOWR(type,nr,size)     _IOC(_IOC_READ|_IOC_WRITE,(type),(nr),(_IOC_TYPECHECK(size)))
   --  #define _IOR_BAD(type,nr,size)  _IOC(_IOC_READ,(type),(nr),sizeof(size))
   --  #define _IOW_BAD(type,nr,size)  _IOC(_IOC_WRITE,(type),(nr),sizeof(size))
   --  #define _IOWR_BAD(type,nr,size) _IOC(_IOC_READ|_IOC_WRITE,(type),(nr),sizeof(size))
   --
   --  These types and record replace a bunch of C macros (some of which
   --  are listed above) for breaking the 32 bit IOCTL number into fields.
   --
   type dir_type is (none, write, read, rw);
   for dir_type use (none => 0, write => 1, read => 2, rw => 3);
   for dir_type'Size use 2;
   --
   subtype nr_type   is uint8;
   subtype io_type   is uint8;
   subtype size_type is uint14;
   --
   --  Structure for decoding IOCTL numbers.
   --
   type ioctl_type is record
      nr   : nr_type;     --   8 bits
      code : io_type;     --   8 bits (Linux calls this type)
      size : size_type;   --  14 bits
      dir  : dir_type;    --   2 bits
   end record;
   for ioctl_type use record
      nr   at 0 range  0 ..  7;
      code at 0 range  8 .. 15;
      size at 0 range 16 .. 29;
      dir  at 0 range 30 .. 31;
   end record;
   for ioctl_type'Size use 32;
   --
   type ioctl_num is new uint32;
   --
   function ioctl_to_num is new Ada.Unchecked_Conversion(source => ioctl_type,
         target => ioctl_num);
   --
   --  SPI ioctl commands
   --
   --  Commands to read and set the SPI mode (0 .. 3).
   --
   SPI_IOC_RD_MODE  : constant ioctl_num := ioctl_to_num((
      dir => read, code => 16#6b#, nr => 1, size => 1));
   SPI_IOC_WD_MODE  : constant ioctl_num := ioctl_to_num((
      dir => write, code => 16#6b#, nr => 1, size => 1));
   --
   --  Commands to read and set the SPI bit order (LSB or MSB first).
   --
   SPI_IOC_RD_LSB_FIRST : constant ioctl_num := ioctl_to_num((
      dir => read, code => 16#6b#, nr => 2, size => 1));
   SPI_IOC_WR_LSB_FIRST : constant ioctl_num := ioctl_to_num((
      dir => write, code => 16#6b#, nr => 2, size => 1));
   --
   --  Commands to read and set the SPI word length (bits per word).
   --
   SPI_IOC_RD_BITS_PER_WORD : constant ioctl_num := ioctl_to_num((
      dir => read, code => 16#6b#, nr => 3, size => 1));
   SPI_IOC_WR_BITS_PER_WORD : constant ioctl_num := ioctl_to_num((
      dir => write, code => 16#6b#, nr => 3, size => 1));
   --
   --  Commands to read and set the SPI default max speed in Hz.
   --
   SPI_IOC_RD_MAX_SPEED_HZ : constant ioctl_num := ioctl_to_num((
      dir => read, code => 16#6b#, nr => 4, size => 4));
   SPI_IOC_WR_MAX_SPEED_HZ : constant ioctl_num := ioctl_to_num((
      dir => write, code => 16#6b#, nr => 4, size => 4));
   --
   --  Commands to read and set the SPI mode field
   --
   SPI_IOC_RD_MODE32 : constant ioctl_num := ioctl_to_num((
      dir => read, code => 16#6b#, nr => 5, size => 4));
   SPI_IOC_WR_MODE32 : constant ioctl_num := ioctl_to_num((
      dir => write, code => 16#6b#, nr => 5, size => 4));
   --
   function mode_ioctl(f_id : BBS.embed.Linux.file_id; cmd : ioctl_num;
      info : in out uint8) return Interfaces.C.Int
      with pre => ((cmd = SPI_IOC_RD_MODE) or (cmd = SPI_IOC_WD_MODE));
   pragma Import(C, mode_ioctl, "ioctl");
   --
   function lsb_ioctl(f_id : BBS.embed.Linux.file_id; cmd : ioctl_num;
      info : in out uint8) return Interfaces.C.Int
      with pre => ((cmd = SPI_IOC_RD_LSB_FIRST) or (cmd = SPI_IOC_WR_LSB_FIRST));
   pragma Import(C, lsb_ioctl, "ioctl");
   --
   function bits_ioctl(f_id : BBS.embed.Linux.file_id; cmd : ioctl_num;
      info : in out uint8) return Interfaces.C.Int
      with pre => ((cmd = SPI_IOC_RD_BITS_PER_WORD) or (cmd = SPI_IOC_WR_BITS_PER_WORD));
   pragma Import(C, bits_ioctl, "ioctl");
   --
   function speed_ioctl(f_id : BBS.embed.Linux.file_id; cmd : ioctl_num;
      info : in out uint32) return Interfaces.C.Int
      with pre => ((cmd = SPI_IOC_RD_MAX_SPEED_HZ) or (cmd = SPI_IOC_WR_MAX_SPEED_HZ));
   pragma Import(C, speed_ioctl, "ioctl");
   --
   function mode32_ioctl(f_id : BBS.embed.Linux.file_id; cmd : ioctl_num;
      info : in out uint32) return Interfaces.C.Int
      with pre => ((cmd = SPI_IOC_RD_MODE32) or (cmd = SPI_IOC_WR_MODE32));
   pragma Import(C, mode32_ioctl, "ioctl");
   --
   type Linux_SPI_record is new SPI_record with record
      port : BBS.embed.Linux.file_id;
   end record;
   --
   --  Buffer for SPI communication
   --
   buff : aliased buffer;
end;
