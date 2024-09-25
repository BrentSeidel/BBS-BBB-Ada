--
--  Author: Brent Seidel
--  Date: 24-Sep-2024
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
with Ada.Direct_IO;
with BBS.embed.GPIO;
--
package BBS.embed.GPIO.Linux is
   --
   type direction is (input, output);
   type Linux_GPIO_record is new GPIO_record with private;
   --
   --  *****************************************************************
   --  ***  The following routines will probably need to be updated
   --  ***  to work with the new GPIO system.  There may still be files
   --  ***  involved, but it looks like there will also be addresses to
   --  ***  structures.  These should be able to be treated as opaque
   --  ***  objects by the Ada software.
   --  *****************************************************************
   --
   --  Configure a new GPIO object.  The pin control file and GPIO directory
   --  must correspond, otherwise things will not work correctly.  Pin should
   --  be one of the pin constants and port should be one of
   --  the gpio constants from the device specific pins packages.
   --
   procedure configure(self : in out Linux_GPIO_record;
                       pin : string; port : string; dir : direction);
   --
   --  Not all GPIOs have an associated pin control file.  Some pins are dedicated
   --  to GPIO and have no other function.
   --
   procedure configure(self : in out Linux_GPIO_record;
                       port : string; dir : direction);
   --
   --  Set the direction of a pin.  This can be used whether a GPIO pin
   --  has been configured or not.  It is also used by the configure
   --  procedure.
   --
   procedure set_dir(self : in out Linux_GPIO_record;
                     port : String; dir : direction);
   --
   --  Set the value of an output GPIO.
   --
   overriding
   procedure set(self : Linux_GPIO_record; value : bit);
   --
   --  Read the value of an input GPIO.
   --
   overriding
   function get(self : Linux_GPIO_record) return bit;
   --
   --  Close the file for the pin.  Once this is called, the GPIO object will
   --  need to be re-configured.
   --
   procedure close(self : in out Linux_GPIO_record);
   --
private
   --
   type Linux_GPIO_record is new GPIO_record with record
      dir : direction;
   end record;
end BBS.embed.GPIO.Linux;
