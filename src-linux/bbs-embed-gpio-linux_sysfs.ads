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
with Ada.Direct_IO;
with BBS.embed.GPIO;
--
-- This is a high level object-oriented interface to some of the GPIO pins.
--
package BBS.embed.GPIO.Linux_sysfs is

   type direction is (input, output);
   type Linux_GPIO_record is new GPIO_record with private;
   --
   --  Create a new GPIO object
   --
   function gpio_new return GPIO;
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

private
   package Char_IO is new Ada.Direct_IO(Character);
   --
   type Linux_GPIO_record is new GPIO_record with
      record
         gpio_file : Char_IO.File_Type;
         dir : direction;
      end record;
end BBS.embed.GPIO.Linux_sysfs;
