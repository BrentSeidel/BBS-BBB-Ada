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
package BBS.embed.SPI is

   --
   -- The SPI  object
   --
   type SPI_record is abstract tagged limited null record;
   type SPI_ptr is access all SPI_record'Class;
   --
   -- Configure the SPI interface on a BeagleBone Black or other systems that
   -- have multiple functions on the SPI pins.  This configureation procedure
   -- sets the pins to the SPI function.
   --
--   procedure configure(self : in out SPI_record; SPI_file : string;
--                       SCL : string; SDA : string) is abstract;
   --
   -- Configure the SPI interface on a Raspberry PI or other systems that have
   -- dedicated pins for the SPI interface.  This would also work on a system
   -- with shared pins if the pins had already been set to the SPI function.
   --
--   procedure configure(self : in out SPI_record; SPI_file : string) is abstract;
   --
   -- Write a value to the SPI
   --
   procedure set(self : SPI_record; value : uint8) is abstract;
   --
   -- Read a value from the SPI
   --
   function get(self : SPI_record) return uint8 is abstract;

end;
