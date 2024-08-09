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
with BBS.embed.SPI;

package BBS.embed.SPI.Due is

   --
   -- The SPI  object
   --
   type Due_SPI_record is new BBS.embed.SPI.SPI_record with private;
   --
   --  Configure the SPI interface.  The SAM3X8E core actually has two SPI
   --  interfaces, but SPI1 does not seem to be present on the chip pinout.
   --
   procedure configure(self : in out Due_SPI_record);
   --
   -- Write a value to the SPI
   --
   overriding
   procedure set(self : Due_SPI_record; value : uint8);
   --
   -- Read a value from the SPI
   --
   overriding
   function get(self : Due_SPI_record) return uint8;
private
   --
   type Due_SPI_record is new SPI_record with
      record
         port   : Integer;
      end record;

end BBS.embed.SPI.Due;
