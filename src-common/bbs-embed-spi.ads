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
   -- Write a byte to the SPI
   --
   procedure set(self : SPI_record; value : uint8) is abstract;
   --
   --  Write a buffer to the SPI
   --
   procedure set(self : SPI_record; value : buffer; size : buff_index) is abstract;
   --
   -- Read a byte from the SPI
   --
   function get(self : SPI_record) return uint8 is abstract;
   --
   --  Read a buffer from the SPI
   --
   procedure get(self : SPI_record; value : out buffer; size : buff_index) is abstract;
   --
end;
