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
package BBS.embed.GPIO is
   pragma Preelaborate;
   --
   type GPIO_record is abstract tagged limited null record;
   type GPIO is access all GPIO_record'Class;
   --
   -- Set the value of an output GPIO.
   --
   procedure set(self : GPIO_record; value : bit) is abstract;
   --
   -- Read the value of an input GPIO.
   --
   function get(self : GPIO_record) return bit is abstract;
end;