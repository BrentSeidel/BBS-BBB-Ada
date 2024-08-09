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
package BBS.embed.AIN is
   pragma Preelaborate;
   --
   type AIN_record is abstract tagged limited null record;
   type AIN is access AIN_record'Class;
   --
   -- Read the value of an input AIN.  The ADC has 12 bits of resolution, so the
   -- returned value would be in the range 0-4095.
   --
   function get(self : AIN_record) return uint12 is abstract;

end;
