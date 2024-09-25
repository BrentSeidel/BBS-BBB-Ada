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
--
--  This package contains useful items for programming the embedded systems in
--  Ada.  The intention is to include I2C, SPI, and GPIO interfaces.
--
with Ada.Unchecked_Conversion;
package BBS.embed is
   pragma Pure;
   author : constant String := "Brent Seidel";
   version : constant String := "V01.01";

   --
   --  Define some types for use here. (types for 8, 16, 32, and 64 bits
   --  are defined in the root package (BBS)).  Add more here as needed.
   --
   type uint2 is mod 2**2
      with size => 2;
   type uint5 is mod 2**5
      with size => 5;
   type addr7 is mod 2**7  --  7 bit I2C address
     with size => 7;
   type int12 is range -(2**11) .. 2**11 - 1
     with size => 12;
   type uint12 is mod 2**12
     with size => 12;
   type uint14 is mod 2**14
      with size => 14;
   --
   --  Unchecked conversions to convert unsigned into signed values.  Others
   --  may be added as needed.
   --
   function uint12_to_int12 is
     new Ada.Unchecked_Conversion(source => uint12, target => int12);
   --
   --  Get the high and low bytes (uint8) of a 16 bit uint
   --
   function highByte(x : uint16) return uint8 is
     (uint8(x / 2**8));
   function lowByte(x : uint16) return uint8 is
      (uint8(x and 16#FF#));
end;
