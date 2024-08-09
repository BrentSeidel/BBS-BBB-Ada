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
with Ada.IO_Exceptions;
with BBS.units;
use type BBS.units.emf_v;
--
-- This is a high level object-oriented interface to some of the AIN pins on
-- the BeagleBone Black.  This uses the control files in the /sys directory
-- tree.  Since it operates through the file system, it is not particularly high
-- performance, but should be adequate for running relays or indicators.  If you
-- need higher performance, it should be possible to access the device registers
-- directly through /dev/kmem.  It should be possible to change the underlaying
-- implementation here without impacting code that uses this package.
--
package BBS.embed.AIN.linux is
--
-- The analog related pins for the BeagleBone Black are:
-- AIN0 - P9_39
-- AIN1 - P9_40
-- AIN2 - P9_37
-- AIN3 - P9_38
-- AIN4 - P9_33
-- AIN5 - P9_36
-- AIN6 - P9_35
-- Vdd_ADC - P9_32
-- Gnd_ADC - P9_34
--
-- The processor datasheet indicates that there are actually 8 analog inputs, but
-- perhaps one of them may not be brought out to a processor pin.  The analog inputs
-- appear to be part of a touch screen system, but accessing those functions may
-- require writing a specific linux device driver.
--
-- There are no pin control files for these pins - they are dedicated to analog
-- input.
--
   type Linux_AIN_record is new AIN_record with private;
   max_volts : constant BBS.units.emf_v := 1.8;
   --
   -- Create a new AIN object
   --
   function ain_new return AIN;
   --
   -- Configure a new AIN object.  Port should be one of the AIN constants from
   -- the BBS.BBB.pins package.
   --
   procedure configure(self : in out Linux_AIN_record;
                       port : string);
   --
   -- Read the value of an input AIN.  The ADC has 12 bits of resolution, so the
   -- returned value would be in the range 0-4095.
   --
   overriding
   function get(self : Linux_AIN_record) return uint12;
   --
   -- Read the value of an input AIN in volts.  The maximum value is 1.8 so the
   -- result is equal to 1.8*(reading in uint12)/uint12'last;
   --
   function get(self : Linux_AIN_record) return BBS.units.emf_v;
   --
   -- Close the file for the pin.  Once this is called, the AIN object will
   -- need to be re-configured.
   --
   procedure close(self : in out Linux_AIN_record);

private
   package Char_IO is new Ada.Direct_IO(Character);

   type Linux_AIN_record is new AIN_record with
      record
         AIN_file : Char_IO.File_Type;
      end record;
end;
