with Ada.Text_IO;
with Ada.Direct_IO;
with Ada.IO_Exceptions;
--
-- This is a high level object-oriented interface to some of the AIN pins on
-- the BeagleBone Black.  This uses the control files in the /sys directory
-- tree.  Since it operates through the file system, it is not particularly high
-- performance, but should be adequate for running relays or indicators.  If you
-- need higher performance, it should be possible to access the device registers
-- directly through /dev/kmem.  It should be possible to change the underlaying
-- implementation here without impacting code that uses this package.
--
package BBS.BBB.AIN is
--
-- The analog related pins are:
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
   type AIN_record is tagged limited private;
   type AIN is access AIN_record;
   type volts is new float;
   max_volts : constant volts := 1.8;
   --
   -- Create a new AIN object
   --
   function ain_new return AIN;
   --
   -- Configure a new AIN object.  Port should be one of the AIN constants from
   -- the BBS.BBB.pins package.
   --
   procedure configure(self : not null access AIN_record'class;
                       port : string);
   --
   -- Read the value of an input AIN.  The ADC has 12 bits of resolution, so the
   -- returned value would be in the range 0-4095.
   --
   function get(self : not null access AIN_record'class) return uint12;
   --
   -- Read the value of an input AIN in volts.  The maximum value is 1.8 so the
   -- result is equal to 1.8*(reading in uint12)/uint12'last;
   --
   function get(self : not null access AIN_record'class) return volts;
   --
   -- Close the file for the pin.  Once this is called, the AIN object will
   -- need to be re-configured.
   --
   procedure close(self : not null access AIN_record'class);

private
   package Char_IO is new Ada.Direct_IO(Character);
   type AIN_record is tagged limited
      record
         AIN_file : Char_IO.File_Type;
      end record;
end;
