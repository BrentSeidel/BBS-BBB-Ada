with Ada.Text_IO;
with Ada.Direct_IO;
--
-- This is a high level object-oriented interface to some of the GPIO pins on
-- the BeagleBone Black.  This uses the control files in the /sys directory
-- tree.  Since it operates through the file system, it is not particularly high
-- performance, but should be adequate for running relays or indicators.  If you
-- need higher performance, it should be possible to access the device registers
-- directly through /dev/kmem.  It should be possible to change the underlaying
-- implementation here without impacting code that uses this package.
--
package BBS.BBB.GPIO is
--
-- Mapping of GPIO file directories to pins.  Note that not all have an associated
-- pin.  Also many pins have a GPIO which does not have a GPIO directory.  It is
-- unclear how to access them.
--
--   GPIO_2   - P9_22 (multi)
--   GPIO_3   - P9_21 (multi)
--   GPIO_4   - P9_18 (multi)
--   GPIO_5   - P9_17 (multi)
--   GPIO_6   - (apparently no associated pin)
--   GPIO_14  - P9_26 (multi)
--   GPIO_15  - P9_24 (multi)
--   GPIO_22  - P8_19 (multi)
--   GPIO_23  - P8_13 (multi)
--   GPIO_26  - P8_14 (multi)
--   GPIO_27  - P8_17 (multi)
--   GPIO_30  - P9_11 (multi)
--   GPIO_31  - P9_13 (multi)
--   GPIO_44  - P8_12 (multi)
--   GPIO_45  - P8_11 (multi)
--   GPIO_46  - P8_16 (multi)
--   GPIO_47  - P8_15 (multi)
--   GPIO_48  - P9_15 (multi)
--   GPIO_49  - P9_23 (multi)
--   GPIO_50  - P9_14 (multi)
--   GPIO_51  - P9_16 (multi)
--   GPIO_60  - P9_12 (multi)
--   GPIO_61  - P8_26 (multi)
--   GPIO_65  - P8_18 (multi)
--   GPIO_66  - P8_07 (multi)
--   GPIO_67  - P8_08 (multi)
--   GPIO_68  - P8_10 (multi)
--   GPIO_69  - P8_09 (multi)
--   GPIO_112 - P9_30 (multi)
--   GPIO_114 - (apparently no associated pin)
--   GPIO_115 - P9_27 (multi)
--   GPIO_116 - (apparently no associated pin)

   type GPIO_record is tagged limited private;
   type GPIO is access GPIO_record;
   type direction is (input, output);
   --
   -- Create a new GPIO object
   --
   function gpio_new return GPIO;
   --
   -- Configure a new GPIO object.  The pin control file and GPIO directory
   -- must correspond, otherwise things will not work correctly.  Pin should
   -- be one of the pin constants and port should be one of
   -- the gpio constants from BBS.BBB.pins package.
   --
   procedure configure(self : not null access GPIO_record'class;
                       pin : string; port : string; dir : direction);
   procedure set(self : not null access GPIO_record'class; value : bit);
   function get(self : not null access GPIO_record'class) return bit;
   procedure close(self : not null access GPIO_record'class);

private
   package Char_IO is new Ada.Direct_IO(Character);
   type GPIO_record is tagged limited
      record
         gpio_file : Char_IO.File_Type;
         dir : direction;
      end record;
end;
