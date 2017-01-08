with Ada.Text_IO;
with Interfaces.C;
use type Interfaces.C.unsigned_long;

package BBS.embed.SPI is

   --
   -- The SPI  object
   --
   type SPI_record is abstract tagged limited null record;
   type SPI_ptr is access all SPI_record'Class;
   --
--   function SPI_new return SPI_record'Class is abstract;
   --
   -- Configure the SPI interface on a BeagleBone Black or other systems that
   -- have multiple functions on the SPI pins.  This configureation procedure
   -- sets the pins to the SPI function.
   --
   procedure configure(self : in out SPI_record; SPI_file : string;
                       SCL : string; SDA : string) is abstract;
   --
   -- Configure the SPI interface on a Raspberry PI or other systems that have
   -- dedicated pins for the SPI interface.  This would also work on a system
   -- with shared pins if the pins had already been set to the SPI function.
   --
   procedure configure(self : in out SPI_record; SPI_file : string) is abstract;
   --
   -- Write a value to the SPI
   --
   procedure set(self : SPI_record; value : uint8) is abstract;
   --
   -- Read a value from the SPI
   --
   function get(self : SPI_record) return uint8 is abstract;

end;
