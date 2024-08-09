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
