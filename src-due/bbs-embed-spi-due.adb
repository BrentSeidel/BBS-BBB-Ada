with BBS.embed.due.dev;
with SAM3x8e;
use type SAM3x8e.Bit;
with SAM3x8e.PMC;  --  Needed to enable SPI clocking
with SAM3x8e.PIO;  --  Needed to configure SPI pins

package body BBS.embed.SPI.Due is

   --
   --  Configure the SPI interface.  The SAM3X8E core actually has two SPI
   --  interfaces, but SPI1 does not seem to be present on the chip pinout.
   --
   procedure configure(self : in out Due_SPI_record) is
   begin
      --
      --  Enable clock for SPI-0
      SAM3x8e.PMC.PMC_Periph.PMC_PCER0.PID.Arr(BBS.embed.due.dev.SPI0_ID) := 1;
   end;
   --
   -- Write a value to the SPI
   --
   overriding
   procedure set(self : Due_SPI_record; value : uint8) is
   begin
      null;
   end;
   --
   -- Read a value from the SPI
   --
   overriding
   function get(self : Due_SPI_record) return uint8 is
   begin
      null;
   end;

end BBS.embed.SPI.Due;
