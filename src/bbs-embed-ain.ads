package BBS.embed.AIN is
--
   type AIN_record is abstract tagged limited null record;
   type AIN is access AIN_record'Class;
   --
   -- Read the value of an input AIN.  The ADC has 12 bits of resolution, so the
   -- returned value would be in the range 0-4095.
   --
   function get(self : AIN_record) return uint12 is abstract;

end;
