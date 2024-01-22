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
   --  are defined in the root package (BBS)).
   --
   type addr7 is mod 2**7
     with size => 7;
   type int12 is range -(2**11) .. 2**11 - 1
     with size => 12;
   type uint12 is mod 2**12
     with size => 12;
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
