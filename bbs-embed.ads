with Ada.Unchecked_Conversion;
package BBS.embed is
--
-- This package contains useful items for programming the BeagleBone Black in
-- Ada.  The intention is to include I2C, SPI, and GPIO interfaces.
--
   author : constant String := "Brent Seidel";
   version : constant String := "V01.00";

   --
   -- Define some types for use here.
   --
   type bit is range 0 .. 1
     with Size => 1;
   type addr7 is mod 2**7
     with size => 7;
   type int8 is range -128 .. 127
     with size => 8;
   type uint8 is mod 2**8
     with size => 8;
   type int12 is range -(2**11) .. 2**11 - 1
     with size => 12;
   type uint12 is mod 2**12
     with size => 12;
   type int16 is range -(2**15) .. 2**15 - 1
     with size => 16;
   type uint16 is mod 2**16
     with Size => 16;
   type int32 is range -(2**31) .. 2**31 - 1
     with Size => 32;
   type uint32 is mod 2**32
     with Size => 32;
   type int64 is range -(2**63) .. 2**63 - 1
     with Size => 64;
   type uint64 is mod 2**64
     with Size => 64;
   --
   -- A couple of unchecked conversions to convert unsigned into signed values.
   --
   function uint8_to_int8 is
     new Ada.Unchecked_Conversion(source => uint8, target => int8);
   function uint12_to_int12 is
     new Ada.Unchecked_Conversion(source => uint12, target => int12);
   function uint16_to_int16 is
     new Ada.Unchecked_Conversion(source => uint16, target => int16);
   --
   -- Get the high and low bytes (uint8) of a 16 bit uint
   --
   function highByte(x : uint16) return uint8 is
     (uint8(x / 2**8));
   function lowByte(x : uint16) return uint8 is
      (uint8(x and 16#FF#));
end;
