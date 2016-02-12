with Ada.Unchecked_Conversion;
package BBS.BBB is
--
-- This package contains useful items for programming the BeagleBone Black in
-- Ada.  The intention is to include I2C, SPI, and GPIO interfaces.
--
   author : constant String := "Brent Seidel";
   version : constant String := "V01.00";

   --
   -- Define some types for use here.
   --
   type addr7 is range 0 .. 127
     with size => 7;
   type int8 is range -128 .. 127
     with size => 8;
   type uint8 is range 0 .. 255
     with size => 8;
   type int16 is range -32768 .. 32767
     with size => 16;
   type uint16 is range 0 .. 65535
     with Size => 16;
   --
   -- A couple of unchecked conversions to convert unsigned into signed values.
   --
   function uint8_to_int8 is
     new Ada.Unchecked_Conversion(source => uint8, target => int8);
   function uint16_to_int16 is
     new Ada.Unchecked_Conversion(source => uint16, target => int16);
   --
   -- It's possible that some of this could easily be ported to the Raspberry
   -- PI.
   --
end;
